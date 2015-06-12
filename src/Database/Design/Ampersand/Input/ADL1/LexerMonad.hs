{-| Module      :  LexerMonad
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Database.Design.Ampersand.Input.ADL1.LexerMonad
    ( LexerMonad
    , getPos, incPos, nextPos, addPos
    , openBracket, closeBracket, checkBracketsAtEOF
    , lexerError, lexerWarning
    , runLexerMonad
    , getOpts
    ) where

import Database.Design.Ampersand.Input.ADL1.LexerMessage
import Database.Design.Ampersand.Input.ADL1.FilePos
import Database.Design.Ampersand.Misc

import Control.Applicative
import Control.Monad

type Bracket = (FilePos, Char)

-- Output monad: [LexerWarning]
-- State monad: FilePos and [Bracket]
newtype LexerMonad a =
    LM ([Options] -> FilePos -> [Bracket] ->
        Either LexerError (a, [LexerWarning], FilePos, [Bracket]))

unLM :: LexerMonad t -> [Options] -> FilePos -> [Bracket]
          -> Either LexerError (t, [LexerWarning], FilePos, [Bracket])
unLM (LM x) = x

bindLM :: LexerMonad a -> (a -> LexerMonad b) -> LexerMonad b
bindLM (LM f) g =
    LM (\opts pos brackets ->
        case f opts pos brackets of
            Left err -> Left err
            Right (a, warnings, pos2, brackets2) ->
                case unLM (g a) opts pos2 brackets2 of
                    Left err -> Left err
                    Right (b, moreWarnings, pos3, brackets3) ->
                        Right (b, warnings ++ moreWarnings, pos3, brackets3))

returnLM :: a -> LexerMonad a
returnLM x = LM (\_ pos brackets -> Right (x, [], pos, brackets))

instance Monad LexerMonad where
    (>>=) = bindLM
    return = returnLM

instance Functor LexerMonad where
    -- fmap :: (a -> b) -> LexerMonad a -> LexerMonad b
    fmap ab la = do { a <- la; return (ab a) }

instance Applicative LexerMonad where
    pure = returnLM
    (<*>) = ap

runLexerMonad :: [Options] -> FilePath -> LexerMonad a -> Either LexerError (a, [LexerWarning])
runLexerMonad opts file (LM f) =
    case f opts (initPos file) [] of
        Left err -> Left err
        Right (a, warnings, _, _) -> Right (a, keepOneTabWarning warnings)

-- TODO: These methods are not being used anywhere
getOpts :: LexerMonad [Options]
getOpts = LM (\opts pos brackets -> Right (opts, [], pos, brackets))

getPos :: LexerMonad FilePos
getPos = LM (\_ pos brackets -> Right (pos, [], pos, brackets))

incPos :: Int -> LexerMonad ()
incPos i = LM (\_ pos brackets -> Right ((), [], addPos i pos, brackets))

nextPos :: Char -> LexerMonad ()
nextPos c = LM (\_ pos brackets -> Right ( (), [], updatePos pos c, brackets ))

lexerError :: LexerErrorInfo -> FilePos -> LexerMonad a
lexerError err pos =
    LM (\_ _ _ -> Left (LexerError pos err))

lexerWarning :: LexerWarningInfo -> FilePos -> LexerMonad ()
lexerWarning warning warningPos =
    LM (\_ pos brackets ->
        Right ((), [LexerWarning warningPos warning], pos, brackets))

openBracket :: Char -> LexerMonad ()
openBracket c = LM (\_ pos brackets ->
    Right ( (), [], pos, (pos, c) : brackets ))

closeBracket :: Char -> LexerMonad ()
closeBracket c = LM (\_ pos brackets ->
    case brackets of
        [] -> Left (LexerError pos (TooManyClose c))
        (pos2, c2):rest
            | matchBracket c2 c ->
                Right ((), [], pos, rest)
            | otherwise ->
                Left (LexerError pos (UnexpectedClose c pos2 c2))
    )

checkBracketsAtEOF :: LexerMonad ()
checkBracketsAtEOF = LM (\_ pos brackets ->
    case brackets of
        [] -> Right ((), [], pos, [])
        _  -> Left (LexerError pos (StillOpenAtEOF brackets))
    )

matchBracket :: Char -> Char -> Bool
matchBracket '(' ')' = True
matchBracket '[' ']' = True
matchBracket '{' '}' = True
matchBracket _ _ = False
