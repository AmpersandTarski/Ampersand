{-| Module      :  LexerMonad
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Database.Design.Ampersand.Input.ADL1.LexerMonad
    ( LexerMonad
    , addPos
    , lexerError, lexerWarning
    , runLexerMonad
    ) where

import Database.Design.Ampersand.Input.ADL1.LexerMessage
import Database.Design.Ampersand.Input.ADL1.FilePos
import Database.Design.Ampersand.Misc

import Control.Monad

type Bracket = (FilePos, Char)

-- | The type of the Lexer Monad
--   Output monad: [LexerWarning]
--   State monad: FilePos and [Bracket]
newtype LexerMonad a =
    LM ([Options]   -- The command line options
       -> FilePos   -- The position in the file
       -> [Bracket] -- List of brackets
       -> Either LexerError (a, [LexerWarning], FilePos, [Bracket]) -- The result is either an error or the result with a list of warnings, a file position and a list of brackets
       )

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

-- | Runs the lexer monad
runLexerMonad :: [Options]    -- ^ The command line options
              -> FilePath     -- ^ The file to be read (used for error messages)
              -> LexerMonad a -- ^ The lexer monad to run
              -> Either LexerError (a, [LexerWarning]) -- ^ Result is either an error or a result and a list of warnings
runLexerMonad opts file (LM f) =
    case f opts (initPos file) [] of
        Left err -> Left err
        Right (a, warnings, _, _) -> Right (a, keepOneTabWarning warnings)

-- | Generates a monad with an error message
lexerError :: LexerErrorInfo -- ^ The generated error
           -> FilePos        -- ^ The location where the error is originated
           -> LexerMonad a   -- ^ The resulting monad
lexerError err pos =
    LM (\_ _ _ -> Left (LexerError pos err))

-- | Generates a monad with a warning message
lexerWarning :: LexerWarningInfo -- ^ The generated warning
             -> FilePos          -- ^ The location where the warning is originated
             -> LexerMonad ()    -- ^ The resulting monad
lexerWarning warning warningPos =
    LM (\_ pos brackets ->
        Right ((), [LexerWarning warningPos warning], pos, brackets))
