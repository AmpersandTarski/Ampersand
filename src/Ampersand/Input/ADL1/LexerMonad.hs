-- | Module      :  LexerMonad
--    License     :  GPL
--
--    Maintainer  :  helium@cs.uu.nl
--    Stability   :  experimental
--    Portability :  portable
module Ampersand.Input.ADL1.LexerMonad
  ( LexerMonad,
    addPos,
    lexerError,
    lexerWarning,
    runLexerMonad,
  )
where

import Ampersand.Basics
import Ampersand.Input.ADL1.FilePos
import Ampersand.Input.ADL1.LexerMessage

type Bracket = (FilePos, Char)

-- | The type of the Lexer Monad
--   Output monad: [LexerWarning]
--   State monad: FilePos and [Bracket]
newtype LexerMonad a
  = LM
      ( FilePos -> -- The position in the file
        [Bracket] -> -- List of brackets
        Either LexerError (a, [LexerWarning], FilePos, [Bracket]) -- The result is either an error or the result with a list of warnings, a file position and a list of brackets
      )

unLM ::
  LexerMonad a ->
  FilePos ->
  [Bracket] ->
  Either LexerError (a, [LexerWarning], FilePos, [Bracket])
unLM (LM x) = x

bindLM :: LexerMonad a -> (a -> LexerMonad b) -> LexerMonad b
bindLM (LM f) g =
  LM
    ( \pos brackets ->
        case f pos brackets of
          Left err -> Left err
          Right (a, warnings, pos2, brackets2) ->
            case unLM (g a) pos2 brackets2 of
              Left err -> Left err
              Right (b, moreWarnings, pos3, brackets3) ->
                Right (b, warnings ++ moreWarnings, pos3, brackets3)
    )

returnLM :: a -> LexerMonad a
returnLM x = LM (\pos brackets -> Right (x, [], pos, brackets))

instance Monad LexerMonad where
  (>>=) = bindLM
  return = returnLM

instance Functor LexerMonad where
  -- fmap :: (a -> b) -> LexerMonad a -> LexerMonad b
  fmap ab la = do ab <$> la

instance Applicative LexerMonad where
  pure = returnLM
  m1 <*> m2 = do
    x1 <- m1
    x1 <$> m2

-- | Runs the lexer monad
runLexerMonad ::
  -- | The file to be read (used for error messages)
  FilePath ->
  -- | The lexer monad to run
  LexerMonad a ->
  -- | Result is either an error or a result and a list of warnings
  Either LexerError (a, [LexerWarning])
runLexerMonad file (LM f) =
  case f (initPos file) [] of
    Left err -> Left err
    Right (a, warnings, _, _) -> Right (a, keepOneTabWarning warnings)

-- | Generates a monad with an error message
lexerError ::
  -- | The generated error
  LexerErrorInfo ->
  -- | The location where the error is originated
  FilePos ->
  -- | The resulting monad
  LexerMonad a
lexerError err pos =
  LM (\_ _ -> Left (LexerError pos err))

-- | Generates a monad with a warning message
lexerWarning ::
  -- | The generated warning
  LexerWarningInfo ->
  -- | The location where the warning is originated
  FilePos ->
  -- | The resulting monad
  LexerMonad ()
lexerWarning warning warningPos =
  LM
    ( \pos brackets ->
        Right ((), [LexerWarning warningPos warning], pos, brackets)
    )
