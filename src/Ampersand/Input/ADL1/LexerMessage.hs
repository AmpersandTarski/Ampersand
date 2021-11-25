{- Based on module LexerMessage from Helium (GPL license) -}

module Ampersand.Input.ADL1.LexerMessage
  ( LexerError (..),
    LexerErrorInfo (..),
    LexerWarning (..),
    LexerWarningInfo (..),
    keepOneTabWarning,
    showLexerErrorInfo,
    showLexerWarningInfo,
  )
where

import Ampersand.Basics
import Ampersand.Input.ADL1.FilePos (FilePos)
import qualified Ampersand.Input.ADL1.LexerTexts as Texts

-- | Defines a lexer error
data LexerError
  = -- | The lexer file position and error information
    LexerError FilePos LexerErrorInfo

-- | Defines the different lexer error types
data LexerErrorInfo
  = -- | The comment was not terminated
    UnterminatedComment
  | -- | The markup section was not terminated
    UnterminatedMarkup
  | -- | The atom was not terminated
    UnterminatedAtom
  | -- | The character was unexpected
    UnexpectedChar Char
  | -- | The string was not terminated
    NonTerminatedString String
  | -- | In UnexpectedClose, first char is the closing bracket we see,
    --   second char is the closing bracket we would like to see first
    --   e.g. [(1,3]  =>  UnexpectedClose ']' ... ')'
    TooManyClose Char
  | -- | An unexpected closing bracket was found
    UnexpectedClose Char FilePos Char
  | -- | Brackets were not closed at the end of the file
    StillOpenAtEOF [(FilePos, Char)]
  | -- | DateTime not conforming to ISO 8601 format
    ProblematicISO8601DateTime
  deriving (Show)

-- | Converts the error information into a list of error messages
showLexerErrorInfo ::
  -- | The error information
  LexerErrorInfo ->
  -- | The error messages
  [String]
showLexerErrorInfo info =
  case info of
    UnterminatedComment -> [Texts.lexerUnterminatedComment]
    UnterminatedMarkup -> [Texts.lexerUnterminatedMarkup]
    UnterminatedAtom -> [Texts.lexerUnterminatedAtom]
    UnexpectedChar c -> [Texts.lexerUnexpectedChar c]
    NonTerminatedString _ -> Texts.lexerNonTerminatedString : Texts.lexerCorrectStrings
    TooManyClose c -> [Texts.lexerTooManyClose c]
    UnexpectedClose c1 _ c2 -> Texts.lexerUnexpectedClose c1 c2
    StillOpenAtEOF [b] -> [Texts.lexerStillOpenAtEOF [show (snd b)]]
    StillOpenAtEOF bs ->
      [ Texts.lexerStillOpenAtEOF
          . reverse -- 'reverse' because positions will be sorted and brackets are
          -- reported in reversed order
          . map (show . snd)
          $ bs
      ]
    ProblematicISO8601DateTime -> [Texts.lexerProblematicISO8601DateTime]

-- | Defines a lexer warning
data LexerWarning
  = -- | The lexer file position and warning information
    LexerWarning FilePos LexerWarningInfo

-- | Defines the different lexer warning types
data LexerWarningInfo
  = -- | Tab character was encountered
    TabCharacter
  | -- | Nested comment was encountered
    NestedComment FilePos
  | -- | The UTF BOM character was found
    UtfChar
  | -- | Syntax coloring cannot handle names containing --
    CommentOperator

-- | Converts the warning information into a list of warning messages
showLexerWarningInfo ::
  -- | The warning information
  LexerWarningInfo ->
  -- | The warning messages
  [String]
showLexerWarningInfo info =
  case info of
    TabCharacter -> Texts.lexerTabCharacter
    NestedComment _ -> Texts.lexerNestedComment
    UtfChar -> Texts.lexerUtfChar
    CommentOperator -> Texts.lexerCommentOperator

-- | Generates a TabCharacter warning
keepOneTabWarning ::
  -- | The old warnings
  [LexerWarning] ->
  -- | The new warnings
  [LexerWarning]
keepOneTabWarning = keepOneTab True
  where
    keepOneTab isFirst (warning@(LexerWarning _ TabCharacter) : rest)
      | isFirst = warning : keepOneTab False rest
      | otherwise = keepOneTab isFirst rest
    keepOneTab isFirst (warning : rest) =
      warning : keepOneTab isFirst rest
    keepOneTab _ [] = []
