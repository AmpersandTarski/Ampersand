{- Based on module LexerMessage from Helium (GPL license) -}

module Ampersand.Input.ADL1.LexerMessage
    ( LexerError(..)
    , LexerErrorInfo(..)
    , LexerWarning(..)
    , LexerWarningInfo(..)
    , keepOneTabWarning
    , showLexerErrorInfo
    , showLexerWarningInfo
    ) where

import Ampersand.Input.ADL1.FilePos(FilePos)
import qualified Ampersand.Input.ADL1.LexerTexts as Texts

-- | Defines a lexer error
data LexerError = LexerError FilePos LexerErrorInfo -- ^ The lexer file position and error information
    deriving(Show)

-- | Defines the different lexer error types
data LexerErrorInfo
    -- | The comment was not terminated
    = UnterminatedComment
    -- | The purpose statement was not terminated
    | UnterminatedPurpose
    -- | The atom was not terminated
    | UnterminatedAtom
    -- | The character was unexpected
    | UnexpectedChar Char
    -- | The string was not terminated
    | NonTerminatedString String
    -- | In UnexpectedClose, first char is the closing bracket we see, 
    --   second char is the closing bracket we would like to see first
    --   e.g. [(1,3]  =>  UnexpectedClose ']' ... ')'
    | TooManyClose Char
    -- | An unexpected closing bracket was found
    | UnexpectedClose Char FilePos Char
    -- | Brackets were not closed at the end of the file
    | StillOpenAtEOF [(FilePos, Char)]
    -- | DateTime not conforming to ISO 8601 format
    | ProblematicISO8601DateTime
    deriving(Show)

-- | Converts the error information into a list of error messages
showLexerErrorInfo :: LexerErrorInfo -- ^ The error information
                   -> [String] -- ^ The error messages
showLexerErrorInfo info =
    case info of
        UnterminatedComment          -> [ Texts.lexerUnterminatedComment               ]
        UnterminatedPurpose          -> [ Texts.lexerUnterminatedPurpose               ]
        UnterminatedAtom             -> [ Texts.lexerUnterminatedAtom                  ]
        UnexpectedChar c             -> [ Texts.lexerUnexpectedChar c                  ]
        NonTerminatedString _        -> [ Texts.lexerNonTerminatedString, correctStrings ]
        TooManyClose c                 -> [ Texts.lexerTooManyClose c ]
        UnexpectedClose c1 _ c2        ->   Texts.lexerUnexpectedClose c1 c2
        StillOpenAtEOF [b]             -> [ Texts.lexerStillOpenAtEOF [ show (snd b) ] ]
        StillOpenAtEOF bs              -> [ Texts.lexerStillOpenAtEOF (reverse (map (show.snd) bs)) ]
            -- 'reverse' because positions will be sorted and brackets are
            -- reported in reversed order
        ProblematicISO8601DateTime   -> [ Texts.lexerProblematicISO8601DateTime        ]
correctStrings :: String
correctStrings = Texts.lexerCorrectStrings

-- | Defines a lexer warning
data LexerWarning =
    LexerWarning FilePos LexerWarningInfo -- ^ The lexer file position and warning information

-- | Defines the different lexer warning types
data LexerWarningInfo
    = TabCharacter -- ^ Tab character was encountered
    | NestedComment FilePos  -- ^ Nested comment was encountered
    | UtfChar -- ^ The UTF BOM character was found
    | CommentOperator -- ^ Syntax coloring cannot handle names containing --

-- | Converts the warning information into a list of warning messages
showLexerWarningInfo :: LexerWarningInfo  -- ^ The warning information
                     -> [String] -- ^ The warning messages
showLexerWarningInfo info = 
    case info of
        TabCharacter                    -> Texts.lexerTabCharacter
        NestedComment _                 -> Texts.lexerNestedComment
        UtfChar                         -> Texts.lexerUtfChar
        CommentOperator                 -> Texts.lexerCommentOperator

-- | Generates a TabCharacter warning
keepOneTabWarning :: [LexerWarning] -- ^ The old warnings
                  -> [LexerWarning] -- ^ The new warnings
keepOneTabWarning = keepOneTab True
  where
    keepOneTab isFirst (warning@(LexerWarning _ TabCharacter):rest)
        | isFirst   = warning : keepOneTab False rest
        | otherwise = keepOneTab isFirst rest
    keepOneTab isFirst (warning:rest) = 
        warning : keepOneTab isFirst rest
    keepOneTab _ [] = []
