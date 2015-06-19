{-| Module      :  LexerMessage
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Database.Design.Ampersand.Input.ADL1.LexerMessage
    ( LexerError(..)
    , LexerErrorInfo(..)
    , LexerWarning(..)
    , LexerWarningInfo(..)
    , keepOneTabWarning
    , isLooksLikeFloatWarningInfo
    , showLexerErrorInfo
    , showLexerWarningInfo
    , showLexerWarnings
    ) where

import Data.List (intercalate)
import Database.Design.Ampersand.Input.ADL1.FilePos(FilePos)
import qualified Database.Design.Ampersand.Input.ADL1.LexerTexts as Texts

--TODO: Delete the code commented out
{-

instance HasMessage LexerError where
    getRanges (LexerError _ (StillOpenAtEOF brackets)) =
        reverse (map (sourcePosToRange . fst) brackets)
    getRanges (LexerError pos (UnexpectedClose _ pos2 _)) =
        map sourcePosToRange [pos, pos2]
    getRanges (LexerError pos _) =
        [ sourcePosToRange pos ]
    getMessage (LexerError _ info) = 
        let (line:rest) = showLexerErrorInfo info
        in MessageOneLiner (MessageString line) :
            [ MessageHints Texts.hint [ MessageString s | s <- rest ] ]

sourcePosToRange :: FilePos -> Range
sourcePosToRange pos = 
    let name = sourceName pos; line = sourceLine pos; col = sourceColumn pos
        position = Position_Position name line col
    in Range_Range position position
 -}

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

correctStrings :: String
correctStrings = Texts.lexerCorrectStrings

{-
instance HasMessage LexerWarning where
    getRanges (LexerWarning pos (NestedComment pos2)) =
       map sourcePosToRange [ pos, pos2 ]
    getRanges (LexerWarning pos _) =
        [ sourcePosToRange pos ]
    getMessage (LexerWarning _ info) = 
        let (line:rest) = showLexerWarningInfo info
        in MessageOneLiner (MessageString (Texts.warning ++ ": " ++ line)) :
            [ MessageHints Texts.hint [ MessageString s | s <- rest ] ]
-}

-- | Defines a lexer warning
data LexerWarning =
    LexerWarning FilePos LexerWarningInfo -- ^ The lexer file position and warning information

-- | Defines the different lexer warning types
data LexerWarningInfo
    = TabCharacter -- ^ Tab character was encountered
    | LooksLikeFloatNoFraction String -- ^ The number looks like a float, but doesn't have fraction
    | LooksLikeFloatNoDigits String   -- ^ The number looks like a float, but doesn't have digits
    | NestedComment FilePos  -- ^ Nested comment was encountered
    | UtfChar -- ^ The UTF BOM character was found
    | CommentOperator -- ^ Syntax coloring cannot handle names containing --

-- | Converts a list of warnings to a single string
showLexerWarnings :: [LexerWarning] -- ^ The warnings
                  -> String         -- ^ The string for the user
showLexerWarnings ws = intercalate "\n-----------\n" $ map showWarning ws
            where showWarning (LexerWarning pos info) = "Warning: " ++
                    intercalate "\n" (showLexerWarningInfo info) ++ " " ++ show pos

-- | Converts the warning information into a list of warning messages
showLexerWarningInfo :: LexerWarningInfo  -- ^ The warning information
                     -> [String] -- ^ The warning messages
showLexerWarningInfo info = 
    case info of
        TabCharacter                    -> Texts.lexerTabCharacter
        LooksLikeFloatNoFraction digits -> Texts.lexerLooksLikeFloatNoFraction digits
        LooksLikeFloatNoDigits fraction -> Texts.lexerLooksLikeFloatNoDigits fraction
        NestedComment _                 -> Texts.lexerNestedComment
        UtfChar                         -> Texts.lexerUtfChar
        CommentOperator                 -> Texts.lexerCommentOperator

-- TODO: This is only valid for haskell.. Probably more of the warnings too!
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

-- | Checks whether the given warning is a LooksLikeFloatNoFraction or LooksLikeFloatNoDigits
isLooksLikeFloatWarningInfo :: LexerWarningInfo -- ^ The warning
                            -> Bool             -- ^ The result
isLooksLikeFloatWarningInfo warningInfo =
   case warningInfo of
      LooksLikeFloatNoFraction _ -> True
      LooksLikeFloatNoDigits _   -> True
      _                          -> False