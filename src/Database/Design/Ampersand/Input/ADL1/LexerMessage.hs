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
    ) where

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

data LexerError = LexerError FilePos LexerErrorInfo
    deriving(Show)

data LexerErrorInfo
    = UnterminatedComment
	| UnterminatedPurpose
    | UnterminatedAtom
    | UnterminatedInfix
    | MissingExponentDigits
    | UnexpectedChar Char
    | UnexpectedInfixKeyword String
    | UnexpectedInfixChar Char	
    | IllegalEscapeInChar
    | EmptyChar
    | IllegalCharInChar
    | NonTerminatedChar (Maybe String)
    | EOFInChar

    | EOFInString
    | IllegalEscapeInString
    | NewLineInString
    | IllegalCharInString

    | TooManyClose Char
        -- In UnexpectedClose, first char is the closing bracket we see, 
        -- second char is the closing bracket we would like to see first
        -- e.g. [(1,3]  =>  UnexpectedClose ']' ... ')'
    | UnexpectedClose Char FilePos Char
    | StillOpenAtEOF [(FilePos, Char)]
    deriving(Show)

showLexerErrorInfo :: LexerErrorInfo -> [String]
showLexerErrorInfo info =
    case info of
        UnterminatedComment          -> [ Texts.lexerUnterminatedComment               ]
        UnterminatedPurpose          -> [ Texts.lexerUnterminatedPurpose               ]
        UnterminatedAtom             -> [ Texts.lexerUnterminatedAtom                  ]
        UnterminatedInfix            -> [ Texts.lexerUnterminatedInfix                 ]
        MissingExponentDigits        -> [ Texts.lexerMissingExponentDigits 
                                        , correctFloats 
                                        ]
        UnexpectedChar c             -> [ Texts.lexerUnexpectedChar c                  ]
        UnexpectedInfixKeyword s     -> [ Texts.lexerUnexpectedInfixKeyword s          ]   
        UnexpectedInfixChar c        -> [ Texts.lexerUnexpectedInfixChar c             ]   		
        IllegalEscapeInChar          -> [ Texts.lexerIllegalEscapeInChar, correctChars ]
        EmptyChar                    -> [ Texts.lexerEmptyChar, correctChars           ]
        IllegalCharInChar            -> [ Texts.lexerIllegalCharInChar, correctChars   ]
        NonTerminatedChar mn         -> [ Texts.lexerNonTerminatedChar
                                        , correctChars
                                        ] ++ case mn of
                                               Nothing -> []
                                               Just name -> [ Texts.lexerInfixHint name ]
        EOFInChar                      -> [ Texts.lexerEOFInChar, correctChars]
        
        EOFInString                    -> [ Texts.lexerEOFInString,              correctStrings ]
        IllegalEscapeInString          -> [ Texts.lexerIllegalEscapeInString,    correctStrings ]
        NewLineInString                -> [ Texts.lexerNewLineInString,          correctStrings ]
        IllegalCharInString            -> [ Texts.lexerIllegalCharInString,      correctStrings]
                
        TooManyClose c                 -> [ Texts.lexerTooManyClose c ]
        UnexpectedClose c1 _ c2        ->   Texts.lexerUnexpectedClose c1 c2
        StillOpenAtEOF [b]             -> [ Texts.lexerStillOpenAtEOF [ show (snd b) ] ]
        StillOpenAtEOF bs              -> [ Texts.lexerStillOpenAtEOF (reverse (map (show.snd) bs)) ]
            -- 'reverse' because positions will be sorted and brackets are
            -- reported in reversed order

correctFloats, correctChars, correctStrings :: String
correctFloats  = Texts.lexerCorrectFloats
correctChars   = Texts.lexerCorrectChars 
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

data LexerWarning =
    LexerWarning FilePos LexerWarningInfo

data LexerWarningInfo 
    = TabCharacter
    | LooksLikeFloatNoFraction String
    | LooksLikeFloatNoDigits String
    | NestedComment FilePos
    | CommentOperator

showLexerWarningInfo :: LexerWarningInfo -> [String]
showLexerWarningInfo info = 
    case info of
        TabCharacter                    -> Texts.lexerTabCharacter
        LooksLikeFloatNoFraction digits -> Texts.lexerLooksLikeFloatNoFraction digits
        LooksLikeFloatNoDigits fraction -> Texts.lexerLooksLikeFloatNoDigits fraction
        NestedComment _                 -> Texts.lexerNestedComment
        CommentOperator                 -> Texts.lexerCommentOperator

-- TODO: This is only valid for haskell.. Probably more of the warnings too!
keepOneTabWarning :: [LexerWarning] -> [LexerWarning]
keepOneTabWarning = keepOneTab True
  where
    keepOneTab isFirst (warning@(LexerWarning _ TabCharacter):rest)
        | isFirst   = warning : keepOneTab False rest
        | otherwise = keepOneTab isFirst rest
    keepOneTab isFirst (warning:rest) = 
        warning : keepOneTab isFirst rest
    keepOneTab _ [] = []

isLooksLikeFloatWarningInfo :: LexerWarningInfo -> Bool
isLooksLikeFloatWarningInfo warningInfo =
   case warningInfo of
      LooksLikeFloatNoFraction _ -> True
      LooksLikeFloatNoDigits _   -> True
      _                          -> False