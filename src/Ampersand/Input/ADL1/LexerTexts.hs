module Ampersand.Input.ADL1.LexerTexts
    ( lexerCommentOperator
    , lexerCorrectStrings
    , lexerNestedComment
    , lexerNonTerminatedString
    , lexerStillOpenAtEOF
    , lexerTabCharacter
    , lexerTooManyClose
    , lexerUnexpectedChar
    , lexerUnexpectedClose
    , lexerUnterminatedAtom
    , lexerUnterminatedComment
    , lexerUnterminatedPurpose
    , lexerProblematicISO8601DateTime
    , lexerUtfChar
    ) where

import Data.IORef
import System.IO.Unsafe
import Data.Maybe (fromMaybe)

data Language = English | Dutch deriving Eq

{-# NOINLINE language #-}
language :: IORef Language
language = unsafePerformIO (newIORef English)

data Arrow a b = a :-> b

infix 0 :->

select :: IORef Language -> [Arrow Language msg] -> msg
select languageRef table =
    let lang = unsafePerformIO (readIORef languageRef)
        convert (a :-> b) = (a, b)
    in fromMaybe (error "Texts.select: unknown language")
        (lookup lang (map convert table))

-- | Translates 'Unterminated comment' into the chosen language
lexerUnterminatedComment :: String -- ^ The translated string
lexerUnterminatedComment = select language
    [ English :-> "Unterminated comment"
    , Dutch   :-> "Commentaar niet afgesloten"
    ]

-- | Translates 'Unterminated purpose' into the chosen language
lexerUnterminatedPurpose:: String -- ^ The translated string
lexerUnterminatedPurpose= select language
    [ English :-> "Unterminated PURPOSE section"
    , Dutch   :-> "PURPOSE sectie niet afgesloten"
    ]

-- | Translates 'Unterminated atom' into the chosen language
lexerUnterminatedAtom :: String -- ^ The translated string
lexerUnterminatedAtom = select language
    [ English :-> "Unterminated Atom literal"
    , Dutch   :-> "Atom literal niet afgesloten"
    ]
-- | Translates 'ProblematicISO8601DateTime' into the chosen language
lexerProblematicISO8601DateTime:: String 
lexerProblematicISO8601DateTime = select language
    [ English :-> "Incorrect DATETIME value (ISO 8601 standard)"
    , Dutch   :-> "foutieve DATETIME waarde (Zie ISO 8601)"
    ]
-- | Translates 'Unexpected character' into the chosen language
lexerUnexpectedChar :: Char -> String -- ^ The translated string
lexerUnexpectedChar c = select language
    [ English :-> "Unexpected character '" ++ [c] ++ "'"
    , Dutch   :-> "Onverwachte letter '" ++ [c] ++ "'"
    ]

-- | Translates 'Unterminated string' into the chosen language
lexerNonTerminatedString :: String -- ^ The translated string
lexerNonTerminatedString = select language
    [ English :-> "Unterminated string literal"
    , Dutch   :-> "String niet afgesloten"
    ]

-- | Translates 'Close bracket but no open bracket' into the chosen language
lexerTooManyClose :: Show a => a -> String -- ^ The translated string
lexerTooManyClose c = select language
    [ English :-> "Close bracket " ++ show c ++ " but no open bracket"
    , Dutch   :-> "Haakje " ++ show c ++ " wordt gesloten maar nergens geopend"
    ]

-- | Translates 'Unexpected close bracket' into the chosen language
lexerUnexpectedClose :: Char -> Char -> [String]
lexerUnexpectedClose c1 c2 = select language
    [ English :-> [ "Unexpected close bracket " ++ show c1
                  , "Expecting a close bracket for " ++ show c2
                  ]
    , Dutch   :-> [ "Onverwacht sluithaakje " ++ show c1
                  , "Sluithaakje voor " ++ show c2 ++ " wordt nog verwacht"
                  ]
    ]

-- | Translates 'Bracket is never closed' into the chosen language
lexerStillOpenAtEOF :: [String] -- ^ The brackets
                    -> String -- ^ The translated string
lexerStillOpenAtEOF [s] = select language
    [ English :-> "Bracket " ++ s ++ " is never closed"
    , Dutch   :-> "Sluithaakje voor " ++ s ++ " wordt nog verwacht"
    ]

-- | Translates 'Brackets are never closed' into the chosen language
lexerStillOpenAtEOF xs = select language
    [ English :-> "The following brackets are never closed: " ++ commasAnd xs
    , Dutch   :-> "De volgende haakjes worden nergens gesloten: " ++ kommasEn xs
    ]

-- | Gives string examples in the chosen language
lexerCorrectStrings :: String -- ^ The translated string
lexerCorrectStrings = select language
    [ English :-> "Correct examples of Strings: \"Ampersand (&) is cool\" \"Helium is cool too!\" \"abc\\ndef\" \"\""
    , Dutch   :-> "Correcte voorbeelden van teksten: \"Ampersand (&) is geweldig\" \"Helium is ook geweldig!\" \"abc\\ndef\" \"\""
    ]

-- | Translates 'Tab character encountered' into the chosen language
lexerTabCharacter :: [String] -- ^ The translated strings
lexerTabCharacter = select language
    [ English :-> [ "Tab character encountered; may cause problems with the layout rule"
                  , "Configure your editor to replace tabs by spaces"
                  ]
    , Dutch   :-> [ "Tab karakters kunnen problemn opleveren met de layout rule"
                  , "Stel je editor zo in dat hij tabs vervangt door spaties"
                  ]
    ]

-- | Translates 'Unrecognized character' into the chosen language
lexerUtfChar :: [String] -- ^ The translated strings
lexerUtfChar = select language
    [ English :-> [ "Unrecognized character in the beginning of the file."
                  , "Is it saved with encoding UTF-8 without BOM?"
                  ]
    , Dutch   :-> [ "Onbekend karakter in het begin van het bestand."
                  , "Is het bestand opgeslagen met encoding UTF-8 zonder BOM?"
                  ]
    ]

-- | Translates 'Nested comment' into the chosen language
lexerNestedComment :: [String] -- ^ The translated strings
lexerNestedComment = select language
    [ English :-> [ "Syntax coloring usually can not handle nested comments"
                  , "Some of your code may be in comments but not visibly so"
                  ]
    , Dutch   :-> [ "Syntax kleuring van editor kan meestal niet overweg met genest commentaar"
                  , "Het kan zo zijn dat een deel van je code in commentaar staat maar dat je dat niet ziet"
                  ]
    ]

-- | Translates 'Comment operator' into the chosen language
lexerCommentOperator :: [String] -- ^ The translated strings
lexerCommentOperator = select language
    [ English :-> [ "Syntax coloring usually cannot handle names containing --"
                  , "If you wanted to start a comment, write spaces around --"
                  ]
    , Dutch   :-> [ "Syntax kleuring van editor kan meestal niet overweg met namen die -- bevatten"
                  , "Als je commentaar wilde beginnen schrijf dan spaties voor en na --"
                  ]
    ]

-- | Adds commas and 'and' in between each element of the list
commasAndLang :: String   -- ^ The word to use for 'and'
              -> [String] -- ^ The list of items to show
              -> String   -- ^ The result
commasAndLang _ [] = []
commasAndLang _ [x] = x
commasAndLang a (x:xs) = x ++ concatMap (", " ++) (init xs) ++ " " ++ a ++ " " ++ last xs

commasAnd :: [String] -> String
commasAnd = commasAndLang "and"

kommasEn :: [String] -> String
kommasEn = commasAndLang "en"
