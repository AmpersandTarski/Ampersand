module Ampersand.Input.ADL1.LexerTexts
  ( lexerCommentOperator,
    lexerCorrectStrings,
    lexerNestedComment,
    lexerNonTerminatedString,
    lexerStillOpenAtEOF,
    lexerTabCharacter,
    lexerTooManyClose,
    lexerUnexpectedChar,
    lexerUnexpectedClose,
    lexerUnterminatedAtom,
    lexerUnterminatedComment,
    lexerUnterminatedMarkup,
    lexerProblematicISO8601DateTime,
    lexerUtfChar,
  )
where

import Ampersand.Basics hiding (Arrow)
import System.IO.Unsafe (unsafePerformIO)

type Language = Lang

{-# NOINLINE language #-}
language :: IORef Language
language = unsafePerformIO (newIORef English)

data Arrow a b = a :-> b

infix 0 :->

select :: IORef Language -> [Arrow Language msg] -> msg
select languageRef table =
  let lang = unsafePerformIO (readIORef languageRef)
      convert (a :-> b) = (a, b)
   in fromMaybe
        (error "Texts.select: unknown language")
        (lookup lang (map convert table))

-- | Translates 'Unterminated comment' into the chosen language
lexerUnterminatedComment ::
  -- | The translated string
  String
lexerUnterminatedComment =
  select
    language
    [ English :-> "Unterminated comment",
      Dutch :-> "Commentaar niet afgesloten"
    ]

-- | Translates 'Unterminated markup' into the chosen language
lexerUnterminatedMarkup ::
  -- | The translated string
  String
lexerUnterminatedMarkup =
  select
    language
    [ English :-> "Unterminated markup section. (any text between `{+` and `+}` ).",
      Dutch :-> "markup sectie niet afgesloten. (tekst tussen `{+` en `+}` )."
    ]

-- | Translates 'Unterminated atom' into the chosen language
lexerUnterminatedAtom ::
  -- | The translated string
  String
lexerUnterminatedAtom =
  select
    language
    [ English :-> "Unterminated Atom literal",
      Dutch :-> "Atom literal niet afgesloten"
    ]

-- | Translates 'ProblematicISO8601DateTime' into the chosen language
lexerProblematicISO8601DateTime :: String
lexerProblematicISO8601DateTime =
  select
    language
    [ English :-> "Incorrect DATETIME value (ISO 8601 standard)",
      Dutch :-> "foutieve DATETIME waarde (Zie ISO 8601)"
    ]

-- | Translates 'Unexpected character' into the chosen language
lexerUnexpectedChar ::
  Char ->
  -- | The translated string
  String
lexerUnexpectedChar c =
  select
    language
    [ English :-> "Unexpected character '" ++ [c] ++ "'",
      Dutch :-> "Onverwachte letter '" ++ [c] ++ "'"
    ]

-- | Translates 'Unterminated string' into the chosen language
lexerNonTerminatedString ::
  -- | The translated string
  String
lexerNonTerminatedString =
  select
    language
    [ English :-> "Unterminated string literal",
      Dutch :-> "String niet afgesloten"
    ]

-- | Translates 'Close bracket but no open bracket' into the chosen language
lexerTooManyClose ::
  (Show a) =>
  a ->
  -- | The translated string
  String
lexerTooManyClose c =
  select
    language
    [ English :-> "Close bracket " ++ show c ++ " but no open bracket",
      Dutch :-> "Haakje " ++ show c ++ " wordt gesloten maar nergens geopend"
    ]

-- | Translates 'Unexpected close bracket' into the chosen language
lexerUnexpectedClose :: Char -> Char -> [String]
lexerUnexpectedClose c1 c2 =
  select
    language
    [ English
        :-> [ "Unexpected close bracket " ++ show c1,
              "Expecting a close bracket for " ++ show c2
            ],
      Dutch
        :-> [ "Onverwacht sluithaakje " ++ show c1,
              "Sluithaakje voor " ++ show c2 ++ " wordt nog verwacht"
            ]
    ]

-- | Translates 'Bracket is never closed' into the chosen language
lexerStillOpenAtEOF ::
  -- | The brackets
  [String] ->
  -- | The translated string
  String
lexerStillOpenAtEOF [s] =
  select
    language
    [ English :-> "Bracket " ++ s ++ " is never closed",
      Dutch :-> "Sluithaakje voor " ++ s ++ " wordt nog verwacht"
    ]
lexerStillOpenAtEOF xs =
  select
    language
    [ English :-> "The following brackets are never closed: " ++ commasAnd xs,
      Dutch :-> "De volgende haakjes worden nergens gesloten: " ++ kommasEn xs
    ]

-- | Gives string examples in the chosen language
lexerCorrectStrings ::
  -- | The translated string
  [String]
lexerCorrectStrings =
  select
    language
    [ English
        :-> [ "Correct examples of Strings: \"Ampersand (&) is cool\" \"Helium is cool too!\" \"abc\\ndef\" \"\"",
              "This error may be caused by a missing quote-character ('\"'), or by the inadvertent use of the escape character ('\\')."
            ],
      Dutch
        :-> [ "Correcte voorbeelden van teksten: \"Ampersand (&) is geweldig\" \"Helium is ook geweldig!\" \"abc\\ndef\" \"\"",
              "Deze fout kan worden veroorzaakt door een missende quote ('\"'), of door verkeerd gebruik van het escape character ('\\')."
            ]
    ]

-- | Translates 'Tab character encountered' into the chosen language
lexerTabCharacter ::
  -- | The translated strings
  [String]
lexerTabCharacter =
  select
    language
    [ English
        :-> [ "Tab character encountered; may cause problems with the layout rule",
              "Configure your editor to replace tabs by spaces"
            ],
      Dutch
        :-> [ "Tab karakters kunnen problemn opleveren met de layout rule",
              "Stel je editor zo in dat hij tabs vervangt door spaties"
            ]
    ]

-- | Translates 'Unrecognized character' into the chosen language
lexerUtfChar ::
  -- | The translated strings
  [String]
lexerUtfChar =
  select
    language
    [ English
        :-> [ "Unrecognized character in the beginning of the file.",
              "The expected encoding should be UTF-8 without BOM."
            ],
      Dutch
        :-> [ "Onbekend karakter in het begin van het bestand.",
              "De verwachtte encoding is UTF-8 zonder BOM."
            ]
    ]

-- | Translates 'Nested comment' into the chosen language
lexerNestedComment ::
  -- | The translated strings
  [String]
lexerNestedComment =
  select
    language
    [ English
        :-> [ "Syntax coloring usually can not handle nested comments",
              "Some of your code may be in comments but not visibly so"
            ],
      Dutch
        :-> [ "Syntax kleuring van editor kan meestal niet overweg met genest commentaar",
              "Het kan zo zijn dat een deel van je code in commentaar staat maar dat je dat niet ziet"
            ]
    ]

-- | Translates 'Comment operator' into the chosen language
lexerCommentOperator ::
  -- | The translated strings
  [String]
lexerCommentOperator =
  select
    language
    [ English
        :-> [ "Syntax coloring usually cannot handle names containing --",
              "If you wanted to start a comment, write spaces around --"
            ],
      Dutch
        :-> [ "Syntax kleuring van editor kan meestal niet overweg met namen die -- bevatten",
              "Als je commentaar wilde beginnen schrijf dan spaties voor en na --"
            ]
    ]

-- | Adds commas and 'and' in between each element of the list
commasAndLang ::
  -- | The word to use for 'and'
  String ->
  -- | The list of items to show
  [String] ->
  -- | The result
  String
commasAndLang _ [] = []
commasAndLang _ [x] = x
commasAndLang a [x, y] = x ++ " " ++ a ++ " " ++ y
commasAndLang a (x : y : zs) = x ++ ", " ++ commasAndLang a (y : zs)

commasAnd :: [String] -> String
commasAnd = commasAndLang "and"

kommasEn :: [String] -> String
kommasEn = commasAndLang "en"
