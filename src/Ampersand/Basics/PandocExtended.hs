{-# LANGUAGE DeriveDataTypeable #-}

module Ampersand.Basics.PandocExtended
  ( PandocFormat (..),
    Markup (..),
    markup2Markdown,
    string2Blocks,
  )
where

import Ampersand.Basics.Hashing
import Ampersand.Basics.Languages
import Ampersand.Basics.Prelude hiding (toList)
import Ampersand.Basics.String (toText1Unsafe)
import Ampersand.Basics.Unique
import Ampersand.Basics.Version
import qualified RIO.Text as T
import Text.Pandoc hiding (Meta)
import Text.Pandoc.Builder hiding (str)

data PandocFormat = HTML | ReST | LaTeX | Markdown deriving (Eq, Show, Ord, Enum, Bounded)

data Markup = Markup
  { amLang :: Lang, -- No Maybe here!  In the A-structure, it will be defined by the default if the P-structure does not define it. In the P-structure, the language is optional.
    amPandoc :: Blocks
  }
  deriving (Show, Eq, Ord, Typeable, Data)

instance Unique Markup where
  showUnique x = toText1Unsafe ("Markup_" <> (tshow . abs . hash . tshow) x)

-- | a way to show the pandoc in a default way. We currently use Markdown for this purpose.
markup2Markdown :: Markup -> Text
markup2Markdown = blocks2String . amPandoc
  where
    blocks2String :: Blocks -> Text
    blocks2String ec =
      case runPure $ writeMarkdown def (Pandoc nullMeta (toList ec)) of
        Left pandocError -> fatal $ "Pandoc error: " <> tshow pandocError
        Right txt -> txt

-- | use a suitable format to read generated strings. if you have just normal text, ReST is fine.
-- | defaultPandocReader should be used on user-defined strings.
string2Blocks :: PandocFormat -> Text -> Blocks
string2Blocks defaultformat str =
  case runPure $ theParser (removeCRs str) of
    Left err ->
      fatal
        ( "Proper error handling of Pandoc is still TODO."
            <> "\n  This particular error is caused by some "
            <> tshow defaultformat
            <> " in your script:"
            <> "\n"
            <> tshow err
        )
    Right (Pandoc _ blocks) -> fromList blocks
  where
    theParser =
      case defaultformat of
        Markdown -> readMarkdown def
        ReST -> readRST def
        HTML -> readHtml def
        LaTeX -> readLaTeX def

    removeCRs :: Text -> Text
    removeCRs txt = case T.uncons txt of
      Nothing -> mempty
      Just ('\r', tl) -> case T.uncons tl of
        Nothing -> T.singleton '\r'
        Just ('\n', xs) -> T.cons '\n' $ removeCRs xs
        Just (c, xs) -> T.cons '\r' . T.cons c $ removeCRs xs
      Just (h, tl) -> T.cons h (removeCRs tl)
