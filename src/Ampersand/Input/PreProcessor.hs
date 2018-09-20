module Ampersand.Input.PreProcessor (
      preProcess
    , PreProcDefine
) where

import Data.List
import Data.String
import Data.Char
import Data.Maybe
import Data.Bool
import Data.Either
import Data.Functor
import Control.Monad hiding (guard)
import Control.Applicative hiding ( many )
import Text.Parsec hiding ( (<|>) )
import Prelude

type PreProcDefine = String

preProcess :: [PreProcDefine] -> String -> String
preProcess defs = block2file defs True . (either (error . show) id) <$> file2block ""
 
-- Run the parser
file2block :: String -> String -> Either ParseError Block
file2block fileName = parseFile fileName . map lexLine . lines

-- LEXER
data LexLine = Codeline String
             | IfNotStart Guard
             | IfStart Guard
             | IfEnd
instance Show LexLine where
  show = showLex

showLex :: LexLine -> String
showLex (Codeline x)   = x
showLex (IfNotStart x) = "If Not " ++ guard x
showLex (IfStart x)    = "If "     ++ guard x
showLex (IfEnd)        = "End If"

parseIfStartLine :: String -> Maybe LexLine
parseIfStartLine x = fmap    (IfStart . Guard . head . words)    (stripPrefix "--#IF "    $ dropWhile isSpace x)

parseNotIfStartLine :: String -> Maybe LexLine
parseNotIfStartLine x = fmap (IfNotStart . Guard . head . words) (stripPrefix "--#IFNOT " $ dropWhile isSpace  x)

parseIfEndLine :: String -> Maybe LexLine
parseIfEndLine x = fmap (const IfEnd) (stripPrefix "--#ENDIF" $ dropWhile isSpace x)

lexLine :: String -> LexLine
lexLine line = fromMaybe (Codeline line) $ (parseIfStartLine line
                                          <|> parseNotIfStartLine line
                                          <|> parseIfEndLine line)

-- PARSER
newtype Guard = Guard String
guard :: Guard -> String
guard (Guard x) = x

data BlockElem = LineElem String
               | IfElem IfBlock
               | IfNotElem IfNotBlock

type Block   = [ BlockElem ]

data IfBlock    = IfBlock    Guard Block
data IfNotBlock = IfNotBlock Guard Block

type Parser a = Parsec [LexLine] () a

myToken :: (LexLine -> Maybe a) -> Parser a
myToken constructor = tokenPrim showLex (\pos _ _ -> incSourceLine pos 1) constructor

codeLine :: Parser BlockElem
codeLine = myToken ((fmap LineElem) <$> line2string)
  where
  line2string :: LexLine -> Maybe String
  line2string (Codeline s) = Just s
  line2string _          = Nothing

ifStart :: Parser Guard
ifStart = myToken guard2string
  where
  guard2string (IfStart g) = Just g
  guard2string _           = Nothing

ifNotStart :: Parser Guard
ifNotStart = myToken guard2string
  where
  guard2string (IfNotStart g) = Just g
  guard2string _              = Nothing

ifEnd :: Parser ()
ifEnd = myToken (matchIfEnd)
  where
  matchIfEnd IfEnd = Just ()
  matchIfEnd _     = Nothing

ifBlock :: Parser IfBlock
ifBlock = do
  guard' <- ifStart;
  lines' <- many blockElem
  _     <- ifEnd
  return (IfBlock guard' lines')

ifNotBlock :: Parser IfNotBlock
ifNotBlock = do
  guard' <- ifNotStart;
  lines' <- block
  _     <- ifEnd
  return (IfNotBlock guard' lines')

blockElem :: Parser BlockElem
blockElem = choice [codeLine, IfElem <$> ifBlock, IfNotElem <$> ifNotBlock ] <?> "a block element"

block :: Parser Block
block = many blockElem

parseFile :: String -> [LexLine] -> (Either ParseError Block)
parseFile fileName = parse (block <* eof) fileName

-- Turn Blocks Back into text

-- Turn a block back
-- Could be done with a stateful monad where
block2file :: [PreProcDefine] -> Bool -> Block -> String
block2file defs shown = concat . map (blockElem2string defs shown)

-- Handle single entry in a block
-- Responsible for adding newlines
--                  list of flags      Showing this element?  2 process    output
blockElem2string :: [PreProcDefine] -> Bool ->                BlockElem -> String
blockElem2string _    True  (LineElem line) = line ++ "\n"
blockElem2string _    False (LineElem line) = "--hiden by preprocc " ++ line ++ "\n"
-- Lots of unpacking to get to the IfBlock
blockElem2string defs showing  (IfElem    (IfBlock    (Guard guard') block')) =
    "--#IF " ++ guard' ++ "\n" ++
    (block2file defs (showing &&     (guard' `elem` defs)) block') ++
    "--#ENDIF\n"
blockElem2string defs showing  (IfNotElem (IfNotBlock (Guard guard') block')) =
    "--#IFNOT " ++ guard' ++ "\n" ++
    (block2file defs (showing && not (guard' `elem` defs)) block') ++
    "--#ENDIF\n"
