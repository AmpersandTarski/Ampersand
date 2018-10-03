module Ampersand.Input.PreProcessor (
      preProcess
    , preProcess'
    , PreProcDefine
) where

import Data.List
import qualified Data.List.NonEmpty as NEL
import Data.String
import Data.Maybe
import Data.Bool
import Data.Either
import Data.Functor
import Control.Monad hiding (guard)
import Control.Applicative hiding ( many )
import Text.Parsec hiding ( (<|>) )
import Text.Parsec.Error
import Prelude
import Ampersand.Input.ADL1.CtxError

type PreProcDefine = String

preProcess :: String -> [PreProcDefine] -> String -> Guarded String
preProcess f d i = case preProcess' f d i of
                   (Left  err) -> Errors $ (PE . Message . show $ err) NEL.:| []
                   (Right out) -> Checked out

preProcess' :: String -> [PreProcDefine] -> String -> Either ParseError String
preProcess' fileName defs input = (block2file defs True) <$> (file2block fileName input)

-- Run the parser
file2block :: String -> String -> Either ParseError Block
file2block fileName = (parseLexedFile fileName) <=< (fullLexer fileName)

-- LEXER

type Lexer a = Parsec String () a


data LexLine = Codeline String
             | IfNotStart Guard
             | IfStart Guard
             | ElseClause
             | IfEnd
instance Show LexLine where
  show = showLex

showLex :: LexLine -> String
showLex (Codeline x)   = x
showLex (IfNotStart x) = "If Not " ++ guard x
showLex (IfStart x)    = "If "     ++ guard x
showLex (ElseClause)   = "Else"
showLex (IfEnd)        = "End If"

--preProcDirective :: Lexer ()


whitespace :: Lexer ()
whitespace = skipMany1 space

ifWithGuard :: Lexer LexLine
ifWithGuard = (IfStart . Guard) <$>
              (try(string "IF")      *>
               whitespace            *>
               some alphaNum        <*
               manyTill anyChar endOfLine
              )

ifNotWithGuard :: Lexer LexLine
ifNotWithGuard = (IfNotStart . Guard) <$>
                 (try(string "IFNOT")   *>
                  whitespace            *>
                  some alphaNum        <*
                  manyTill anyChar endOfLine
                 )

ifEnd :: Lexer LexLine
ifEnd = (const IfEnd) <$>
            (try(string "ENDIF")   *>
             manyTill anyChar endOfLine
            )

elseClause :: Lexer LexLine
elseClause = (const ElseClause) <$>
            (try(string "ELSE")   *>
             manyTill anyChar endOfLine
            )

-- This fails without consuming input on comments,
-- but fails with consuming input (and message "preproccesor directive")
-- for comments starting with #.
preProcDirective :: Lexer LexLine
preProcDirective = try(spaces *> string "--#") *> spaces *>
                  (ifNotWithGuard <|> ifWithGuard <|> elseClause <|> ifEnd <?> "preproccesor directive")

lexLine :: Lexer LexLine
lexLine = preProcDirective <|> Codeline <$> manyTill anyChar endOfLine

fullLexer :: String -> String -> Either ParseError [LexLine]
fullLexer filename = parse (many lexLine <* eof) filename

-- PARSER
newtype Guard = Guard String   deriving (Show)
guard :: Guard -> String
guard (Guard x) = x

data BlockElem = LineElem String
               | IfElem IfBlock
               | IfNotElem IfNotBlock
     deriving (Show)

type Block   = [ BlockElem ]

data IfBlock    = IfBlock    Guard Block (Maybe Block)
     deriving (Show)
data IfNotBlock = IfNotBlock Guard Block (Maybe Block)
     deriving (Show)

type TokenParser a = Parsec [LexLine] () a

parserToken :: (LexLine -> Maybe a) -> TokenParser a
parserToken constructor = tokenPrim showLex (\pos _ _ -> incSourceLine pos 1) constructor

lineElem :: TokenParser BlockElem
lineElem = parserToken ((fmap LineElem) <$> line2string)
  where
  line2string :: LexLine -> Maybe String
  line2string (Codeline s) = Just s
  line2string _          = Nothing

ifElemStart :: TokenParser Guard
ifElemStart = parserToken guard2string
  where
  guard2string (IfStart g) = Just g
  guard2string _           = Nothing

ifNotElemStart :: TokenParser Guard
ifNotElemStart = parserToken guard2string
  where
  guard2string (IfNotStart g) = Just g
  guard2string _              = Nothing

ifElemEnd :: TokenParser ()
ifElemEnd = parserToken (matchIfEnd)
  where
  matchIfEnd IfEnd = Just ()
  matchIfEnd _     = Nothing

elseClauseStart :: TokenParser ()
elseClauseStart = parserToken (matchIfEnd)
  where
  matchIfEnd ElseClause = Just ()
  matchIfEnd _          = Nothing

ifBlock :: TokenParser IfBlock
ifBlock = do
  guard'    <- ifElemStart;
  lines'    <- many blockElem
  elseLines <- optionMaybe( elseClauseStart *> many blockElem)
  _         <- ifElemEnd
  return (IfBlock guard' lines' elseLines)

ifNotBlock :: TokenParser IfNotBlock
ifNotBlock = do
  guard'    <- ifNotElemStart;
  lines'    <- block
  elseLines <- optionMaybe( elseClauseStart *> many blockElem)
  _         <- ifElemEnd
  return (IfNotBlock guard' lines' elseLines)

blockElem :: TokenParser BlockElem
blockElem = choice [lineElem, IfElem <$> ifBlock, IfNotElem <$> ifNotBlock ] <?> "a block element"

block :: TokenParser Block
block = many blockElem

parseLexedFile :: String -> [LexLine] -> (Either ParseError Block)
parseLexedFile fileName = parse (block <* eof) fileName

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
blockElem2string defs showing  (IfElem (IfBlock (Guard guard') block' elseBlock)) =
    "--#IF " ++ guard' ++ "\n" ++
    (block2file defs (showing && (guard' `elem` defs)) block') ++
    (maybe ""
      ( ("--#ELSE\n" ++) .
        block2file defs (showing && not (guard' `elem` defs))
      ) elseBlock
    ) ++
    "--#ENDIF\n"
blockElem2string defs showing  (IfNotElem (IfNotBlock (Guard guard') block' elseBlock)) =
    "--#IFNOT " ++ guard' ++ "\n" ++
    (block2file defs (showing && not (guard' `elem` defs)) block') ++
    (maybe ""
      ( ("--#ELSE\n" ++) .
        block2file defs (showing && (guard' `elem` defs))
      ) elseBlock
    ) ++
    "--#ENDIF\n"
