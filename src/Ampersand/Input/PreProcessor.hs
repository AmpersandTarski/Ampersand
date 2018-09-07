module Ampersand.Input.PreProcessor (
      preProcess
    , PreProcDefine
) where

import Data.List
import Data.String
import Data.Char
import Data.Either
import Data.Maybe
import Prelude

type PreProcDefine = String

preProcess :: [PreProcDefine] -> String -> String
preProcess defs = block2file defs True . file2block
 
-- Run the full parser
file2block :: String -> Block
file2block = parseBlock . map parseLine . lines

-- Turn a block back
block2file :: [PreProcDefine] -> Bool -> Block -> String
block2file defs shown = unlines . map (blockElem2string defs shown)

-- Handle single entry in a block
blockElem2string :: [PreProcDefine] -> Bool -> BlockElem -> String
blockElem2string _    True  (Left  line) = line
blockElem2string _    False (Left  line) = "-- hide by preprocc " ++ line
-- Lots of unpacking to get to the IfBlock
blockElem2string defs False (Right (IfBlock (Guard guard) block)) = 
    "-- IF " ++ guard ++ block2file defs False block ++ "-- ENDIF"
blockElem2string defs True  (Right (IfBlock (Guard guard) block)) = 
    "-- IF " ++ guard ++ "\n" ++ 
    (block2file defs (guard `elem` defs) block) ++ 
    "\n-- ENDIF"
 
-- Here the experimentation starts

{- Our grammar is:
Codeline = Any line that is not IfStartLine or IfEndLine
Word = consecutive non-whitespace chars
String litterals are encased in pairs of '
: is concatenation
| is disjunction
\ is negation

IfStartLine = ' '* : '-- if ' : Word : ' '* : '\n'
IfEndLine   = ' '* : '-- endif ' : ' '* : '\n'
CodeLine    = Line \ (IfStartLine | IfEndLine)

IfBlock = IfStartLine : Block : IfEndLine

Block = (CodeLine | IfBlock)*
 -}

-- Do we want to implement the relevant strings

-- "Implement" the grammar in Haskell Types
newtype Guard = Guard String

type BlockElem = Either String IfBlock
type Block   = [ BlockElem ]
data IfBlock = IfBlock Guard Block

data Line = Codeline String
          | IfStart Guard
          | IfEnd
 
-- First, define a function that reads our primitive 'line'

-- It must be possible to do this nicer. Probably with something like <* and *> or <|>
parseLine :: String -> Line
parseLine line = case parseIfStartLine line of 
            Just guard -> IfStart guard
            _          -> case parseIfEndLine line of 
                               Just () -> IfEnd 
                               _       -> Codeline line
                                   
parseIfStartLine :: String -> Maybe Guard
parseIfStartLine x = fmap (Guard . head . words) (stripPrefix "-- IF " $ stripFront x)

parseIfEndLine :: String -> Maybe ()
parseIfEndLine x = fmap (const ()) (stripPrefix "-- ENDIF" $ stripFront x)

-- Next, define a function that processes our data type [Line]

-- Probably want something like:
-- parseBlock :: [Line] -> ( [Line] , Block ) 
-- or
-- parseBlock :: [Line] -> ( Block, [Line] ) 
parseBlock :: [Line] -> Block
parseBlock [] = []
parseBlock (line:rest) = case line of
    Codeline plainLine -> (Left plainLine) : parseBlock rest
    IfStart guard      -> let (blockLines, remainingLines) = break endsBlock rest in
                              (Right $ IfBlock guard $ parseBlock blockLines) : parseBlock remainingLines
    IfEnd              -> (Left "") : parseBlock rest


 
-- Helper functions

stripFront :: String -> String
stripFront = dropWhile isSpace

-- there has to be a better way
endsBlock :: Line -> Bool
endsBlock IfEnd = True
endsBlock _     = False
