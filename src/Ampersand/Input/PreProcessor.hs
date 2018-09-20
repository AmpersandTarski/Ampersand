module Ampersand.Input.PreProcessor (
      preProcess
    , PreProcDefine
) where

import Data.List
import Data.String
import Data.Char
import Data.Maybe
import GHC.Base

type PreProcDefine = String

preProcess :: [PreProcDefine] -> String -> String
preProcess defs = block2file defs True . file2block
 
-- Run the full parser
file2block :: String -> Block
file2block = parseBlock . map parseLine . lines

-- Turn a block back
block2file :: [PreProcDefine] -> Bool -> Block -> String
block2file defs shown = concat . map (blockElem2string defs shown)

-- Do we want to implement the relevant strings e.g. "--# IF" as constants?

-- "Implement" the grammar in Haskell Types
newtype Guard = Guard String

data BlockElem = LineElem String
               | IfElem IfBlock
               | IfNotElem NotIfBlock

type Block   = [ BlockElem ]

data IfBlock    = IfBlock    Guard Block
data NotIfBlock = IfNotBlock Guard Block

data Line = Codeline String
          | IfNotStart Guard
          | IfStart Guard
          | IfEnd
 
-- First, define a function that reads our primitive 'line'

-- We now do (f x) <|> (g x), would be nicer to have (f <|> g) x
-- would require <|> :: (a -> Maybe b) -> (a -> Maybe b) -> (a -> Maybe b)
-- Monad >=> comes close but not quite
parseLine :: String -> Line
parseLine line = fromMaybe (Codeline line) $ (parseIfStartLine line
                                          <|> parseNotIfStartLine line
                                          <|> parseIfEndLine line)

--f :: (a -> Maybe b) -> (a -> Maybe b) -> (a -> Maybe b)

parseIfStartLine :: String -> Maybe Line
parseIfStartLine x = fmap    (IfStart . Guard . head . words)    (stripPrefix "--#IF "    $ dropWhile isSpace x)

parseNotIfStartLine :: String -> Maybe Line
parseNotIfStartLine x = fmap (IfNotStart . Guard . head . words) (stripPrefix "--#IFNOT " $ dropWhile isSpace  x)

parseIfEndLine :: String -> Maybe Line
parseIfEndLine x = fmap (const IfEnd) (stripPrefix "--#ENDIF" $ dropWhile isSpace x)

-- Next, define a function that processes our data type [Line]

-- Use blockParser to turn a list of lines into a list of BlockElem
-- Note that blockParser could consume more than one line
-- Fold after build?
parseBlock :: [Line] -> Block
parseBlock [] = []
parseBlock lineList = (\(x, y) -> y ++ parseBlock x) . blockParser $ lineList

blockParser :: [Line] -> ([Line], [BlockElem] )
blockParser [] = ([], [])
blockParser (line:rest) = case line of
    Codeline plainLine -> (rest, [LineElem plainLine])
    IfStart guard      -> let (blockLines, remainingLines) = break endsIfBlock rest in
                              ( remainingLines, [IfElem    $ IfBlock    guard (parseBlock blockLines)] )
    IfNotStart guard   -> let (blockLines, remainingLines) = break endsIfBlock rest in
                              ( remainingLines, [IfNotElem $ IfNotBlock guard (parseBlock blockLines)] )
    IfEnd              -> (rest, [] )

endsIfBlock :: Line -> Bool
endsIfBlock IfEnd = True
endsIfBlock _     = False



-- Handle single entry in a block
-- Responsible for adding newlines
--                  list of flags      Showing this element?  2 process    output
blockElem2string :: [PreProcDefine] -> Bool ->                BlockElem -> String
blockElem2string _    True  (LineElem line) = line ++ "\n"
blockElem2string _    False (LineElem line) = "--hiden by preprocc " ++ line ++ "\n"
-- Lots of unpacking to get to the IfBlock
blockElem2string defs hiding  (IfElem    (IfBlock    (Guard guard) block)) =
    "--#IF " ++ guard ++ "\n" ++
    (block2file defs (hiding &&     (guard `elem` defs)) block) ++
    "--#ENDIF\n"
blockElem2string defs hiding  (IfNotElem (IfNotBlock (Guard guard) block)) =
    "--#IFNOT " ++ guard ++ "\n" ++
    (block2file defs (hiding && not (guard `elem` defs)) block) ++
    "--#ENDIF\n"
