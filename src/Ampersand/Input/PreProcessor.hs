module Ampersand.Input.PreProcessor (
      preProcess
    , preProcess'
    , PreProcDefine
    , processFlags
) where

import           Ampersand.Basics hiding (guard,many,try)
import           Ampersand.Input.ADL1.CtxError
import           Data.Char(isSpace)
import qualified RIO.List as L
import qualified Data.List.NonEmpty as NEL
import qualified Data.Set as Set
import           Text.Parsec hiding ( (<|>) )

type PreProcDefine = String

-- | Remove and add flags to the set of enabled flags based on an import statement.
processFlags :: Set.Set PreProcDefine -- ^ Old set of preprocessor flags
             -> [String]              -- ^ List of preprocessor flags from an import
             -> Set.Set PreProcDefine -- ^ Set of preprocessor flags after import
processFlags oldFlags importList = Set.difference (Set.union oldFlags addedDefines) removedDefines
  where (addedDefines, removedDefines) =
            (\(removed, added) -> (Set.fromList added, Set.fromList $ map (fromMaybe "" . L.stripPrefix "!") removed))
            (L.partition (L.isPrefixOf "!") importList)

-- Shim that changes our 'Either ParseError a' from preProcess' into 'Guarded a'
-- | Runs the preProcessor on input
preProcess :: String          -- ^ filename, used only for error reporting
           -> Set.Set PreProcDefine -- ^ list of flags, The list of defined 'flags
           -> String          -- ^ input, The actual string to processs
           -> Guarded String  -- ^ result, The result of processing
preProcess f d i = case preProcess' f d i of
                   (Left  err) -> Errors $ (PE err) NEL.:| []
                   (Right out) -> Checked out []

-- | Runs the preProcessor on input
preProcess' :: String                   -- ^ filename, used only for error reporting
            -> Set.Set PreProcDefine       -- ^ list of flags, The list of defined 'flags
            -> String                   -- ^ input, The actual string to process
            -> Either ParseError String -- ^ result, The result of processing
-- We append "\n" because the parser cannot handle a final line not terminated by a newline.
preProcess' fileName defs input = (block2file defs True) <$> (file2block fileName (input ++ "\n"))

-- Run the parser
file2block :: String                  -- ^ filename, used only for error reporting
           -> String                  -- ^ input, the string to process
           -> Either ParseError Block -- ^ result
file2block fileName = (parseLexedFile fileName) <=< (runLexer fileName)

---- LEXER

newtype Guard = Guard String
guard :: Guard -> String
guard (Guard x) = x

data LexLine = CodeLine String
             | IncludeLine String String
             | IfNotStart Guard
             | IfStart Guard
             | ElseClause
             | EndIf
instance Show LexLine where
  show = showLex
showLex :: LexLine -> String
showLex (CodeLine x)      = "LEX: CODELINE " ++ x
showLex (IncludeLine x y) = "LEX: INCLUDE "  ++ x ++ "--#" ++ y
showLex (IfNotStart x)    = "LEX: IFNOT "    ++ guard x
showLex (IfStart x)       = "LEX: IF "       ++ guard x
showLex (ElseClause)      = "LEX: ELSE"
showLex (EndIf)           = "LEX: ENDIF"

type Lexer a = Parsec String () a

-- | Transform the String 'input' into a list of LexLine tokens. Using 'filename' for error reporting.
runLexer :: String -- ^ filename, only used for error reporting
         -> String -- ^ input, the string to process
         -> Either ParseError [LexLine]
runLexer filename = parse (many lexLine <* eof) filename

lexLine :: Lexer LexLine
lexLine = preProcDirective <|> includeLine <|> codeLine

codeLine :: Lexer LexLine
codeLine = CodeLine <$> untilEOL

includeLine :: Lexer LexLine
includeLine  = do {
    ; spaces'  <- try (many space <* string "INCLUDE")
    ; included <- manyTill anyChar ((lookAhead . try )
                                     (    return () <$> string "--#"
                                      <|> return () <$> endOfLine
                                     )
                                   )
    ; flags    <- string "--#" *> untilEOL <|> endOfLine *> return ""
    ; return $ IncludeLine (spaces' ++ "INCLUDE" ++ included) flags
    }

preProcDirective :: Lexer LexLine
preProcDirective = (try preProcPrefix) *>
                     (   ifNotGuard
                     <|> ifGuard
                     <|> elseClause
                     <|> ifEnd
                     <?> "preproccesor directive"
                     )

-- This pattern signifies the line is meant for the preProcessor.
-- Lines that don't start with this pattern are 'CodeLine's
preProcPrefix :: Lexer ()
preProcPrefix = whitespace *> string "--" *> many (char '-') *> whitespace *> char '#' *> whitespace

ifGuard :: Lexer LexLine
ifGuard = (IfStart . Guard) <$>
              (try(string "IF") *>
               whitespace       *>
               some alphaNum    <*
               untilEOL
              )

ifNotGuard :: Lexer LexLine
ifNotGuard = (IfNotStart . Guard) <$>
                 (try(string "IFNOT") *>
                  whitespace          *>
                  some alphaNum       <*
                  untilEOL
                 )

elseClause :: Lexer LexLine
elseClause = (const ElseClause) <$> (try(string "ELSE") *> untilEOL)

ifEnd :: Lexer LexLine
ifEnd = (const EndIf) <$> (try(string "ENDIF") *> untilEOL)

-- Helper Lexers
whitespace :: Lexer ()
whitespace = skipMany $ satisfy (\x -> isSpace x && not (x == '\n' || x == '\r'))

untilEOL :: Lexer String
untilEOL = manyTill anyChar endOfLine

---- PARSER

-- | A block element is either a normal line, or a Guarded Block (i.e. an IF or IFNOT block)
data BlockElem = LineElem String
               | IncludeElem String String
               | GuardedElem GuardedBlock  -- These cover IF and IFNOT blocks

type Block   = [ BlockElem ]

-- The first BOOL here determines whether this is an IF or IFNOT block
data GuardedBlock = GuardedBlock Bool  -- ^ This covers whether this is an IF or an IFNOT block. True for IF, false for IFNOT.
                                 Guard -- ^ The guard of the IF or IFNOT
                                 Block -- ^ The actual Block
                                 (Maybe Block)  -- ^ An optional ELSE block.
                                                {- (Note that there is a difference between Maybe [] and Nothing here.
                                                The first represents and empty ELSE block, the second an absent block.
                                                This matters for preserving line numbers.
                                                -}

type TokenParser a = Parsec [LexLine] () a

parseLexedFile :: String -> [LexLine] -> (Either ParseError Block)
parseLexedFile fileName = parse (many blockElem <* eof) fileName

blockElem :: TokenParser BlockElem
blockElem = choice [lineElem, includeElem, ifBlock, ifNotBlock ]

lineElem :: TokenParser BlockElem
lineElem = parserToken ((fmap LineElem) <$> line2string)
  where
  line2string (CodeLine s) = Just s
  line2string _            = Nothing

includeElem :: TokenParser BlockElem
includeElem = parserToken (line2string)
  where
  line2string (IncludeLine s m) = Just $ IncludeElem s m
  line2string _                 = Nothing

ifBlock :: TokenParser BlockElem
ifBlock = GuardedElem <$> (pure (GuardedBlock True)
          <*> ifElemStart
          <*> many blockElem
          <*> optionMaybe(elseClauseStart *> many blockElem)
          <*  ifElemEnd
          )

ifNotBlock :: TokenParser BlockElem
ifNotBlock = GuardedElem <$> (pure (GuardedBlock False)
             <*> ifNotElemStart
             <*> many blockElem
             <*> optionMaybe(elseClauseStart *> many blockElem)
             <*  ifElemEnd
             )
{-| Helper function to create parsers. Takes a constructor of type (LexLine -> Maybe a) and returns a parser. The
    returned parser yields  x  if the constructor returns  Just x  and the parser fails if the constructor returns Nothing.
-}
parserToken :: (LexLine -> Maybe a) -> TokenParser a
parserToken constructor = tokenPrim showLex (\pos _ _ -> incSourceLine pos 1) constructor

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
ifElemEnd = parserToken matchIfEnd
  where
  matchIfEnd EndIf = Just ()
  matchIfEnd _     = Nothing

elseClauseStart :: TokenParser ()
elseClauseStart = parserToken matchIfEnd
  where
  matchIfEnd ElseClause = Just ()
  matchIfEnd _          = Nothing

---- TURN BLOCK BACK INTO TEXT

{- Note the recursion here:
   block2file calls blockElem2String, which might call showGuardedBlock, which calls block2file and potentially also
   showElse, which again calls block2file

   This matches the recursion where a 'Block' contains multiple 'BlockElem's which can contain a 'GuardedBlock' which
   contains a main 'Block', and potentially an ELSE 'Block'.
-}

-- | Renders a Block type back into a String, according to some context
block2file :: Set.Set PreProcDefine -- ^ defs, List of defined flags
           -> Bool    -- ^ showing, whether we are showing the current block, or it is hidden
           -> Block   -- ^ block, the block we want to process
           -> String
block2file defs showing = concat . map (blockElem2string defs showing)

-- | Renders a single block element back into text
blockElem2string :: Set.Set PreProcDefine -- ^ flags, the list of active flags
                 -> Bool            -- ^ showing, whether we are showing the current block element, or it is hidden
                 -> BlockElem       -- ^ blockElem, the block element to render
                 -> String
blockElem2string _    True    (LineElem line)           = line ++ "\n"
blockElem2string _    True    (IncludeElem line flags)  = line ++ "   " ++ flags ++ "\n"
blockElem2string _    False   (LineElem line)           = "--hiden by preprocc " ++ line ++ "\n"
blockElem2string _    False   (IncludeElem line flags)  = "--hiden by preprocc " ++ line ++ " " ++ flags ++ "\n"
blockElem2string defs showing (GuardedElem guardedElem) = showGuardedBlock defs showing guardedElem

-- | Renders a GuardedBlock
-- This is where the rendering logic of IF and IFNOT is implemented
-- Simplification of this function is why IF and IFNOT are both represented by the type GuardedBlock
showGuardedBlock :: Set.Set PreProcDefine -- ^ flags, the list of active flags
                 -> Bool            -- ^ showing, whether we are showing the current block element, or it is hidden
                 -> GuardedBlock    -- ^ guardedBlock, the element to render
                 -> String
showGuardedBlock defs showing (GuardedBlock ifType (Guard guard') block elseBlock) =
    -- The  xor (not ifType)  is a succinct way to express the difference between IF blocks and NOTIF blocks
    let showMainBody = (xor (not ifType) (guard' `Set.member` defs)) in
      concat [ guardedBlockName ifType ++ guard' ++ "\n"
             , (block2file defs (showing &&      showMainBody)  block    )
             , (showElse  defs (showing && (not showMainBody)) elseBlock)
             , "--#ENDIF\n"
             ]

-- Helper functions
guardedBlockName :: Bool -> String
guardedBlockName ifType = (if ifType then "--#IF " else "--#IFNOT ")

showElse :: Set.Set PreProcDefine -> Bool -> Maybe Block -> String
showElse defs showing = maybe "" (("--#ELSE\n" ++) . block2file defs showing)

xor :: Bool -> Bool -> Bool
xor p q = (p || q) && not (p && q)

