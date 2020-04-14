{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveFunctor #-}
module GRDT.Parser where --Define all parse rule
import GRDT.Data
import GHC.Base as A (Alternative(empty))
import Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Char
-- import Control.Monad.Combinators -- from parser-combinators
import Data.Void
import Data.Either
import Language.Haskell.TH

type Parser = Parsec Void String
type FDAfterParse = FoundDecl (InpName,[Card]) InpName
type IFAfterParse = Interface InpName (InpName, InpName) InpName FDAfterParse
type CPAfterParse = Component (InpName, -- name
                               InpName) -- target Haskell type (typecast if necessary)
                               InpName  -- types that come out of the expr when asked
                               FDAfterParse -- info used in expr
type IFWhileParse = Interface InpName -- type
                              (InpName, Maybe InpName) () InpName
type CPWhileParse = Component (InpName, Maybe InpName) () InpName

findDecl :: Declaration -> FDAfterParse
findDecl (Decl nm (RelationType s t) c)  = FoundDecl (nm,c) (s,t)

parseGRDT :: String -> Q ([Declaration],[IFAfterParse])
parseGRDT inp
 = do p <- location -- Language-Haskell-TH, gives position, module name and filename
      let (row,col') = loc_start p
      -- prefix determines a best guess as to how we are called. 'Q' does not allow us to inspect the Haskell source directly.
      let (prefix,col) = if col' >= 7 then (replicate (col'-7) ' '++"[grdt|",col') else ("",1)
      let spos = SourcePos (loc_filename p) (mkPos row) (mkPos col)
      let sp = PosState inp 0 spos (mkPos 8) prefix
      let st = State inp 0 sp []
      case snd $ runParser' parse_scripts st of
        (Left e) -> fail (errorBundlePretty e)
        (Right v) -> performLookups v

performLookups :: ([Declaration], [IFWhileParse])
               -> Q ([Declaration], [IFAfterParse])
performLookups (decls,ifs)
 = (\v -> (decls,v)) <$> traverse (onComponents fillDetails) ifs
 where lkp :: InpName -> Q FDAfterParse
       lkp s
         = maybe (fail' s ((++) "undeclared relation " . show)) (return . findDecl)
            $ M.lookup (runUP s) mp
       mp = M.fromList [(runUP nm,d) | d@(Decl nm _ _) <- decls]
       fillDetails :: InpName
                   -> CPWhileParse
                   -> Q CPAfterParse
       fillDetails t (CP (nm,tp) dclExpr)
        = do v' <- traverse lkp dclExpr
             v <- onRAIdent (const (pure t)) v'
             return (CP (nm,maybe (target v) id tp) v)

checkIfElem :: String -> [String] -> Bool
checkIfElem ty lst =
    if ty `elem` lst then True else False

get_string:: Parser String
get_string = some (satisfy f)
   where f x = not (elem x "\t\n\r :;()[]{}<>?,.+\"~@!#$%^&")

spaceOrComment :: Parser ()
spaceOrComment = space *> comment
comment :: Parser ()
comment = (string "--" *> many (satisfy (\x -> x `notElem` "\n\r")) *> spaceOrComment)
        <|> return ()
        -- todo: {- (inline) comments like this -}

comma :: Parser String
comma = string "," <* spaceOrComment

option_alt :: Alternative f => Parser (f a) -> Parser (f a)
option_alt p = p <|> return A.empty

-- Parse script as list of decs and list of infos, will also check if name are valid or not.
parse_scripts :: Parser ([Declaration],[IFWhileParse])
parse_scripts = spaceOrComment *> (partitionEithers <$> parse_scriptElement `sepBy` spaceOrComment)

parse_scriptElement :: Parser (Either Declaration IFWhileParse)
parse_scriptElement =
  (Right <$> parse_interface) <|> (Left <$> parse_declaration)

parse_declaration :: Parser Declaration
parse_declaration =
  Decl <$> (parse_name <* spaceOrComment <* string "::" <* spaceOrComment)
       <*> parse_relation <* spaceOrComment
       <*> option_alt (string "[" *> parse_card `sepBy` comma <* string "]") <* spaceOrComment

parse_relation :: Parser RelationType
parse_relation =
  RelationType <$> parse_name <* spaceOrComment <* string "*" <* spaceOrComment <*> parse_name

parse_card :: Parser Card
parse_card =
  ((string "UNI" *> return UNI )
  <|> (string "TOT" *> return TOT)
  <|> (string "INJ" *> return INJ)
  <|> (string "SUR" *> return SUR))
  <?> "Cardinality (UNI, TOT, INJ, or SUR)"

-- parse name
parse_name :: Parser InpName
parse_name = up get_string

-- parse interface
parse_interface :: Parser IFWhileParse
parse_interface =
  Intf <$> (string "INTERFACE" *> spaceOrComment *> parse_name <* spaceOrComment)
       <*> (string "::" *> spaceOrComment *> parse_name <* spaceOrComment)
       <*> option_alt (string "[" *> spaceOrComment *> (parse_component `sepBy` (comma <* spaceOrComment)) <* string "]" <* spaceOrComment)

-- Parse components
parse_component :: Parser CPWhileParse
parse_component = (cp <$> (parse_name <* spaceOrComment <* string ":" <* spaceOrComment)
                      <*> parse_expr <* spaceOrComment
                      <*> option_alt (fmap Just parse_name) <* spaceOrComment)
  where cp nm e tp = CP (nm,tp) e
parse_expr :: Parser (RAExpr () InpName)
parse_expr = RAIdent () <$ string "I" <|> (parse_name <* spaceOrComment >>= rest)
  where
  rest :: InpName -> Parser (RAExpr () InpName)
  rest v = (RAConverse <$> (string "~" *> rest v))
           <|> return (RAVar v)

up :: Parser a -> Parser (UserProvided a)
up p = UP <$> getParserState <*> p -- getParserState :: MonadParsec e s m => m (State s e)
