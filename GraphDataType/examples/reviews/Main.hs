{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Main where -- Read from file and parse it
import GRDT
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad (when)
import Data.Either (partitionEithers)
import System.Exit (die)
import Data.Void (Void)
import qualified Data.Map as Map

-- | We define our own types for how we want to internally represent an article and a person
-- these are typed wrappers around what's known as "Vieze identifier".

newtype Article = Article Int deriving (Eq,Ord,Show)
data Person = Person Int deriving (Eq,Ord,Show)

$( return [] ) -- separate the datatypes above this line from the template-haskell below this line

-- the syntax [grdt| ... |] is to insert some Haskell code here
-- the code below establishes the ontology in 7 lines of code,
-- and then the interface to Haskell.
[grdt|
    userId    :: Person * Int [UNI,TOT,INJ]
    articleId :: Article * Int [UNI,TOT,INJ]
    author    :: Article * Person  -- the author(s) of an article
    canreview :: Person * Article  -- determines if a person would be a potential reviewer
    reviewer  :: Article * Person  -- determines if a person is an assigned reviewer
    name      :: Person * String [UNI,TOT]
    title     :: Article * String [UNI,TOT]

    -- the most convenient data-structure for parsing articles:
    INTERFACE ArticleIn :: Article
      [ articleId: articleId
      , title: title
      , authors: author NamedPerson
      ]
    -- the 'INTERFACE ArticleIn' will create the datatype called ArticleIn,
    -- with the constructor ArticleIn that gets three arguments.
    -- The arguments will be of haskell-types Int, String and NamedPerson respectively

    -- Purpose NamedPerson: this interface is used for parsing and printing representations of people
    INTERFACE NamedPerson :: Person
      [ userId: userId
      , name:name
      ]

    -- Purpose ReviewerIn: this datatype corresponds to what the list of reviewers looks like in the input
    INTERFACE ReviewerIn :: Person
      [ userId: userId
      , name:name
      , review_options: canreview
      ]
    
    -- Purpose PaperInfo: this datatype is most convenient for displaying warnings
    INTERFACE PaperInfo :: Article
      [ title : title
      , possible_reviewers : canreview~ NamedPerson
      , authors : author NamedPerson
      ]
    
    -- Purpose ReviewerInfo: this is the datatype that is most convenient for displaying the output of our program to the user
    INTERFACE ReviewerInfo :: Person
      [ name: name
      , assigned : reviewer~ ArticleIn
      ]

    -- the types 'Article' and 'Person' already exist, so they won't be generated
    -- but we wish to tell the code generator what these types look like
    INTERFACE Article :: Article
      [ articleId:articleId ]
    INTERFACE Person :: Person
      [ userId:userId ]

    |] -- Template-haskell [grdt| ... |] ends here.

-- besides generating some datatypes, the above gives us some functions 'for free'.
-- The following functions are generated through the code above (among others):
-- functions to generate a population based on the generated data-types:
  -- fromArticleIn :: ArticleIn -> Information
  -- fromReviewerIn :: ReviewerIn -> Information
-- functions to combine populations: (Information is in the 'Semigroup' and 'Monoid' class)
  -- (<>) :: Information -> Information -> Information 
-- functions to get data-types out of a population again:
  -- getPaperInfo :: Information -> [PaperInfo]
  -- getReviewerInfo :: Information -> [ReviewerInfo]

main :: IO ()
main = do -- parse the input from stdIn
          (articles,reviewers,reviews_per_article) <- parseIO pInput =<< getContents
          -- collect a population based on our input:
          let information = foldMap fromArticleIn articles <> foldMap fromReviewerIn reviewers
          -- converting the information into a format that's useful for showing the warnings
          mapM_ (warnAboutPaper reviews_per_article) (getPaperInfo information :: [PaperInfo])
          -- add the reviews to the information using a bit of custom code
          let completedInformation = matchPapers reviews_per_article information
          -- convert the population to a format to display
          let reviewerInfo = getReviewerInfo completedInformation
          mapM_ pprintReviewerInfo reviewerInfo

warnAboutPaper :: Int -> PaperInfo -> IO ()
warnAboutPaper rpa (PaperInfo ti rvs ats)
 = do when (nr < rpa) . warn $ show rpa++" reviewer(s) required per paper, this paper can get at most "++show nr
      sequence_ [ warn $ "An author "++show r++" ("++nm++") is listed as a potential reviewer"
                | (NamedPerson r nm)<-rvs,(NamedPerson a _)<-ats, r == a]
 where
   nr = length rvs
   warn s = putStrLn $ "Warning about the paper titled "++show ti++ "\n"++s++"\n"

matchPapers :: Int -> Information -> Information
matchPapers reviews_per_article information = information{reviewer=solution information}
  where solution = matchN reviews_per_article . converse . canreview


-- some auxiliary functions. This is mostly parsing, and a bit of code for matching reviewers to articles
 
matchN :: Int -> Relation Article Person -> Relation Article Person
matchN n (Relation x)
 = Relation (fst (reduce (foldr doubleCountAcc mempty (map doubleCount x))))
 where doubleCount (a,p) = (Map.singleton a [p],Map.singleton p (1::Int))
       doubleCountAcc (mp1,mp2) (mp3,mp4)
         = (Map.unionWith (<>) mp1 mp3, Map.unionWith (+) mp2 mp4)
       reduce :: (Map.Map Article [Person], Map.Map Person Int) -> ([(Article,Person)], Map.Map Person Int)
       reduce (as,ps) = foldr reduceA ([],ps) (Map.toList as)
       reduceA :: (Article,[Person])
               -> ([(Article,Person)], Map.Map Person Int)
               -> ([(Article,Person)], Map.Map Person Int)
       reduceA (a,lst) (acc, ps) | length lst <= n = (acc ++ map ((,) a) lst, ps)
        | otherwise
           = case break (\v -> Map.findWithDefault 0 v ps >= minP) lst of
               (i,v:vs) -> reduceA (a, i++vs) (acc, Map.adjust (+ (-1)) v ps)
               _ -> error ("Expecting "++show n++" to be non-negative")
          where minP::Int
                minP = maximum (map (\v -> Map.findWithDefault 0 v ps) lst)


type Parser = Parsec Void String
parseIO :: Parser v -> String -> IO v
parseIO p input =
  case parse p "" input of
    Left  e -> die (errorBundlePretty e)
    Right x -> return x

spaced :: String -> Parser ()
spaced a = space *> string a *> space

quoted :: Parser String
quoted = string "\"" *> innerstr <* string "\""
  where innerstr = (:) <$> (okChar <|> string "\\\"" *> pure '\"'
                                   <|> string "\\\\" *> pure '\\')
                       <*> innerstr
                   <|> return ""
        okChar = lookAhead printChar *> noneOf "\\\""

pNr :: Parser Int
pNr = read <$> some digitChar

pArticle :: Parser ArticleIn
pArticle = spaced "Article" >> ArticleIn
             <$> pNr <* spaced ":"
             <*> quoted -- title
             <* spaced "[" <*> sepBy pPerson (string ",") <* spaced "]"

pPerson :: Parser NamedPerson
pPerson = space >> NamedPerson <$> pNr <* spaced ":" <*> quoted -- name

pReviewer :: Parser ReviewerIn
pReviewer = spaced "Reviewer" >> ReviewerIn
              <$> pNr <* spaced ":"
              <*> quoted -- name
              <* spaced "[" <*> sepBy (Article <$> pNr) (spaced ",") <* spaced "]"

pInput :: Parser ([ArticleIn],[ReviewerIn],Int)
pInput = uncurry (,,) . partitionEithers
         <$> many (space >> Left <$> pArticle <|> Right <$> pReviewer)
         <* spaced "Reviews per article:" <*> pNr <* space

pprintReviewerInfo :: ReviewerInfo -> IO ()
pprintReviewerInfo (ReviewerInfo nm as) | null as = return ()
 | otherwise = do putStrLn$ nm++" is assigned:"
                  mapM_ pprintArticle as
                  putStrLn ""
pprintArticle :: ArticleIn -> IO ()
pprintArticle (ArticleIn nr nm _)
  = putStrLn $ " - "++show nr++": "++nm
