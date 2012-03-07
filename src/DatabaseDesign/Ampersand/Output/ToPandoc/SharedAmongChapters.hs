{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DatabaseDesign.Ampersand.Output.ToPandoc.SharedAmongChapters 
    ( module Text.Pandoc
    , module Text.Pandoc.Builder
    , Chapter(..)
    , Xreferencable(..)
    , xrefFigure1
    , Purpose(..)
    , purpose
    , purposes
    , purposes2Blocks
    , meaning2Blocks
    , amPandoc
    , isMissing
    , dpRule
    , Counter(..),newCounter,incEis)
where
import DatabaseDesign.Ampersand.Basics  
import DatabaseDesign.Ampersand.Core.AbstractSyntaxTree
import DatabaseDesign.Ampersand.ADL1
import DatabaseDesign.Ampersand.Classes
import DatabaseDesign.Ampersand.Fspec
import Text.Pandoc
import Text.Pandoc.Builder  (toList, codeBlock)
import DatabaseDesign.Ampersand.Output.PredLogic        (PredLogicShow(..), showLatex)
import DatabaseDesign.Ampersand.Misc
import DatabaseDesign.Ampersand.Output.AdlExplanation
import DatabaseDesign.Ampersand.Output.PandocAux

--fatal :: Int -> String -> a
--fatal = fatalMsg "SharedAmongChapters.hs"

data Chapter = Intro 
             | NatLangReqs
             | FunctionalRequirements 
             | Diagnosis 
             | ConceptualAnalysis
             | ProcessAnalysis
             | DataAnalysis
             | SoftwareMetrics
             deriving Show

class Xreferencable a where
  xLabel :: a  -> String
  xrefReference  :: a  -> Inline
  xrefReference a = RawInline "latex" ("\\ref{"++xLabel a++"}")
  xrefLabel :: a -> Inline
  xrefLabel a = RawInline "latex" ("\\label{"++xLabel a++"}")
  
instance Xreferencable Chapter where
  xLabel a = "chapter" ++ escapeNonAlphaNum (show a)
  
instance Xreferencable Picture where
  xLabel a = "figure" ++ escapeNonAlphaNum (uniqueName a)

--Image [Inline] Target
--      alt.text (URL,title)
xrefFigure1 :: Picture -> [Inline]
xrefFigure1 pict = 
   [ RawInline "latex" "\\begin{figure}[htb]\n\\begin{center}\n\\scalebox{.3}[.3]{"
   , Image [Str $ "Here, "++uniqueName pict++" should have been visible"] (uniqueName pict, xLabel pict)
   , RawInline "latex" "}\n"
   , RawInline "latex" ("\\caption{"++latexEscShw (caption pict)++"}\n") 
   , xrefLabel pict
   , RawInline "latex" "\n\\end{center}\n\\end{figure}"]


  
--GMI: What's the meaning of the Int?
dpRule :: Fspc -> Options -> [Rule] -> Int -> [A_Concept] -> [Declaration]
          -> ([([Inline], [[Block]])], Int, [A_Concept], [Declaration])
dpRule fSpec flags = dpR
 where
   dpR [] n seenConcs seenDeclarations = ([], n, seenConcs, seenDeclarations)
   dpR (r:rs) n seenConcs seenDeclarations
     = ( ( [Str (name r)]
         , [ let purps = purposes fSpec (language flags) r in            -- Als eerste de uitleg van de betreffende regel..
             purposes2Blocks flags purps ++
             purposes2Blocks flags [p | d<-nds, p<-purposes fSpec (language flags) d] ++  -- Dan de uitleg van de betreffende relaties
             [ Plain text1 | not (null nds)] ++
             pandocEqnArray [ ( texOnly_Id(name d)
                              , ":"
                              , texOnly_Id(name (source d))++(if isFunction d then texOnly_fun else texOnly_rel)++texOnly_Id(name(target d))++symDefLabel d
                              )
                            |d<-nds] ++
             [ Plain text2 | not (null rds)] ++
             [ Plain text3 ] ++
             (if showPredExpr flags
              then pandocEquation ((showLatex.toPredLogic) r++symDefLabel r)
              else pandocEquation (showMath r++symDefLabel r)
             )++
             [ Plain text4 | isSignal r] ++
             (if not (isSignal r) then [] else
              if showPredExpr flags
              then pandocEquation ((showLatex.toPredLogic) r++symDefLabel r)
              else pandocEquation (showMath r++symDefLabel r)
             )++
             [ Plain text5 | length nds>1]
           ] 
         ): dpNext
       , n'
       , seenCs 
       , seenDs
       )
       where
        text1
         = case (length nds,language flags) of
             (1,Dutch)   -> let d = head nds in
                            [Str ("Om dit te formaliseren is een "++(if isFunction d then "functie" else "relatie")++" "),Str (name d),Str " nodig (",RawInline "latex" $ symDefRef d,Str "):"]
             (1,English) -> let d = head nds in
                            [Str "In order to formalize this, a ", Str (if isFunction d then "function" else "relation"), Space, Str (name d),Str " is introduced (",RawInline "latex" $ symDefRef d,Str "):"]
             (l,Dutch)   -> [Str "Om te komen tot de formalisatie in vergelijking",RawInline "latex" "~",RawInline "latex" $ symDefRef r,Str (" zijn de volgende "++count flags l "relatie"++" nodig.")]
             (l,English) -> [Str "To arrive at the formalization in equation",RawInline "latex" "~",RawInline "latex" $ symDefRef r,Str (", the following "++count flags l "relation"++" are introduced.")]
        text2
         = (case (length nds,length rds,language flags) of
             (0,1,Dutch)   -> [Str "Definitie ",RawInline "latex" $ symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ") wordt gebruikt"]
             (0,1,English) -> [Str "We use definition ",RawInline "latex" $ symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ")"]
             (0,_,Dutch)   -> Str "We gebruiken definities ":commaNLPandoc (Str "en") [RawInline "latex" $ symDefRef d++" ("++texOnly_Id (name d)++")" |d<-rds]
             (0,_,English) -> Str "We use definitions ":commaEngPandoc (Str "and") [RawInline "latex" $ symDefRef d++" ("++texOnly_Id (name d)++")" |d<-rds]
             (_,1,Dutch)   -> [Str "Daarnaast gebruiken we definitie ",RawInline "latex" $ symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ")"]
             (_,1,English) -> [Str "Beside that, we use definition ",RawInline "latex" $ symDefRef (head rds), Space, Str "(", Str (name (head rds)), Str ")"]
             (_,_,Dutch)   -> Str "Ook gebruiken we definities ":commaNLPandoc (Str "en") [RawInline "latex" $ symDefRef d++" ("++texOnly_Id (name d)++")" |d<-rds]
             (_,_,English) -> Str "We also use definitions ":commaEngPandoc (Str "and") [RawInline "latex" $ symDefRef d++" ("++texOnly_Id (name d)++")" |d<-rds]
           )++
           (case (length nds,language flags) of
             (1,Dutch)   -> [Str " om eis",RawInline "latex" "~",RawInline "latex" $ symReqRef r, Str " (pg.",RawInline "latex" "~",RawInline "latex" $ symReqPageRef r, Str ") te formaliseren:"]
             (1,English) -> [Str " to formalize requirement",RawInline "latex" "~",RawInline "latex" $ symReqRef r, Str " (page",RawInline "latex" "~",RawInline "latex" $ symReqPageRef r, Str "):"]
             _           -> [Str ". "]
           )
        text3
         = case (language flags,isSignal r) of
            (Dutch  ,False) -> [Str "Dit betekent: "]
            (English,False) -> [Str "This means: "]
            (Dutch  ,True)  -> [Str "Activiteiten, die door deze regel zijn gedefinieerd, zijn afgerond zodra: "]
            (English,True)  -> [Str "Activities that are defined by this rule are finished when: "]
        text4
         = case language flags of
                 Dutch   -> [Str " Deze activiteiten worden opgestart door:"]
                 English -> [Str " These activities are signalled by:"]
        text5
         = case (language flags,isSignal r) of
             (Dutch  ,False) -> [Str "Dit komt overeen met eis",RawInline "latex" "~",RawInline "latex" $ symReqRef r, Str " op pg.",RawInline "latex" "~",RawInline "latex" $ symReqPageRef r, Str "."]
             (English,False) -> [Str "This corresponds to requirement",RawInline "latex" "~",RawInline "latex" $ symReqRef r, Str " on page",RawInline "latex" "~",RawInline "latex" $ symReqPageRef r, Str "."]
             (Dutch  ,True)  -> [ Str "Dit komt overeen met "
                                , Quoted  SingleQuote [Str (name r)]
                                , Str " (",RawInline "latex" $ symReqRef r, Str " op pg.",RawInline "latex" "~",RawInline "latex" $ symReqPageRef r, Str ")."]
             (English,True)  -> [Str "This corresponds to "
                                , Quoted  SingleQuote [Str (name r)]
                                , Str " (",RawInline "latex" $ symReqRef r, Str " op pg.",RawInline "latex" "~",RawInline "latex" $ symReqPageRef r, Str ")."]
        ncs = concs r >- seenConcs            -- newly seen concepts
        cds = [(c,cd) | c<-ncs, cd<-conceptDefs fSpec, cdcpt cd==name c]    -- ... and their definitions
        ds  = map makeDeclaration (mors r)
        nds = [d | d@(Sgn{})<-ds >- seenDeclarations]     -- newly seen declarations
        rds = [d | d@(Sgn{})<-ds `isc` seenDeclarations]  -- previously seen declarations
        ( dpNext, n', seenCs,  seenDs ) = dpR rs (n+length cds+length nds+1) (ncs++seenConcs) (nds++seenDeclarations)


data Counter = Counter { --getConc :: Int
                    --     getDecl :: Int
                    --   , getRule :: Int
                        getEisnr:: Int
                       }
newCounter :: Counter
newCounter = Counter 1
incEis :: Counter -> Counter
--incConc x = x{getConc = getConc x + 1}
--incDecl x = x{getDecl = getDecl x + 1}
--incRule x = x{getRule = getRule x + 1}
incEis x = x{getEisnr = getEisnr x + 1}

purposes2Blocks :: Options -> [Purpose] -> [Block]
purposes2Blocks flags ps
 = case ps of
    [] -> []
          -- by putting the ref after the first inline of the definition, it aligns nicely with the definition
    _  -> case concatMarkup [expl{amPandoc = insertAfterFirstInline (ref purp) $ amPandoc expl} | purp<-ps, let expl=explMarkup purp] of
           Nothing -> []
           Just p  -> amPandoc p
   where   -- The reference information, if available for this purpose, is put
    ref purp = case fspecFormat flags of
                FLatex | (not.null.explRefId) purp-> [RawInline "latex" ("\\marge{"++latexEscShw (explRefId purp)++"}\n")]
                _                                 -> []

-- Insert an inline after the first inline in the list of blocks, if possible. 
insertAfterFirstInline :: [Inline] -> [Block] -> [Block]
insertAfterFirstInline inlines (            Plain (inl:inls):pblocks)        =             Plain (inl : (inlines++inls)) : pblocks
insertAfterFirstInline inlines (            Para (inl:inls):pblocks)         =             Para (inl : (inlines++inls)) : pblocks
insertAfterFirstInline inlines (BlockQuote (Para (inl:inls):pblocks):blocks) = BlockQuote (Para (inl : (inlines++inls)) : pblocks):blocks
insertAfterFirstInline inlines blocks                                        = Plain inlines : blocks

isMissing :: Maybe Purpose -> Bool
isMissing mp =
  case mp of 
    Nothing -> True
    Just p  -> (not . explUserdefd) p

