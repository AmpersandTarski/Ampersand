>  module Typology
>         ( Inheritance(Isa), IsaPath, akin, kin, sub
>         , Typology(Typ), Typologic(typology), makeIsa, genEq
>         , compress, makeTrees, makeTypo) where
>  import Auxiliaries
>         ( rd, chain, eqCl )
>  import CommonClasses 
>         ( Collection(elems, isc, (>-)),eleM,empty,uni)
>  import Classification 
>         (Classification(Cl),root,makeClassifications)

Every concept has a typological sequence associated with it. For example, the
sequence "concept, thing, living thing, animal, elephant" is typological
sequence of "elephant". Concepts may have more than one typological sequence. The
sequence "concept, creature, grey creature, elephant" is an alternative sequence for
elephant.
That is why the result is of type [[IsaPath a]] rather than [IsaPath a].

>  data Inheritance a = Isa [(a,a)] [a] deriving Show

>  type IsaPath a = [a]

>  akin :: Eq a => IsaPath a -> IsaPath a -> Bool
>  xs `akin` ys
>   = head xs==head ys && last xs==last ys && (xs `sub` ys || ys `sub` xs)

>  kin :: Eq a => IsaPath a -> IsaPath a -> Bool
>  xs `kin` ys
>   = head xs==head ys && (xs `sub` ys || ys `sub` xs)

>  sub :: Eq a => IsaPath a -> IsaPath a -> Bool
>  (x:xs) `sub` (y:ys) = if x==y then xs `sub` ys else xs `sub` (y:ys)
>  xs     `sub`    ys  = null ys

>  data Typology a = Typ [IsaPath a] deriving Show

  instance (Eq a,Show a) => Show (Typology a) where
   showsPrec p
    = showString . chain "\n" . map show . makeTrees

An element deeper inside a typology is more specific,
whereas elements nearer to the root are called more general.

>  genEq :: Eq a => Typology a -> a->a->Bool
>  genEq (Typ world) l r = if l==r then True  else or[f l r ps|ps<-world]
>  spcEq :: Eq a => Typology a -> a->a->Bool
>  spcEq (Typ world) l r = if l==r then True  else or[f r l ps|ps<-world]
>  gen   :: Eq a => Typology a -> a->a->Bool
>  gen   (Typ world) l r = if l==r then False else or[f l r ps|ps<-world]
>  spc   :: Eq a => Typology a -> a->a->Bool
>  spc   (Typ world) l r = if l==r then False else or[f r l ps|ps<-world]

>  f :: Eq a => a -> a -> [a] -> Bool
>  f l r ps = not (null p2)
>   where
>    p1 = dropWhile (/=l) ps
>    p2 = dropWhile (/=r) p1

>  makeIsa :: Eq a => Typology a -> Inheritance a
>  makeIsa (Typ paths)
>    = Isa (rd [(a,b)|path<-paths, length path>1, (a,b)<-zip path (tail path)])
>          (rd [c| [c]<-paths])

The function typology creates all typological sequences. For example:

   typology (Isa [(1,178),(1,10),(1,12),(10,178)] [4,5])
=
   Typ [[1,10,178],[1,10],[1,12],[10,178],[4],[5],[1],[10],[178],[12]] :: Typology Integer 

>  class Eq a => Typologic a where
>   match    :: a -> a -> Bool
>   match       = (==)
>   mul      :: [a] -> [a] -> [a]
>   xs `mul` ys = xs++tail ys

 mul must satisfy:
    p `mul` p' == pth  implies that length p+length p' == 1+length pth

>   typology :: Show a => Inheritance a -> Typology a
>   typology (Isa rs cs)
>    = Typ (compress akin [p|p<-f 1 [] (singles rs) []]
>           ++ [[c]| c<-elems (Isa rs cs)])
>      where
>       singles rs         = [[a,b] | (a,b)<-rs, a/=b]
>       f n paths [] []    = paths
>       f n paths new []   = f (2*n) pn cands [p|p<-cands, head p `elem` tail p]
>                            where pn = paths++new
>                                  cands = [p|p<-pn `combine` pn,n<length p-1]
>       f n paths new errs = error("cyclic specialization\n " ++ (chain "\n ".map show) [head e:takeWhile (/=head e) (tail e)| e<-errs])
>       combine rs rs'     = [r `mul` r'| r<-rs, r'<-rs', last r `match` head r']

Todo: try this, to get rid of combine
        f n paths new  = f (2*n) pn [p `mul` p'| p<-pn, p'<-pn, last p==head p',n<length p+length p'-2]

Todo: check the following proof
Comment
    let len p = length p-1, being the number of ISA links of IsaPath p
     pre:     all [ 0<=len p*2<=n | p<-paths] && all [ n<len p*2<=2*n | p<-new]
           =>
     (1)      all [ 0<=len p*2<=2*n | p<-paths++new]
           =>                                               { let pn=paths++new }
     (2)      all [ 0<=len p*2<=4*n | p<-pn `combine` pn]
           => 
     (3)      all [ 2*n<len p*2<=4*n | p<-pn `combine` pn, n<len p]
           =>                          { let  new' = [p|p<-pn `combine` pn,n<len p]
                                         let  n' = 2*n                          }
     post:    all [ 0<=len p*2<=n' | p<-pn] && all [ n'<len p*2<=2*n' | p<-new']


>  compress :: Eq a => (IsaPath a->IsaPath a->Bool) -> [IsaPath a] -> [IsaPath a]
>  compress f paths
>   = eqCl paths
>     where
>      eqCl [] = []
>      eqCl (x:xs)
>       = l : eqCl [e|e<-[x|x<-xs,x/=l], not (f l e)]
>         where
>          l = longest x xs
>      longest l []     = l
>      longest l (x:xs) | f x l     = longest x xs
>                       | otherwise = longest l xs

  uniT :: Eq a => Typology a -> Typology a -> Typology a
  uniI :: Eq a => Inheritance a -> Inheritance a -> Inheritance a

>  instance Collection Inheritance where
>   e `eleM` isa              = e `elem` (elems isa)
>   empty                     = Isa [] []
>   elems (Isa ts es)         = rd ([a|(a,b)<-ts]++[b|(a,b)<-ts]++es)
>   Isa rx cx `uni` Isa ry cy = Isa (rd (rx++ry)) (rd (cx++cy))
>   Isa rx cx `isc` Isa ry cy = Isa (rx `isc` ry) (cx `isc` cy)
>   Isa rx cx >- Isa ry cy = Isa (rx>-ry) (cx>-cy)

>  instance Collection Typology where
>   eleM e (Typ pths) = or [e `elem` p| p<-pths]
>   empty = Typ []
>   elems (Typ paths) = rd (concat paths)
>   Typ px `uni` Typ py = Typ (compress akin (px++py++[init r++r'| r<-px, r'<-py, last r==head r']))
>   Typ pxs `isc` Typ pys = error "intersection of Typology not yet implemented"
>   Typ pxs >- Typ pys = error "set difference of Typology not yet implemented"

(makeTrees maakt gebruik van een polynomiaal algoritme, en is voor praktische bomen te traag.)

>  makeTrees :: Eq a => Typology a -> [Classification a]
>  makeTrees (Typ []) = []
>  makeTrees (Typ paths)
>   = [ Cl (head (head pp)) (makeTrees (Typ [tail p| p<-pp, length p>1]))
>     | pp<-eqCl head (compress sub paths)]

Het maken van bomen via makeTrees en via makeClassifications moet op hetzelfde neerkomen.
Dit is voor vrij grote bomen getest, maar nog niet bewezen. Een tegenvoorbeeld is
nog niet gevonden. Dit testen gebeurt met een lijstje tweetallen, die "basic" is.
Uit de volgende toets moet True komen, voor elk lijstje van tweetallen

>  check :: (Show i, Typologic i) => Inheritance i -> Bool
>  check (Isa zaken cs)
>     = not (basic zaken) ||
>        and [ cl==cl'| cl<-makeTrees (typology (Isa zaken []))
>                     , cl'<-makeClassifications zaken
>                     , root cl==root cl']
>  basic :: Eq a => [(a,a)] -> Bool
>  basic zaken = null [(a,b)|(a,b)<-zaken, not (null[c| (a',c)<-zaken, a==a', (c',b')<-zaken, b==b', c==c'])]

De lijst
    [("beest", "aap"), ("beest","zoogdier"), ("zoogdier", "aap")]
is niet basic, want ("beest", "aap") kan weg.


>  makeTypo :: Eq a => [Classification a] -> Typology a
>  makeTypo trees = Typ (paths trees)
>   where paths []    = [[]]
>         paths trees = [r: path| Cl r cls<-trees, path<-paths cls]

The following stuff is for testing and not for export.

>  instance Typologic Int

>  tt :: Typology Int
>  tt = (makeTypo . makeTrees . typology) (Isa [(1,178),(1,10),(1,12),(10,178)] [3])

try for instance:   makeTrees (typology tmul (Isa [(1,178),(1,10),(1,12),(10,178)] []))
should produce:
[1
  10
    178
  12] :: [Classification Integer] 

>  t1 = typology (Isa [(1,2),(1,3),(1,4),(3,2)] []) :: Typology Int
>  t2 = typology (Isa [(5,6),(2,5),(5,7),(5,8),(9,5)] []) :: Typology Int
>  t3 = typology (Isa [(5,6),(3,5),(3,9),(2,6),(5,7),(5,8),(9,5)] []) :: Typology Int

>  trees :: (Eq a, Show a) => Typology a -> IO ()
>  trees = putStr . chain "\n" . map show . makeTrees

