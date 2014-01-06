-- | Utilities for manipulating windows of lists (eg. random subsequence of a list around some element)
module Window(slidingWindows) where
import System.Random(randomR,getStdGen,RandomGen,mkStdGen)

-- |Select a window of size around word
--
-- >>> selectWindow 1 ("abc",'d', "efg")
-- ('d',"d")
-- >>> selectWindow 2 ("abc",'d', "efg")
-- ('d',"ad")
-- >>> selectWindow 3 ("abc",'d', "efg")
-- ('d',"ade")
-- >>> selectWindow 4 ("abc",'d', "efg")
-- ('d',"bade")
-- >>> selectWindow 5 ("abc",'d', "efg")
-- ('d',"badfe")
-- >>> selectWindow 2 ("",'d', "efg")
-- ('d',"de")
selectWindow :: Int -> ([a],a,[a]) -> (a,[a])
selectWindow n (pref,word,suff) = (word, selectWindow' n (pref,word,suff) [] [])
  where
    selectWindow' 1 (pref,word,suff) prefs suffs = prefs ++ (word:suffs)                          
    selectWindow' n ([],word,[])     prefs suffs = prefs ++ (word:suffs)                          
    selectWindow' n ([],word,suff)   prefs suffs = selectWindow' (n-1) ([],word,tail suff) prefs (head suff :suffs)
    selectWindow' 2 (pref,word,suff) prefs suffs = selectWindow' 1     (tail pref,word,suff)      (head pref:prefs) suffs
    selectWindow' n (pref,word,suff) prefs suffs = selectWindow' (n-2) (tail pref,word,tail suff) (head pref:prefs) (head suff: suffs)
    

-- |Generate random sliding windows over a given sentence
--
-- >>> slidingWindows 4 (mkStdGen 0) "abcdefgh"
-- [('a',"adcb"),('b',"abdc"),('c',"bc"),('d',"cde"),('e',"e"),('f',"ef"),('g',"efgh"),('h',"gh")]
slidingWindows :: (RandomGen g) =>
                  Int       -- Maximum size of window
               -> g         -- random generator seed
               -> [a]       -- 'sentence' to generate windows from
               -> [(a,[a])] -- list of couple (word, window)
slidingWindows w g sentence = snd $ foldl (randomWindow w) (g,[]) (parts sentence)
  where
    randomWindow :: (RandomGen g) =>
                  Int
               -> (g, [(a,[a])])
               -> ([a],a,[a])
               -> (g, [(a,[a])])
    randomWindow w (g,ws) part =
      let (n,g') = randomR (1,w) g
      in  (g', selectWindow n part: ws)
  
-- |Iterate over a list, splitting it in 3 parts: prefix, element, suffix
--
-- >>> reverse $ parts [1,2,3,4]
-- [([],1,[2,3,4]),([1],2,[3,4]),([2,1],3,[4]),([3,2,1],4,[])]
parts :: [a] -> [([a],a,[a])]
parts xs = parts' xs []
  where
    parts' []     ps                   = ps
    parts' (x:xs) []                   = parts' xs [([],x,xs)]
    parts' (x:xs) p@((pref,v,suff):ps) = parts' xs ((v:pref,x,xs):p)
