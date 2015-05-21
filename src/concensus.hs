{-# OPTIONS_GHC -XTypeSynonymInstances #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}
{-# OPTIONS_GHC -XOverlappingInstances #-}
    
type SymbolCount = (Char, Int)
instance Ord SymbolCount where
         compare (_, c1) (_, c2) = if c1 == c2 then EQ
                                     else if c1 <= c2 then LT
                                     else GT
     
type IndexedSymbolCounts = (Char, [Int])
instance Ord IndexedSymbolCounts where
         compare (sym, []) (sym', []) = EQ
         compare (sym, counts) (sym', counts') = compare (head counts) (head counts')

example :: [IndexedSymbolCounts]
example = [('A', [5,2]), ('T', [0,4]), ('G', [3,3])]


-- next provides the count of the remaining symbols.
-- ('A', [2]) = next ('A', [5,2])
next :: IndexedSymbolCounts -> IndexedSymbolCounts
next (sym, []) = (sym, [])
next (sym, counts) = (sym, tail $ counts)


concensus :: [IndexedSymbolCounts] -> String
concensus [] = []
concensus profile = (chooseMax profile):(concensus p')
          where
          chooseMax :: [IndexedSymbolCounts] -> Char
          chooseMax p = let symbol :: IndexedSymbolCounts -> Char
                            symbol s = fst s
                        in symbol $ maximum p
          p' :: [IndexedSymbolCounts]
          p' = map next profile
