
countChar :: Char ->  String -> Int
countChar c s = length $ filter (\c' -> c' == c) s

--countChar' :: Char -> String -> Int          
--countChar' c s = length [ x | x <-s  x == c ]           
         
--Given a string S, and a list of symbols to look for [Syms], return a list of the occurences each symbol appears in the string [Occurs].
occurs :: String -> [Char] -> [(Char, Int)]
occurs = undefined
       
--bind countChar to every symbol in a list of characters
--map countChar "ATCG" :: [String -> Int]
      
--["AAAA", "TTTT"] >>= \s -> (map (countChar) "ATGC") >>= \f -> return f s
 
--["AAAA", "T"] >>= \s -> (map (\c -> (c ,countChar c)) "ATGC") >>= \f -> return (fst f,snd f $ s)
 

--For a set of symbols, alphabet, and a string
-- map the frequency each symbol in alphabet occurs onto the string.
type Alphabet = [Char]           
someF :: Alphabet -> String -> [(Char, Int)]
someF a s = map (\c -> (c, countChar c s)) a



        
--Want profile to have signature :
--       profile :: Alphabet -> [String] -> [[(Char, Int)]]
-- To do this, the current profile function needs to be generalized.
-- In what way?         
        
testString :: String
testString = "AGTCGCATGTA" --A3 G3 T3 C2           
