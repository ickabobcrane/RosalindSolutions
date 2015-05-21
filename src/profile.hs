import Control.Applicative ((<$>))
import Data.List (transpose)
import qualified Data.Text as T
import DNA

import Text.Parsec
import Text.Parsec.Text

countChar :: Char ->  String -> Int
countChar c s = length $ filter (\c' -> c' == c) s

type FastaID = String
     
fastaRecord :: Parser (FastaID, [Char])
fastaRecord = do 
           -- Introduced by a line that begins with '>'
           _ <- char '>'
           -- The introduction is followed by a label line
           id' <- do { manyTill anyChar (try newline) }
           -- the rest of a Fasta
           dnaString <-do { many DNA.dnaNucleotide }
           return (id', dnaString)


type DNAString = String
profile :: [DNAString] -> [(Char, [Int])]
profile [] = []
profile dna = let
        columns :: [String]
        columns = Data.List.transpose dna
        emptyCounts :: [(Char, [Int])]
        emptyCounts = [('A',[]), ('T',[]), ('G',[]), ('C',[])]
        symbolCount :: [Char] -> [(Char,[Int])] -> [(Char,[Int])]
        symbolCount [] _ = []
        symbolCount  _ [] = []
        symbolCount col (s:symbols) = update s : symbolCount col symbols
                             where
                             update :: (Char, [Int]) -> (Char, [Int])
                             update (sym, counts) = let
                                                  newcnt = length $ filter (\c -> c == sym) col
                                                  in (sym, newcnt:counts)
        
        in foldr symbolCount emptyCounts columns
        
                                                        

        

-- main :: IO ()
-- main = do
--      records <- parse (fastaRecord `sepEndBy` spaces) "input" (T.pack getContents)
--      cols <-  Data.List.transpose . (map snd) <$> records
--      return ""
      
     


----------------- Tests and Test Data ----------------
s1 :: String
s1 = "ATCGCGAT"
   
s2 :: String  
s2 = "ATGCAGAC"

s3 :: String   
s3 = "TACGCGTT" 

ss :: [String]   
ss = [s1,s2,s3]

testInput = concat ss
          
