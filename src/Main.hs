module Main where
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many)
       
-- RNA base nucleotides       
data RNABase = Adenine  | 
               Cytosine |
               Guanine  |
               Uracil deriving (Eq,Ord)
-- Show     
instance Show RNABase where
         show Adenine = "A"
         show Cytosine = "C"
         show Guanine = "G"
         show Uracil = "U"

-- Read
-- *From BOS's Real World Haskell, tyvm BOS.*
instance Read RNABase where 
-- readsPrec is the main function for parsing input
   readsPrec _ value = 
             tryParse [("A",Adenine), ("C",Cytosine), ("G",Guanine), ("U",Uracil)]
             where tryParse [] = [] -- If there is nothing left to try, fail
                   tryParse ((attempt,result):xs) = 
                            -- Compare the start of the string to the string to
                            -- be parse to the string we are looking for
                            if (take (length attempt) value) == attempt
                               -- We have a match, return result and remaining
                               -- input
                               then [(result, drop (length attempt) value)]
                               -- If we don't have a match, try the next pair
                               -- in the list of attempts
                               else tryParse xs


data Codon = Codon RNABase RNABase RNABase deriving (Eq,Ord)

instance Show Codon where
          show (Codon a b c) =  "Codon " ++ show a ++ show b ++ show c

data Protein = Stop | Protein Char deriving Eq

instance Show Protein where
         show Stop  = "Protein Stop"
         show (Protein c) = "Protein " ++ show c

 -- The RNA bases
rnaBase :: Parser RNABase
rnaBase = adenine <|> guanine <|> cytosine <|> uracil
        where 
              adenine  = char 'A' >> return Adenine
              guanine  = char 'G' >> return Guanine
              cytosine = char 'C' >> return Cytosine
              uracil   = char 'U' >> return Uracil        

 -- A codon is three RNA bases                   
codon :: Parser Codon
codon = count 3 rnaBase >>= (\bases -> return (Codon (bases !! 0) (bases !! 1) (bases !! 2)))



protein :: Parser Protein
protein =  stop <|> singleCharRepr <?> "Protein"
        where 
              stop = try $ string "Stop" >> return Stop
              singleCharRepr = try $ letter >>= (return . Protein)

 -- A relation is a pairing of a codon with the protein it encodes.
 -- ex.
 --      ("UAG", Stop)
 --      ("UUU", 'F')
relation :: Parser (Codon,Protein)
relation = codon >>= (\c -> count 1 space >> protein >>= (\p -> return (c,p))) <?> "a relation"

relations :: Parser [(Codon,Protein)]
relations = relation `sepEndBy` spaces

codon2Protein :: Parser [(Codon,Protein)]          
codon2Protein = relations >>= return

{- Our goal is to read a file into list of two tuples,
   and add the tuples to a key-value map. -}
main :: IO ()
main = do
        results <- parseFromFile codon2Protein "codon_table.txt"
        print results
     

     
aLineOfCodons :: String     
aLineOfCodons = "UUU F      CUU L      AUU I      GUU V"     

twoLinesOfCodons :: String              
twoLinesOfCodons = "UUU F      CUU L      AUU I      GUU V"
                 ++ "\n"
                 ++ "UCC S      CCC P      ACC T      GCC A"

                 
