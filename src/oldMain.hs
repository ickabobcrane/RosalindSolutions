module Main where
import qualified Data.Map.Strict as Map
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

-- A codon is a string of 3 nucleobases
data Codon = Codon RNABase RNABase RNABase deriving (Eq,Ord)

instance Show Codon where
          show (Codon a b c) =  "Codon " ++ show a ++ show b ++ show c

-- Amino Acids will be abreviated         
data AminoAcid = Stop | AminoAcid Char deriving Eq

-- Polypeptides are chains of AminoAcids
type Polypeptide = [AminoAcid] 

instance Show AminoAcid where
         show Stop  = "Stop"
         show (AminoAcid c) = show c
         
----Parser Definitions----
         
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
codon = count 3 rnaBase >>= (\(x:x1:x2:[]) -> return (Codon x x1 x2))

      
aminoAcid :: Parser AminoAcid
aminoAcid =  stop <|> singleCharRepr <?> "AminoAcid"
        where 
              stop = try $ string "Stop" >> return Stop
              singleCharRepr = try $ letter >>= (return . AminoAcid)

        
 -- A relation is a pairing of a codon with the aminoAcid it encodes.
 -- ex.
 --      ("UAG", Stop)
 --      ("UUU", 'F')
relation :: Parser (Codon,AminoAcid)
relation = codon >>= (\c -> count 1 space >> aminoAcid >>= (\p -> return (c,p))) <?> "a relation"

         
relations :: Parser [(Codon,AminoAcid)]
relations = relation `sepEndBy` spaces

          
codon2AminoAcid :: Parser [(Codon,AminoAcid)]          
codon2AminoAcid = relations >>= return


------End Parser Definitions-----



                
                  

-- create map
createMap ::[(Codon, AminoAcid)] -> Map.Map Codon AminoAcid
createMap (r:rs) = Map.fromList (r:rs)
createMap  []    = Map.empty

-- key lookup
translate :: Map.Map Codon AminoAcid -> [Codon] -> [Maybe AminoAcid]
translate _ [] = []                              
translate lookupTable (codon:cs) = value:(translate lookupTable cs)
                           where 
                           value = Map.lookup codon lookupTable
          
outputPolyPeptide :: Maybe Polypeptide -> IO ()
outputPolyPeptide Nothing = return ()
outputPolyPeptide (Just []) = return ()  
outputPolyPeptide (Just (a:as)) = outputAminoAcid a >> outputPolyPeptide (Just as)

readAndParseRnaFromFile :: FilePath -> IO String
readAndParseRnaFromFile filepath = undefined


                        
main = do
--         rna <- readAndParseRnaFromFile ""
         relationTable <- parseFromFile codon2AminoAcid "codon_table.txt"
         case relationTable of
            (Left e ) -> print $ show e 
            (Right t) -> do
                   maybePolypeptide <- return $  sequence (translate2Protein t sample) 
                   outputPolyPeptide $ maybePolypeptide
                   where
                   translate2Protein = translate . createMap

                   

outputAminoAcid :: AminoAcid -> IO ()
outputAminoAcid Stop = return ()
outputAminoAcid (AminoAcid c) = putChar c


sample ::[Codon]
sample = let result = parse (many codon) "" "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
         in case result of
         (Left _) -> []
         (Right codons) -> codons

aa :: Char -> AminoAcid 
aa c = AminoAcid c          


   
