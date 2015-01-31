module DNA where

import Control.Monad       
import Data.Set
import Text.Parsec       
import Text.Parsec.Text

data Nucleotide = Adenine | Thyamine | Guanine | Cytosine | Uracil
     deriving (Eq, Ord)

instance Show Nucleotide where
         show Adenine = "A"
         show Thyamine = "T"
         show Guanine = "G"
         show Cytosine = "C"
         show Uracil = "U"

type NucleicAcid = [Nucleotide]

newtype Codon = Codon (Nucleotide, Nucleotide, Nucleotide) deriving (Show,Eq,Ord)

rnaBases :: Set Nucleotide
rnaBases = fromList [Adenine, Guanine, Cytosine, Uracil]

dnaBases :: Set Nucleotide
dnaBases = fromList [Adenine, Guanine, Cytosine, Thyamine]         

nucleotide :: Char -> Nucleotide
nucleotide  c | c == 'A' = Adenine
              | c == 'T' = Thyamine
              | c == 'G' = Guanine
              | c == 'C' = Cytosine
              | c == 'U' = Uracil
              | otherwise = undefined

------------------Parsers-------------------
rnaNucleotide :: Parser Char
rnaNucleotide = oneOf . Prelude.concat . Prelude.map show $ toList rnaBases
              
rnaCodon :: Parser Codon              
rnaCodon = do
           c1:c2:c3:[] <- sequence $ replicate 3 rnaNucleotide'
           return $ Codon (c1, c2, c3)
                  where
                  rnaNucleotide' :: Parser Nucleotide
                  rnaNucleotide'  = liftM nucleotide rnaNucleotide

-- Parse an association "XXX Y"
relation :: Parser (Codon, String)   
relation = do
         -- A codon
         codon <- rnaCodon
         -- followed by a space
         _ <- char ' '
         -- last a single character abbreviating the amino acid,
         -- or the word 'Stop'.
         encodedValue <- stop <|> aminoAbreviation
         return (codon, encodedValue)
         where
                stop :: Parser String
                stop = string "Stop"
                aminoAbreviation :: Parser String
                aminoAbreviation = do
                                 charRep <- letter
                                 return (charRep:[])
         

                         
                       
                         

