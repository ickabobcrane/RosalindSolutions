module Main where
       
import Data.Set
import Data.Map

import Text.Parsec
import Text.Parsec.String       

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
    
class Monomer a where 
      chain :: a -> [a] -> [a]

data Nucleotide = Adenine | Thyamine | Guanine | Cytosine | Uracil
     deriving (Eq, Ord, Show)


rnaBases :: Set Nucleotide
rnaBases = Data.Set.fromList [Adenine, Guanine, Cytosine, Uracil]

dnaBases :: Set Nucleotide
dnaBases = Data.Set.fromList [Adenine, Guanine, Cytosine, Thyamine] 

type NucleicAcid = [Nucleotide]

newtype Codon = Codon (Nucleotide, Nucleotide, Nucleotide)
     
newtype AminoAcid = AminoAcid Char deriving (Eq, Show)

type Polypeptide = [AminoAcid]

type ProtienString = [AminoAcid]
     
data Encoding = Start | Stop | Encoding (Codon -> AminoAcid)

type RNACodonTable = Map Codon AminoAcid

-- Given: A RNA string 'S' correspoding to a strand of mRNA of at most 10kbp
-- Return: The protein string  encoded by 'S'
encode :: [Nucleotide] -> ProtienString
encode = undefined

buildAssocList = undefined
               
main = do
       contents <- getContents
       list <- return . buildAssocList $  contents
       print list
       
