module Main where
import DNA

import qualified Data.Text as T

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
     
newtype AminoAcid = AminoAcid Char deriving (Eq, Show)

type Polypeptide = [AminoAcid]

type ProtienString = [AminoAcid]
     
data Encoding = Start | Stop | Encoding (Codon -> AminoAcid)

--type RNACodonTable = Map Codon AminoAcid

-- Given: A RNA string 'S' correspoding to a strand of mRNA of at most 10kbp
-- Return: The protein string  encoded by 'S'
encode :: [Nucleotide] -> ProtienString
encode = undefined

assocList :: a -> a
assocList = id

main :: IO ()          
main = do
       contents <- fmap T.pack getContents
       list <- return $ assocList contents
       print list

-- The encoding table is represented in a very easy to parse format


                
           

               
           
           
