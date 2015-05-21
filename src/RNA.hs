module DNA where
       
import Data.Set
import Text.Parsec

data Nucleotide = Adenine | Thyamine | Guanine | Cytosine | Uracil
     deriving (Eq, Ord)

-- Show     
instance Show Nucleotide where
         show Adenine = "A"
         show Thyamine = "T"
         show Guanine = "G"
         show Cytosine = "C"
         show Uracil = "U"

type NucleicAcid = [Nucleotide]

newtype Codon = Codon (Nucleotide, Nucleotide, Nucleotide)

rnaBases :: Set Nucleotide
rnaBases = Data.Set.fromList [Adenine, Guanine, Cytosine, Uracil]

dnaBases :: Set Nucleotide
dnaBases = Data.Set.fromList [Adenine, Guanine, Cytosine, Thyamine]         
        
rnaNucleotide :: Text.Parsec.Parser Nucleotide     
rnaNucleotide = oneOf . concat . Prelude.map show $ Data.Set.toList rnaBases

              
rnaCodon = do
           c1 <- rnaNucleotide
           c2 <- rnaNucleotide
           c3 <- rnaNucleotide
           return . Codon c1 c2 c3
           
