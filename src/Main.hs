module Main where
       
import DNA
import Control.Applicative hiding (many)
import Control.Monad (liftM)
import Data.Map hiding (map)
import qualified Data.Text as T
import Text.Parsec


{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
       
-- Given: A RNA string 'S' correspoding to a strand of mRNA of at most 10kbp
-- Return: The protein string  encoded by 'S'

-- Folds over a Polypeptide, or a list of Nucleotides, replacing 
-- each codon with the protein it encodes for.  Each codon is 
-- looked up as it is encountered in the fold.  
translateRNA :: [(Codon,String)] -> [Nucleotide] -> Maybe String
translateRNA encodings target =
             let lookupTable = Data.Map.fromList encodings
                 -- The third pattern is included because at least 3 nucleotides are 
                 -- required to consititue a codon
                 translate :: Maybe String -> [Nucleotide] -> Maybe String
                 translate Nothing _ = Nothing
                 translate result [] = result
                 translate result (_:_:[]) =  result
                 translate result (n1:n2:n3:ns) = translate result' ns
                           where
                                target   = Codon (n1,n2,n3)
                                value    = Data.Map.lookup target lookupTable
                                result'  = (++) <$> value <*> result
             in Prelude.foldl translate (Just "") [target] -- This could cause a problem.

main :: IO ()          
main = do
       (codonTable', targetString') <- getUserInput
       codonTable <- return $ validateAndParseMap codonTable'
       targetString <- return $ validateAndParseRNA targetString'
       case codonTable of 
            (Left e) -> print e
            (Right encodings) -> do
                              case targetString of
                                   (Left e') -> print $ show e'
                                   (Right rnaString) -> report (translateRNA encodings rnaString)
       where
       getUserInput = do
                    i1:i2:is <- sequence . map (fmap T.pack) $ [readFile "codon_table.txt", getContents]
                    return (i1,i2)
       report (Just translation) = putStrLn . reverse $ translation
       report Nothing = putStrLn "Error: Translation failed."


-- Look at file:///Library/Haskell/ghc-7.8.3-x86_64/lib/mtl-2.1.3.1/doc/html/Control-Monad-Error.html
-- uses Control.Monad.Error

validateAndParseMap :: T.Text -> Either ParseError [(Codon,String)]
validateAndParseMap table = parse encodings "RNA Encoding Table" table


validateAndParseRNA :: T.Text -> Either ParseError [Nucleotide]
validateAndParseRNA target = parse rna "" target
                           where
                           rna = many (liftM nucleotide rnaNucleotide)
                
           

               
           
           
