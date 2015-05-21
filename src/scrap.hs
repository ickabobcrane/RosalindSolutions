
import DNA
import Data.Map
import qualified Data.Text as T

lookupTable = Data.Map.fromList [("uuu", "P")]

f :: Maybe String -> 
f acc [] = acc  
f (Just acc) rs = f (Just (acc:value)) rest
         where
         next = take 3 rs
         rest = drop 3 rs
         value = Data.Map.lookup next lookupTable


       
         
