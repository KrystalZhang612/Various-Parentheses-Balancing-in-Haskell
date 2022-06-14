module Main where 
import System.IO 
import Text.Printf

--declaring the looper string

matching = looperString []

--implementation for matching function 

  where
    
    --set the true cases--if the brackets balanced
    --this holds when the given string contains any literals 
        
    isBalanced '(' ')' = True
    isBalanced '{' '}' = True
    isBalanced '[' ']' = True
    isBalanced  _   _  = False

    --initialize an empty string str to loop 

    looperString str [] = null str
        
    looperString str (element:elements)
        
    --use prelude 'elem' to return True if the given string contains the matching brackets 

      | element `elem` "([{" = looperString (element:str) elements 
        
    -- mapping the matching brackets to determine overall balancing status 
        
      | element `elem` ")]}" = case str of
        
        open : str' | isBalanced open element -> looperString str' elements 
     
    --imbalanced, unmatching enclosed. return false 
        _ -> False 
     
     --use catch-all guard otherwise here to return all true at the end 
        
      | otherwise = looperString str elements 
        
--main/driver code
        
main :: IO ()

main = do 

--required testing cases 

print $ matching "abab" 

print $ matching "[a(b])" 

--extra testing cases for debugging

print $ matching "{[abc](c)}"

print $ matching "{[(ab)][{cccd}]{}cd}"

print $ matching "{[abcddaaa}]}"

print $ matching "{[{(}])}"

print $ matching "{[][][][{(})}]}ccccddadaa"

print $ matching "{[aabbaaa]}}a"

