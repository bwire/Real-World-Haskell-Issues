-- version without regex machinery
module GlobRegex2(matchesGlob2) where

matchesGlob2 :: FilePath -> String -> Bool
matchesGlob2 name "" = null name
matchesGlob2 (n:name) ('?':pattern) = matchesGlob2 name pattern 
matchesGlob2 name ('*':pattern) = matchStar name pattern
matchesGlob2 name ('[':pattern) = matchCharClass name pattern
matchesGlob2 (n:name) (p:pattern) = (n == p) && matchesGlob2 name pattern
matchesGlob2 "" _ = False

matchStar :: FilePath -> String -> Bool
matchStar "" _ = False
matchStar name pattern = matchesGlob2 name pattern || matchStar (tail name) pattern

matchCharClass :: FilePath -> String -> Bool
matchCharClass _ pat | not (']' `elem` pat) = error "Unmatched '['"
matchCharClass (n:name) ('!':pattern) = (n `notElem` (getCharClass pattern)) && matchesGlob2 name (afterCharClass pattern)
matchCharClass (n:name) pattern = (n `elem` (getCharClass pattern)) && matchesGlob2 name (afterCharClass pattern)

endPredicate :: Char -> Bool
endPredicate = (/=) ']'

getCharClass :: String -> String
getCharClass = takeWhile endPredicate

afterCharClass :: String -> String
afterCharClass = tail . dropWhile endPredicate
 