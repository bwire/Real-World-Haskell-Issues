-- version with the safest signature
module GlobRegexSafe (globToRegex, matchesGlob) where

import Text.Regex.Posix((=~))

type GlobError = String

globToRegex :: String -> Either GlobError String
globToRegex text = performF globToRegex' text "^" "$"

performF :: (String -> Either GlobError String) -> String -> String -> String -> Either GlobError String
performF f text prfx sufx = case f text of
  Left error -> Left error
  Right result -> Right (prfx ++ result ++ sufx)
   
globToRegex' :: String -> Either GlobError String
globToRegex' "" = Right ""
globToRegex' ('*':cs) = performF globToRegex' cs ".*" ""
globToRegex' ('?':cs) = performF globToRegex' cs "." ""
globToRegex' ('[':'!':c:cs) = performF charClass cs ("[^" ++ [c]) ""
globToRegex' ('[':c:cs) = performF charClass cs ('[' : [c]) "" 
globToRegex' ('[':_) = Left "undeterminable character class"
globToRegex' (c:cs) = performF globToRegex' cs (escape c ) "" 

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"
    
charClass :: String -> Either GlobError String
charClass (']':cs) = performF globToRegex' cs "]" ""
charClass (c:cs) = performF charClass cs [c] ""
charClass [] = Left "unterminated character class"

matchesGlob :: FilePath -> String -> Either GlobError Bool
matchesGlob name pattern = 
  case globToRegex pattern of
    Right res -> Right (name =~ res)
    Left err -> Left err