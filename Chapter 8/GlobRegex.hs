module GlobRegex(globToRegex, matchesGlob) where

import Text.Regex.Posix((=~))
import Data.Char(isUpper, toUpper, toLower)

globToRegex :: String -> Bool -> String
globToRegex text cs = '^' : globToRegex' text cs ++ "$"

globToRegex' :: String -> Bool -> String
globToRegex' "" _ = ""
globToRegex' ('*':cs) sns = ".*" ++ globToRegex' cs sns
globToRegex' ('?':cs) sns = '.' : globToRegex' cs sns
globToRegex' ('[':'!':c:cs) sns = "[^" ++ c : charClass cs sns
globToRegex' ('[':c:cs) sns = '[' : c : charClass cs sns
globToRegex' ('[':_) _ = error "undeterminable character class"
globToRegex' (c:cx) sns = escape c sns ++ globToRegex' cx sns

escape :: Char -> Bool -> String
escape c sns
    | c `elem` regexChars = '\\' : [c]
    | otherwise = findCase c sns
  where 
    regexChars = "\\+()^$.{}]|"
    findCase chr sns = case sns of
      True -> [chr]
      False -> '[' : chr : (switchCase chr) : "]"
        where switchCase c | isUpper c = toLower c 
                           | otherwise = toUpper c

charClass :: String -> Bool -> String
charClass (']':cs) sns = ']' : globToRegex' cs sns
charClass (c:cs) sns = c : charClass cs sns
charClass [] _ = error "unterminated character class"

matchesGlob :: FilePath -> String -> Bool -> Bool
matchesGlob name pattern cs = name =~ globToRegex pattern cs
