-- refined version which imports refined module GlobRegexRefined with advanced error handling

module GlobRefined (namesMatching) where

import System.Directory(doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents)
import System.FilePath(dropTrailingPathSeparator, splitFileName, (</>))
import Control.Monad(forM, filterM)
import Control.Exception(handle, SomeException)
import GlobRegexRefined(matchesGlob)
import Data.List(isInfixOf)
  
namesMatching :: String -> IO [FilePath]
namesMatching pattern
  | not (isPattern pattern) = do
      exists <- doesNameExist pattern
      return (if exists then [pattern] else [])
  | otherwise = do
    let (d, baseName') = splitFileName pattern
    let isDeep = isDeepPattern baseName'
    let baseName = if isDeep 
                   then removeAsterisk baseName'
                   else baseName'
    case d of
      "" -> do
        curDir <- getCurrentDirectory
        dirs <- if isDeep then getDirList curDir else return [curDir]
        let listDir = listMatches
        getFiles listDir dirs baseName              
        
      dirName -> do
         dirs <- if isPattern dirName
                 then namesMatching $ dropTrailingPathSeparator dirName
                 else if isDeep then getDirList dirName else return [dirName]
         let listDir = if isPattern baseName
                       then listMatches
                       else listPlain
         getFiles listDir dirs baseName              

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")
         
doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
  fileExists <- doesFileExist name
  if fileExists
  then return True
  else doesDirectoryExist name
         
listMatches :: FilePath -> String -> IO [String]
listMatches dirName pattern = do
  dirName' <- if null dirName
              then getCurrentDirectory
              else return dirName
  handle ((\e  -> return []) :: SomeException -> IO [String]) $ do
    names <- getDirectoryContents dirName'
    let names' = if isHidden pattern
                 then filter isHidden names
                 else filter (not . isHidden) names
    return (filter (`matchesGlob'` pattern) names') where
      matchesGlob' name pattern = case matchesGlob name pattern of
                   Left _ -> False
                   Right res -> res
      
isHidden :: String -> Bool
isHidden ('.':_) = True
isHidden _       = False                 

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
  exists <- if null baseName
            then doesDirectoryExist dirName
            else doesNameExist (dirName </> baseName)
  return (if exists then [baseName] else [])
  
isDeepPattern :: String -> Bool
isDeepPattern = isInfixOf "**"

removeAsterisk :: String -> String
removeAsterisk [] = []
removeAsterisk ('*':xs) = '*' : case xs of
  '*':ys -> ys
  ys     -> ys
removeAsterisk (x:xs) = x : removeAsterisk xs

getDirList :: FilePath -> IO [FilePath]
getDirList dir = do
  contents <- getDirectoryContents dir
  all <- mapM (return . (dir </>)) $ filter (\f -> f /= "." && f /= "..") contents
  dirs <- filterM doesDirectoryExist all
  dirs' <- mapM getDirList dirs
  return (dir : concat dirs')

getFiles :: (FilePath -> String -> IO [String]) -> [FilePath] -> String -> IO [FilePath]
getFiles listDir dirs pattern = do  
  pathNames <- forM dirs $ \dir -> do
    baseNames <- listDir dir pattern
    return (map (dir </>) baseNames)
  return (concat pathNames)
