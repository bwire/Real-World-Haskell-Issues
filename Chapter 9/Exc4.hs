import System.Directory(Permissions, getPermissions, getModificationTime, getDirectoryContents, searchable)
import Data.Time.Clock(UTCTime(..))
import System.FilePath((</>))
import Control.Exception(IOException(..), handle, bracket)
import Control.Monad(liftM, forM, mapM_)
import System.IO(IOMode(..), openFile, hClose, hFileSize)

data Info = Info {
  infoPath :: FilePath,
  infoPerms :: Maybe Permissions,
  infoSize :: Maybe Integer,
  infoModTime :: Maybe UTCTime
} deriving (Eq, Ord, Show)

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle ((\_ -> return Nothing) :: IOException -> IO (Maybe a)) (Just `liftM` act)

getUsefullContents :: FilePath -> IO [String]
getUsefullContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names) 
  
isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)

traverse :: ([Info] -> [Info]) -> FilePath -> IO ([Info])
traverse order path = do
  names <- getUsefullContents path
  contents <- mapM getInfo (path : map (path </>) names)
  liftM concat $ forM (order contents) $ \info -> do 
    if isDirectory info && infoPath info /= path 
    then traverse order (infoPath info)
    else return [info]

betterTraverse :: ([Info] -> [Info]) -> ([Info] -> [Info]) -> FilePath -> IO [Info]
betterTraverse pOrd pFilter path = return . pFilter =<< traverse pOrd path