-- Modify foldTree to allow the caller to change the order of traversal of entries in a directory.

import System.FilePath((</>), takeFileName, takeExtension)
import ControlledVisit
import Data.Char (toLower)
import Data.List (sort)

data Iterate seed = Done { unwrap :: seed }
                  | Skip { unwrap :: seed }
                  | Continue { unwrap :: seed }
                    deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed                       

foldTree :: Iterator a -> a -> FilePath -> Bool -> IO a
foldTree iter initSeed path sortback = do
    endSeed <- fold initSeed path 
    return (unwrap endSeed) 
  where      
    fold seed subPath = getUsefullContents subPath >>= 
      if sortback
      then \pths -> walk seed (reverse $ sort pths)
      else \pths -> walk seed (sort pths)
    
    walk seed (name:names) = do
      let path' = path </> name
      info <- getInfo path'
      case iter seed info of
        done@(Done _) -> return done
        Skip seed' -> walk seed' names
        Continue seed' 
          | isDirectory info -> do
              next <- fold seed' path'
              case next of 
                done@(Done _) -> return done
                seed'' -> walk (unwrap seed'') names
          | otherwise -> walk seed' names
    walk seed _ = return (Continue seed)