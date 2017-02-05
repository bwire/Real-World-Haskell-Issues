--3. Take the predicates and combinators from the section called “Gluing predicates together” and make them work with our new Info type.
import System.Directory(Permissions)
import Data.Time.Clock(UTCTime(..))
import System.FilePath (takeExtension)

data Info = Info {
  infoPath :: FilePath,
  infoPerms :: Maybe Permissions,
  infoSize :: Maybe Integer,
  infoModTime :: Maybe UTCTime
} deriving (Eq, Ord, Show)

-- type of a function which extracts data from Info record
type InfoP a = Info -> a

filePathP :: InfoP FilePath
filePathP = infoPath

sizeP :: InfoP Integer
sizeP info = case infoSize info of
  Just s -> s
  Nothing -> 0
  
equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \i -> f i == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k = \i -> f i `q` k

greaterP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)

lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
lesserP = liftP (<)

simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
simpleAndP l r = \i -> l i && r i

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q il ir = \i -> il i `q` ir i

andP, orP :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = liftP2 (&&)
orP = liftP2 (||)

constP :: a -> InfoP a
constP k _ = k

liftP' :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP' q f k = liftP2 q f (constP k)

liftPath :: (FilePath -> a) -> InfoP a
liftPath f = f . filePathP

myTest2 :: InfoP Bool       
myTest2 = (liftPath takeExtension `equalP` ".cpp") `andP` (sizeP `greaterP` 131072)

(==?) :: (Eq a) => InfoP a -> a -> InfoP Bool
(==?) = equalP

(&&?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
(&&?) = andP

(>?) :: (Ord a) => InfoP a -> a -> InfoP Bool
(>?) = greaterP

myTest3 :: InfoP Bool       
myTest3 = (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 131072)

infix 4 ==?
infix 3 &&?
infix 4 >?

myTest4 :: InfoP Bool       
myTest4 = liftPath takeExtension ==? ".cpp" &&? sizeP >? 131072




