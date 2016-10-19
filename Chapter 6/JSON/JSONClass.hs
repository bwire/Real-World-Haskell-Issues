{-# LANGUAGE FlexibleInstances #-}

module JSONClass (JAry(fromJAry), toJAry) where

type JSONError = String

newtype JAry a = JAry { fromJAry :: [a] } deriving (Eq, Ord, Show)

toJAry :: [a] -> JAry a
toJAry = JAry

newtype JObj a = JObj { fromJObj :: [(String, a)] } deriving (Eq, Ord, Show)
  
data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue)
            | JArray (JAry JValue) deriving (Eq, Ord, Show)
            
class JSON a where
  toJValue :: a -> JValue
  fromJValue :: JValue -> Either JSONError a
  
instance JSON Bool where
  toJValue = JBool
  fromJValue (JBool v) = Right v
  fromJValue _ =  Left "Not a JSON Boolean"

instance JSON String where  
  toJValue = JString
  fromJValue (JString s) = Right s
  fromJValue _ = Left "Not a JSON String"

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _ = Left "not a JSON number"

instance JSON Int where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Integer where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Double where
    toJValue = JNumber
    fromJValue = doubleToJValue id
    
 -- helper functions for JValue newtypes
mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = 
  case mapEithers f xs of
    Left err -> Left err
    Right ys -> case f x of
      Left err -> Left err
      Right y -> Right (y:ys)
mapEithers _ _ = Right []

whenRight :: (b -> a) -> Either c b -> Either c a
whenRight f (Right v) = Right (f v)
whenRight _ (Left err) = Left err 

jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)
jaryFromJValue (JArray (JAry a)) = whenRight JAry (mapEithers fromJValue a)
jaryFromJValue _ = Left "Not a JSON Array"

instance (JSON a) => JSON (JAry a) where
  toJValue = JArray . JAry . map toJValue . fromJAry
  fromJValue = jaryFromJValue

getres :: JValue -> [Bool]
getres v = case fromJValue v of 
 Right (JAry a) -> a
 Left _ -> []
