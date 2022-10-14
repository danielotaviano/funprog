module ExMaybe where

import qualified Data.Maybe                    as M
-- Do not alter this import!
import           Prelude                 hiding ( Maybe(..)
                                                , maybe
                                                )

data Maybe a = Nothing | Just a
    deriving (Show, Eq, Ord)

catMaybes :: [Maybe a] -> [a]
catMaybes []              = []
catMaybes (Nothing  : xs) = catMaybes xs
catMaybes ((Just x) : xs) = x : catMaybes xs


fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust _        = error "Nothing"

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe d Nothing  = d

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

listToMaybe :: [a] -> Maybe a
listToMaybe = undefined

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe p (x : xs) | isJust result = fromJust result : mapMaybe p xs
                    | otherwise     = mapMaybe p xs
  where result = p x

maybe :: b -> (a -> b) -> Maybe a -> b
maybe d p Nothing  = d
maybe _ p (Just x) = p x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

tryToModifyWith :: [Maybe (a -> a)] -> [a] -> [a]
tryToModifyWith []              _        = []
tryToModifyWith _               []       = []
tryToModifyWith (Nothing  : fs) (x : xs) = tryToModifyWith fs xs
tryToModifyWith ((Just f) : fs) (x : xs) = f x : tryToModifyWith fs xs
