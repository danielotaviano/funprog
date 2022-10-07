module Maybe where


import           Prelude hiding (Maybe (..))
data Maybe a = Nothing
             | Just a deriving(Show, Eq)

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
