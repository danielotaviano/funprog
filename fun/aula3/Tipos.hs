module Tipos where

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show)

nextDay :: Weekday -> Weekday
nextDay Monday = Tuesday
nextDay Tuesday = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday = Friday
nextDay Friday = Saturday
nextDay Saturday = Sunday
nextDay Sunday = Monday

nextWorkingDay :: Weekday -> Weekday
nextWorkingDay Friday = Monday
nextWorkingDay Saturday = Monday
nextWorkingDay day = nextDay day