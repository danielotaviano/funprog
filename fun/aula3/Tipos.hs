module Tipos where

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

--  deriving (Show, Eq)

instance (Eq Weekday) where
  Monday == Monday = True
  Tuesday == Tuesday = True
  Wednesday == Wednesday = True
  Thursday == Thursday = True
  Friday == Friday = True
  Saturday == Saturday = True
  Sunday == Sunday = True
  _ == _ = False

instance (Show Weekday) where
  show Monday = "Monday"
  show Tuesday = "Tuesday"
  show Wednesday = "Wednesday"
  show Thursday = "Thursday"
  show Friday = "Friday"
  show Saturday = "Saturday"
  show Sunday = "Sunday"

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
