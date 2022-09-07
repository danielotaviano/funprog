module Av where

data Boolean = True' | False' deriving (Show)

lor :: Boolean -> Boolean -> Boolean
lor True' _ = True'
lor _ True' = True'
lor _ _ = False'

land :: Boolean -> Boolean -> Boolean
land True' True' = True'
land _ _ = False'
