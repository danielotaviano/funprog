module Av where

data Boolean = True' | False' deriving (Show)

lor :: Boolean -> Boolean -> Boolean
lor True' _ = True'
lor _     x = x

land :: Boolean -> Boolean -> Boolean
land True' True' = True'
land _     _     = False'
