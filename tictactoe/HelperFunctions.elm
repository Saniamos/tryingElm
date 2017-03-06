module HelperFunctions exposing (..)

import List

takeAtIndex : Int -> List a -> Maybe a
takeAtIndex i l = 
  List.take i l
    |> List.drop (i-1)
    |> List.head