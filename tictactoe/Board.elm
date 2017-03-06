module Board exposing (..)

import List exposing (List)
import Html

import HelperFunctions as Help

type alias Board a = List (List a)

type alias Coor = (Int, Int)

init : Int -> a -> Board a
init i a = 
  List.repeat i a
    |> List repeat i


get : Board a -> Coor -> Maybe a
get b (x, y) = 
  let 
    subx = Help.takeAtIndex x b
  in
    case subx of
      Just lx ->
        Help.takeAtIndex y lx
      _ ->
        Nothing

set : Board a -> Coor -> a -> Board a
set b (x, y) val =
  

view : Board a -> (a -> String) -> Html Msg
view b toStr =
