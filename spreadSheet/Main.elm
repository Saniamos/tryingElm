module Main exposing (..)

import Html exposing (Html, program, div)

type Msg = None

type Cell
 = Empty
 | Label
 | Formula Expr


type Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Num Float
  | Ref Int Int

type alias Model = List (List Cell)

init : (Model, Cmd Msg)
init = ([ [ Empty ] ], Cmd.none) 

update : Msg -> Model -> Model
update msg model = model


view : Model -> Html Msg
view model = 
  div [] []

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

main : Program Never
main =
  program 
    { view = view
    , update = update
    , init = init
    , subsciptions = subsciptions
    }