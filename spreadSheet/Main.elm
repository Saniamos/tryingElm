module Main exposing (..)

import Html exposing (Html, program, div)

type Msg = None

type Cell
 = Empty
 | Label
 | Formula Expr

type alias Coor = (Int, Int)

type Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Num Float
  | Ref Coor


type alias Spreadsheet = 
  { lookup : Coor -> Cell
  , rows : Int 
  , cols : Int 
  }


type alias Model = Spreadsheet


emptyLookUp : Coor -> Cell
emptyLookUp _ = Empty

empty : Spreadsheet
empty =
  { lookup = emptyLookUp
  , rows = 0
  , cols = 0 
  }

addRow : Spreadsheet -> Spreadsheet
addRow sheet =
  { sheet | rows = rows + 1 }

addColumn : Spreadsheet -> Spreadsheet
addColumn sheet =
  { sheet | cols = cols + 1 }


set : Spreadsheet -> Coor -> Cell -> Spreadsheet
set sheet (x, y) val =
  case x < sheet.rows 
    |> ((&&) x > 0)
    

get : Spreadsheet -> Coor -> Cell



init : (Model, Cmd Msg)
init = ( empty , Cmd.none ) 

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