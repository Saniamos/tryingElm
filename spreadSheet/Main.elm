module Main exposing (..)

import Html exposing (Html, program, div, table, tbody, tr, td, text, button)
import Html.Attributes exposing (attribute, style)
import Html.Events exposing (onClick)

import List

type Msg 
  = None
  | AddRow
  | AddCol
  | Reset

type Cell
 = Empty
 | Label String
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
  , rows = 5
  , cols = 5 
  }

addRow : Spreadsheet -> Spreadsheet
addRow sheet =
  { sheet | rows = sheet.rows + 1 }

addCol : Spreadsheet -> Spreadsheet
addCol sheet =
  { sheet | cols = sheet.cols + 1 }


{-
  takes the previous lookup function and a new key, value pair
  it technically implements a list where no items can be deletet but only overriden
  when used as lookup function it first looks if it holds the value to the requested key
  and returns either that value or passes the request on

  one could discuss if this is really the best way to do it, but i wanted to try this rather than have the same old list of lists
-}
addLookup : (Coor -> Cell) -> (Coor,  Cell) -> Coor -> Cell
addLookup preF (c, v) coor = 
  case coor == c of 
    True ->
      v
    False -> 
      preF coor

set : Spreadsheet -> Coor -> Cell -> Spreadsheet
set sheet (x, y) val =
  let 
    newCols = max sheet.cols x
    newRows = max sheet.rows y
    newLookup = addLookup sheet.lookup ( (x, y), val )
  in 
    { sheet | cols = newCols, rows = newRows, lookup = newLookup }
    

get : Spreadsheet -> Coor -> Cell
get sheet = sheet.lookup



init : ( Model, Cmd Msg )
init = ( empty , Cmd.none ) 

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  case msg of 
    AddRow -> 
      ( addRow model, Cmd.none )
    AddCol ->
      ( addCol model, Cmd.none )
    Reset -> 
      init
    None ->
      ( model, Cmd.none ) 


evalCell : Cell -> String
evalCell c =
  case c of
    Empty ->
      ""
    Formula e ->
      "Some Formula"
    Label s ->
      s


viewCells : Spreadsheet -> Int -> Html Msg
viewCells s x =
  let 
    cols = List.range 1 s.cols
    getVal = 
      \y-> td [ style [ ("padding", "10px") ] ] 
        [ get s (x, y)
          |> evalCell
          |> text
        ]
  in
    tr [] (List.map getVal cols)

viewRows : Spreadsheet -> Html Msg
viewRows s =
  let 
    rows = List.range 1 s.rows
    buildCells = viewCells s
  in
    tbody [] (List.map buildCells rows)


view : Model -> Html Msg
view model = 
  div [] 
  [ button [ onClick AddRow ] [ text "Add row"]
  , button [ onClick AddCol ] [ text "Add column"]
  , button [ onClick Reset ] [ text "Reset"]
  , table [ attribute "border" "1"] [ viewRows model ]
  ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

main : Program Never Model Msg
main =
  program 
    { view = view
    , update = update
    , init = init
    , subscriptions = subscriptions
    }