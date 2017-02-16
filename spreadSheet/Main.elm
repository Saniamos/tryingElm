module Main exposing (..)

import Html exposing (Html, program, div, table, tbody, tr, td, text, button, input)
import Html.Attributes as Attributes exposing (attribute, style)
import Html.Events exposing (onClick, onDoubleClick, onInput)

import List

type Msg 
  = None
  | AddRow
  | AddCol
  | Reset
  | ChangeMode Mode
  | ChangeValue Coor String

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


type alias Spreadsheet = 
  { lookup : Coor -> Cell
  , rows : Int 
  , cols : Int 
  }

type Mode 
  = View
  | Edit

type alias Model = 
  { data : Spreadsheet
  , mode : Mode  
  }

emptyLookUp : Coor -> Cell
emptyLookUp _ = Empty

emptySheet : Spreadsheet
emptySheet =
  { lookup = emptyLookUp
  , rows = 2
  , cols = 2 
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


evalExpr : Expr -> Float
evalExpr e =
  case e of 
    Add a b -> 
      (evalExpr a) + (evalExpr b)
    Sub a b -> 
      (evalExpr a) - (evalExpr b)
    Mult a b -> 
      (evalExpr a) * (evalExpr b)
    Div a b -> 
      (evalExpr a) / (evalExpr b)
    Num a ->
      a

empty : Model
empty =
  { data = emptySheet
  , mode = View 
  }


init : ( Model, Cmd Msg )
init = ( empty , Cmd.none ) 

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let 
    sheet = model.data
  in 
    case msg of 
      AddRow -> 
        ( { model | data = addRow sheet }, Cmd.none )
      AddCol ->
        ( { model | data = addCol sheet }, Cmd.none )
      ChangeValue c s ->
        ( { model | data = set sheet c (Label s) }, Cmd.none )
      ChangeMode m ->
        ( { model | mode = m }, Cmd.none )
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
      evalExpr e
        |> toString
    Label s ->
      s

viewCell: Mode -> Coor -> Cell -> Html Msg
viewCell m coor c =
  case m of
    View ->
     td 
      [ style [ ("padding", "10px") ] 
      , onDoubleClick (ChangeMode Edit)
      ] 
      [ evalCell c
        |> text
      ] 
    Edit ->
      td 
      [ style [ ("padding", "10px") ] 
      , onDoubleClick (ChangeMode View)
      ] 
      [ input 
        [ onInput (ChangeValue coor)
        , evalCell c
          |> Attributes.value
        ]
        [] 
      ] 


viewCells : Model -> Int -> Html Msg
viewCells model x =
  let 
    s = model.data
    cols = List.range 1 s.cols
    getVal = 
      \y-> get s (x, y)
        |> viewCell model.mode (x, y)
  in
    tr [] (List.map getVal cols)

viewRows : Model -> Html Msg
viewRows model =
  let 
    s = model.data
    rows = List.range 1 s.rows
    buildCells = viewCells model
  in
    tbody [] (List.map buildCells rows)


toggleViewMode : Mode -> Mode
toggleViewMode m =
  case m of
    View -> Edit
    Edit -> View

view : Model -> Html Msg
view model =
  let 
    otherMode = toggleViewMode model.mode
  in
    div [] 
    [ button [ onClick (ChangeMode otherMode) ] [ toString otherMode |> text ]
    , button [ onClick AddRow ] [ text "Add row"]
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