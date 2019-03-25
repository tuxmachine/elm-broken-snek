import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, keyCode, onInput)
import Json.Decode as Json
import Time
import List exposing (take, any)
import Random

-- MAIN

main =
  Browser.element {
    init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

size = 10

-- MODEL

type alias Point = { x: Int, y: Int }
type alias Model =
  { snake : Point
    , tail: List Point
    , direction : String
    , score: Int
    , snack: Point
  }

init : () -> (Model, Cmd Msg)
init _ = (
  { snake = { x = 0, y = 0 }
  , direction = "start"
  , tail = []
  , score = 0
  , snack = {x = 2, y = 2}
  }, Cmd.none)

-- UPDATE

type Msg = Keydown Int | Tick Time.Posix | Score(Int, Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Keydown key ->
      case key of
        38 ->
          ({ model | direction = "up" }, Cmd.none)
        40 ->
          ({ model | direction = "down" }, Cmd.none)
        37 ->
          ({ model | direction = "left" }, Cmd.none)
        39 ->
          ({ model | direction = "right" }, Cmd.none)
        32 ->
          ({ model | score = 5 }, Cmd.none)
        _ ->
          (model, Cmd.none)
    Tick time ->
      let snake = model.snake in
      ({ model | snake = (updateSnake model), tail = (take (model.score + 1) (snake :: model.tail)) }, updateSnack model)
    Score (x, y) ->
      let score = model.score + 1 in
      ({ model | snack = { x = x, y = y }, score = score }, Cmd.none)

updateSnake: Model -> Point
updateSnake model =
  case model.direction of
    "up" ->
      let y = minmax model.snake.y -1
          snake = model.snake
      in
      { snake | y = y }
    "down" ->
      let y = minmax model.snake.y 1
          snake = model.snake
      in
      { snake | y = y }
    "left" ->
      let x = minmax model.snake.x -1
          snake = model.snake
      in
      { snake | x = x }
    "right" ->
      let x = minmax model.snake.x 1
          snake = model.snake
      in
      { snake | x = x }
    _ ->
      let snake = model.snake in snake

randomPoint : Random.Generator (Int, Int)
randomPoint =
    Random.pair (Random.int 0 size) (Random.int 0 size)
updateSnack: Model -> Cmd Msg
updateSnack model =
  if model.snack.x == model.snake.x && model.snack.y == model.snake.y then
    Random.generate Score randomPoint
  else
    Cmd.none


minmax base change =
  if base == 0 && change < 0 then
     size
  else if base == size && change > 0 then
     0
  else
    base + change

-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model =
  Time.every (toFloat (1000 - (model.score * 100))) Tick

-- VIEW

stylesheet : String
stylesheet =
  """
table {
  border-collapse: collapse;
  text-align: center;
}
table td {
  border: solid 2px white;
  background: #eee;
  width: 40px;
  height: 40px;
  color: white;
}
td.snake {
  background-color: black;
}
td.tail {
  background-color: #111;
}
td.snack {
  background-color: red;
}
input {
  outline: 0;
  border: 0;
  position: absolute;
  top: 0;
  left: 0;
  background: transparent;
  width: 100%;
  cursor: default;
  height: 100%;
  z-index: 999;
}
body {
  overflow: hidden;
}
  """

onKeyDown: (Int -> msg) -> Attribute msg
onKeyDown tagger =
  on "keydown" (Json.map tagger keyCode)

cellClass: Model -> Point -> String
cellClass model cell =
  if model.snake.x == cell.x && model.snake.y == cell.y then
    "snake"
  else if model.snack.x == cell.x && model.snack.y == cell.y then
    "snack"
  else if inTail model.tail cell then
    "tail"
  else
    ""

inTail: (List Point) -> Point -> Bool
inTail list point =
    List.any (\p -> p.x == point.x && p.y == point.y) list

createRows: Model -> List (Html Msg)
createRows model =
  List.indexedMap (createRow model) (List.repeat (size + 1) 0)

createRow: Model -> Int -> Int -> Html Msg
createRow model row _ =
  tr [] (List.indexedMap (createCell model row) (List.repeat (size + 1) 0))

createCell: Model -> Int -> Int -> Int -> Html Msg
createCell model row index _ =
  td [ class (cellClass model { x = index, y = row }) ] []

view : Model -> Html Msg
view model =
  div []
  [
    node "style" [] [ text stylesheet ]
  , input [onKeyDown Keydown, autofocus True] []
  , table [] (createRows model)
  ]
