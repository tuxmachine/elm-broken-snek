import Browser
import Debug exposing (log)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List


main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Point =
  { x : Int
  , y : Int
  }

type alias PixelStyle =
  { background: String
  , width: String
  , height: String }

type alias Model = {
  snake: List Point
  }

init : Model
init =
  { snake = []}


-- UPDATE

type Direction = North | East | South | West

update : Direction -> Model -> Model
update direction model =
  case direction of
    North ->
      -- Bereken nieuw point
      -- Verplaats laatste element van snake naar nieuw point
      log "North"
      model

    East ->
      -- idem
      log "North"
      model

    South ->
      -- idem
      log "North"
      model

    West ->
      -- idem
      log "North"
      model


-- VIEW

view : Model -> Html Direction
view model =
  div [class "kak"] []
