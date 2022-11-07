module May25 exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Html exposing (Html)
import Browser

main = Browser.sandbox { init = init, view = view, update = update }

type alias Model = {config: Config, space: Piece}
type alias Config = List Piece
type alias Piece = {x:Int, y:Int, n:Int}

type Msg = Slide Piece

size = 4

init: Model
init = {config= Debug.log "" startConfig
        ,space={x=size-1,y=size-1,n=0}}

startConfig: Config
startConfig =
  List.map (\i -> {x=modBy size i, y=i//size, n=i+1})
  (List.range 0 (size^2-2))

update: Msg -> Model -> Model
update msg model =
  case msg of
    Slide piece -> slide piece model

adjacent: Piece -> Piece -> Bool
adjacent p q =
  ((abs (p.x-q.x)) + (abs (p.y-q.y)))==1

slide: Piece -> Model -> Model
slide piece model =
  if adjacent piece model.space then
    {config= List.map (\p-> if p.x==piece.x && p.y==piece.y then
                              {p|x=model.space.x, y=model.space.y}
                              else
                                p
                              ) model.config
    ,space={x=piece.x, y=piece.y, n=0}}
  else
    model

unit = 50
pieceView: Piece -> Svg Msg
pieceView piece =
  g[onClick (Slide piece)
    ,transform ("translate("++(String.fromInt (unit*piece.x))
      ++ "," ++ (String.fromInt (unit*piece.y))++")")]
    [
    rect[
        fill "skyblue"
        ,stroke "black"
        ,width (String.fromInt unit)
        ,height (String.fromInt unit)]
        []
    ,text_[x (String.fromInt (unit//2))
          ,y (String.fromInt (unit//2))][text (String.fromInt piece.n)]
      ]

confView: Config -> List (Svg Msg)
confView config =
  List.map pieceView config

view: Model -> Html Msg
view model =
  Html.div []
  [
  svg [width "400"
      ,height "400"]
      (confView model.config)
  ]
