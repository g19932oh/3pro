module Test3d exposing (..)

import Angle
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Length
import Pixels
import Point3d
import Sphere3d
import Axis3d
import Scene3d
import Scene3d.Material as Material
import Viewpoint3d
import Browser
import Html exposing (Html)
import Time

main = Browser.element {init = init
                        ,update = update
                        ,view = view
                        ,subscriptions = subscriptions}

type alias Model= {angle: Float}
type Msg =Elapsed Time.Posix

init: () -> (Model, Cmd Msg)
init _ =
  (Model 0
  ,Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Elapsed t -> ({model|angle=modek.angle+1}, Cmd.none)

view: Model -> Html Msg
view model =
  let
      material =
        Material.nonmetal
            { baseColor = Color.lightBlue
            , roughness =0.4
            }

        sphere =
          Scene3d.sphere material <|
            Sphere3d.withRadius (Length.meters 1) Point3d.origin

        satellite =
          Scene3d.rotateAround Axis3d.x (Angle.degrees model.angle)
            Scene3d.sphere material <|
              Sphere3d.withRadius (Length.meters 0.1) (Point3d.meters 0 1.2 0)

        plane =
          Scene3d.quad material <|
            (Point3d.meters 1 1 0)
            (Point3d.meters -1 1 0)
            (Point3d.meters -1 -1 0)
            (Point3d.meters 1 -1 0)

        camera =
          Camera3d.perspective
              { viewpoint =
                   Viewpoint3d.lookAt
                       { focalPoint = Point3d.origin
                        , eyePoint = Point3d.meters 5 5 5
                        , upDirection = Direction3d.positiveZ
                       }
                , verticalFieldOfView = Angle.degrees 30
                }
  in

  Html.div[]
      [
      Scene3d.sunny
        { camera = camera
        , clipDepth = Length.centimeters 0.5
        , dimensions = ( Pixels.int 600, Pixels.int 600 )
        , background = Scene3d.transparentBackground
        , entities = [ sphere, plane, satellite ]

        , shadows = False

        , upDirection = Direction3d.z

        , sunlightDirection = Direction3d.yz (Angle.degrees -120)
        }
      ]

subscriptions: Model -> Sub Msg
subscriptions model =
  Time.every 1000 Elapsed
