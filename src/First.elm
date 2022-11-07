module First exposing(..)

import Browser
import Html

main = Browser.sandbox {init=0,update,view=view}

update msg model =
  model

view model =
  Html.h1 [] [Html.text "My First Elm Program"];
