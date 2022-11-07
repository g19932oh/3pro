module ClickList exposing(..)

import Browser
import Svg exposing(..)
import Svg.Attributes exposing(..)
import Svg.Events exposing(..)

type Msg = Clicked Int

main = Browser.sandbox {init=init, update=update, view=view}

init = [{x=1,y=1,on=True}
       ,{x=2,y=1,on=True}
       ,{x=3,y=1,on=True}
       ,{x=1,y=2,on=True}
       ,{x=2,y=2,on=True}
       ,{x=3,y=2,on=True}
       ,{x=1,y=3,on=True}
       ,{x=2,y=3,on=True}
       ,{x=3,y=3,on=True}
       ]

update msg model =
    case msg of
        Clicked i -> toggle i model

toggle lid model =
    let
        l = Maybe.withDefault {x=0,y=0,on=True}
            (List.head  (List.drop lid model))
        c = l.x
        r = l.y
    in
        List.indexedMap (\i cell -> if i==lid then
                                        {cell | on = (not cell.on)}
                                    else if c==cell.x+1 && r==cell.y then
                                             {cell | on = (not cell.on)}
                                         else if c==cell.x-1 && r==cell.y then
                                                  {cell | on = (not cell.on)}
                                              else
                                                  cell
                        ) model

light lid lightdata =
    circle [cx (String.fromInt (100*lightdata.x))
           ,cy (String.fromInt (100*lightdata.y))
           ,r "50"
           ,fill (if lightdata.on then "yellow" else "gray")
           ,stroke "black"
           ,onClick (Clicked lid)
           ]
    []


view model  =
    svg [width "400"
        ,height "400"
        ]
        (List.indexedMap light model)
