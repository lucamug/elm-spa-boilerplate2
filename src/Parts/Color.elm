module Parts.Color
    exposing
        ( black
        , elmOrange
        , fontColor
        , introspection
        , lightOrange
        , red
        , white
        )

import Color
import Element
import Element.Background
import Introspection


example : Color.Color -> Element.Element msg
example type_ =
    Element.el
        [ Element.Background.color type_

        -- Here I can convert the color to string
        ]
    <|
        Element.text "????"


introspection : Introspection.Introspection msg Color.Color
introspection =
    { name = "Colors"
    , signature = "Color.Color"
    , description = "List of colors used in the app."
    , usage = "elmOrange"
    , usageResult = example elmOrange
    , types = types
    , example = example
    }


types : List ( Color.Color, String )
types =
    [ ( elmOrange, "elmOrange" )
    , ( lightOrange, "lightOrange" )
    , ( fontColor, "fontColor" )
    , ( black, "black" )
    , ( white, "white" )
    , ( red, "red" )
    ]


elmOrange : Color.Color
elmOrange =
    Color.rgb 0xF0 0xAD 0x00


lightOrange : Color.Color
lightOrange =
    Color.rgb 0xF8 0xCA 0x83


fontColor : Color.Color
fontColor =
    Color.rgb 0x44 0x44 0x44


black : Color.Color
black =
    Color.rgb 0x00 0x00 0x00


white : Color.Color
white =
    Color.rgb 0xFF 0xFF 0xFF


red : Color.Color
red =
    Color.rgb 0xDD 0x00 0x00
