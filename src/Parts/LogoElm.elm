module Parts.LogoElm
    exposing
        ( Color(..)
        , Type(..)
        , component
        , introspection
        )

-- import Introspection

import Element
import Svg
import Svg.Attributes


-- Original SVG: https://github.com/elm-lang/svg/blob/master/examples/Logo.elm


example : Element.Element msg -> Element.Element msg
example type_ =
    type_


introspection :
    { description : String
    , example : Element.Element msg -> Element.Element msg
    , name : String
    , signature : String
    , types : List ( Element.Element msg1, String )
    , usage : String
    , usageResult : Element.Element msg2
    }
introspection =
    { name = "Logo Elm"
    , signature = "Size -> Type -> Html.Html msg"
    , description = ""
    , usage = "colorOrange 128"
    , usageResult = orange 128
    , types = types
    , example = example
    }


types : List ( Element.Element msg, String )
types =
    [ ( orange 64, "orange" )
    , ( green 64, "green" )
    , ( lightBlue 64, "lightBlue" )
    , ( blue 64, "blue" )
    , ( white 64, "white" )
    , ( black 64, "black" )
    , ( colorful 64, "colorful" )
    ]


orange : Int -> Element.Element msg
orange size =
    component size (Color Orange)


green : Int -> Element.Element msg
green size =
    component size (Color Green)


lightBlue : Int -> Element.Element msg
lightBlue size =
    component size (Color LightBlue)


blue : Int -> Element.Element msg
blue size =
    component size (Color Blue)


white : Int -> Element.Element msg
white size =
    component size (Color White)


black : Int -> Element.Element msg
black size =
    component size (Color Black)


colorful : Int -> Element.Element msg
colorful size =
    component size Colorful


type Type
    = Color Color
    | Colorful


type alias Size =
    Int


type Color
    = Orange
    | Green
    | LightBlue
    | Blue
    | White
    | Black


ratio : Float
ratio =
    -- Width / Height
    1


cssRgb : Color -> String
cssRgb color =
    case color of
        Orange ->
            "#f0ad00"

        Green ->
            "#7fd13b"

        LightBlue ->
            "#60b5cc"

        Blue ->
            "#5a6378"

        White ->
            "#fff"

        Black ->
            "#000"


component : Int -> Type -> Element.Element msg
component height type_ =
    let
        f =
            Svg.Attributes.fill

        d =
            Svg.Attributes.d

        p =
            Svg.path

        c =
            case type_ of
                Colorful ->
                    { c1 = cssRgb Orange
                    , c2 = cssRgb Green
                    , c3 = cssRgb LightBlue
                    , c4 = cssRgb Blue
                    }

                Color c ->
                    { c1 = cssRgb c
                    , c2 = cssRgb c
                    , c3 = cssRgb c
                    , c4 = cssRgb c
                    }
    in
    Element.html
        (Svg.svg
            [ Svg.Attributes.version "1"
            , Svg.Attributes.viewBox "0 0 323 323"
            , Svg.Attributes.height <| toString height
            , Svg.Attributes.width <| toString <| floor <| toFloat height * ratio
            ]
            [ p [ f c.c1, d "M162 153l70-70H92zm94 94l67 67V179z" ] []
            , p [ f c.c2, d "M9 0l70 70h153L162 0zm238 85l77 76-77 77-76-77z" ] []
            , p [ f c.c3, d "M323 144V0H180zm-161 27L9 323h305z" ] []
            , p [ f c.c4, d "M153 162L0 9v305z" ] []
            ]
        )
