module Parts.Button exposing (Type(..), component, introspection)

import Color
import Color.Convert
import Element
import Html
import Html.Attributes
import Introspection
import Parts.Color


introspection : Introspection.Introspection2 msg msg1 a
introspection =
    { name = "Buttons"
    , signature = "List (Html.Attribute msg) -> String -> Type -> Html.Html msg"
    , description = "Button accept a type, an Html.Attribute msg that can be attribute that return a messages, such as onClick, and a string that is used inside the button."
    , usage = """[] "I am a button" Large"""
    , usageResult = common small
    , types = types
    , example = identity
    }


common : (List a -> String -> b) -> b
common type_ =
    type_ [] "I am a button"


types : List ( Element.Element msg, String )
types =
    [ ( common small, "small" )
    , ( common smallImportant, "smallImportant" )
    , ( common large, "large" )
    , ( common largeImportant, "largeImportant" )
    , ( common largeWithSpinner, "largeWithSpinner" )
    , ( common largeImportantWithSpinner, "largeImportantWithSpinner" )
    ]


type Type
    = Small
    | SmallImportant
    | Large
    | LargeImportant
    | LargeWithSpinner
    | LargeImportantWithSpinner


type Color
    = Regular
    | Important
    | TextOnRegular
    | TextOnImportant


type Size
    = SmallSize
    | LargeSize


typeToColor : Color -> Color.Color
typeToColor color =
    case color of
        Regular ->
            Parts.Color.white

        Important ->
            Parts.Color.elmOrange

        TextOnRegular ->
            Parts.Color.fontColor

        TextOnImportant ->
            Parts.Color.white


sizeToString : Size -> String
sizeToString size =
    case size of
        SmallSize ->
            "32px"

        LargeSize ->
            "64px"


small : List (Html.Attribute msg) -> String -> Element.Element msg
small msgs string =
    component msgs string Small


smallImportant : List (Html.Attribute msg) -> String -> Element.Element msg
smallImportant msgs string =
    component msgs string SmallImportant


large : List (Html.Attribute msg) -> String -> Element.Element msg
large msgs string =
    component msgs string Large


largeImportant : List (Html.Attribute msg) -> String -> Element.Element msg
largeImportant msgs string =
    component msgs string LargeImportant


largeWithSpinner : List (Html.Attribute msg) -> String -> Element.Element msg
largeWithSpinner msgs string =
    component msgs string LargeWithSpinner


largeImportantWithSpinner : List (Html.Attribute msg) -> String -> Element.Element msg
largeImportantWithSpinner msgs string =
    component msgs string LargeImportantWithSpinner


component : List (Html.Attribute msg) -> String -> Type -> Element.Element msg
component msgs string type_ =
    let
        { size, bgColor, spinner } =
            case type_ of
                Small ->
                    { size = SmallSize
                    , bgColor = Regular
                    , spinner = False
                    }

                SmallImportant ->
                    { size = SmallSize
                    , bgColor = Important
                    , spinner = False
                    }

                Large ->
                    { size = LargeSize
                    , bgColor = Regular
                    , spinner = False
                    }

                LargeImportant ->
                    { size = LargeSize
                    , bgColor = Important
                    , spinner = False
                    }

                LargeWithSpinner ->
                    { size = LargeSize
                    , bgColor = Regular
                    , spinner = True
                    }

                LargeImportantWithSpinner ->
                    { size = LargeSize
                    , bgColor = Important
                    , spinner = True
                    }

        textColor =
            case bgColor of
                Regular ->
                    TextOnRegular

                _ ->
                    TextOnImportant
    in
    Element.html
        (Html.button
            ([ Html.Attributes.style
                [ ( "background-color", Color.Convert.colorToHex <| typeToColor bgColor )
                , ( "height", sizeToString size )
                , ( "color", Color.Convert.colorToHex <| typeToColor textColor )
                , ( "border-radius", "10px" )
                , ( "padding", "0 60px" )
                , ( "position", "relative" )
                , ( "transition", "all .3s" )
                , if spinner then
                    ( "cursor", "progress" )
                  else
                    ( "cursor", "pointer" )
                , if spinner then
                    ( "padding", "0 80px 0 40px" )
                  else
                    ( "padding", "0 60px" )
                ]
             ]
                ++ msgs
            )
            [ Html.node "style" [] [ Html.text "@keyframes spinner { to { transform: rotate(360deg);}}" ]
            , Html.text string
            , if spinner then
                -- This is a pure css spinner
                Html.div
                    [ Html.Attributes.style
                        [ ( "box-sizing", "border-box" )
                        , ( "position", "absolute" )
                        , ( "top", "50%" )
                        , ( "right", "24px" )
                        , ( "width", "20px" )
                        , ( "height", "20px" )
                        , ( "margin-top", "-10px" )
                        , ( "margin-left", "-10px" )
                        , ( "border-radius", "50%" )
                        , ( "border", "2px solid transparent" )

                        --, ( "border-top-color", typeToColor textColor )
                        , ( "animation", "spinner .6s linear infinite" )
                        ]
                    ]
                    [ Html.text "" ]
              else
                Html.text ""
            ]
        )
