module Parts.Button
    exposing
        ( introspection
        , large
        , largeImportant
        , largeImportantWithSpinner
        , largeWithSpinner
        , small
        , smallImportant
        )

--import Color.Convert
--import Html
--import Html.Attributes
--import Introspection

import Color
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Hack
import Element.Input
import Introspection
import Parts.Color


introspection : Introspection.Introspection2 msg
introspection =
    { name = "Button"
    , signature = "List (Html.Attribute msg) -> String -> Element.Element msg"
    , description = "Button accept a type, an Html.Attribute msg that can be attribute that return a messages, such as onClick, and a string that is used inside the button."
    , usage = "small [] \"I am a button\""
    , usageResult = common small
    , types = types
    , example = identity
    }


common :
    (List (Element.Attribute msg)
     -> Element.Element msg
     -> Maybe msg
     -> Element.Element msg
    )
    -> Element.Element msg
common type_ =
    type_ [] (Element.text "I am a button") Nothing


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


sizeToInt : Size -> Int
sizeToInt size =
    case size of
        SmallSize ->
            32

        LargeSize ->
            64


small : List (Element.Attribute msg) -> Element.Element msg -> Maybe msg -> Element.Element msg
small attributes label onPress =
    component attributes label onPress Small


smallImportant : List (Element.Attribute msg) -> Element.Element msg -> Maybe msg -> Element.Element msg
smallImportant attributes label onPress =
    component attributes label onPress SmallImportant


large : List (Element.Attribute msg) -> Element.Element msg -> Maybe msg -> Element.Element msg
large attributes label onPress =
    component attributes label onPress Large


largeImportant : List (Element.Attribute msg) -> Element.Element msg -> Maybe msg -> Element.Element msg
largeImportant attributes label onPress =
    component attributes label onPress LargeImportant


largeWithSpinner : List (Element.Attribute msg) -> Element.Element msg -> Maybe msg -> Element.Element msg
largeWithSpinner attributes label onPress =
    component attributes label onPress LargeWithSpinner


largeImportantWithSpinner : List (Element.Attribute msg) -> Element.Element msg -> Maybe msg -> Element.Element msg
largeImportantWithSpinner attributes label onPress =
    component attributes label onPress LargeImportantWithSpinner


component : List (Element.Attribute msg) -> Element.Element msg -> Maybe msg -> Type -> Element.Element msg
component attributes label onPress type_ =
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
    Element.Input.button
        ([ Element.Background.color <| typeToColor bgColor
         , Element.Font.color <| typeToColor textColor
         , Element.height <| Element.px <| sizeToInt size
         , Element.Border.rounded 10
         , Element.Border.width 1
         , Element.Border.color <| typeToColor textColor
         , Element.Hack.style [ ( "transition", "all .3s" ) ]
         , Element.centerY
         , if spinner then
            Element.Hack.style [ ( "cursor", "progress" ) ]
           else
            Element.Hack.style [ ( "cursor", "pointer" ) ]
         , if spinner then
            Element.paddingEach { bottom = 0, left = 40, right = 80, top = 0 }
           else
            Element.paddingXY 60 0
         ]
            ++ attributes
        )
        { onPress = onPress
        , label =
            if spinner then
                Element.paragraph []
                    [ Element.el
                        [ Element.paddingEach
                            { bottom = 0
                            , left = 0
                            , right = 0
                            , top = 0
                            }
                        ]
                        label
                    , Element.Hack.styleElement "@keyframes spinner { to { transform: rotate(360deg); } }"
                    , Element.el
                        [ Element.Hack.style
                            [ ( "animation", "spinner .6s linear infinite" )
                            ]
                        ]
                      <|
                        Element.text "↻"
                    ]
            else
                label
        }



{- , if spinner then
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
-}
