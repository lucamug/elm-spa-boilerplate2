module Element.Hack exposing (..)

import Element
import Html
import Html.Attributes


-- h1 : List (Html.Attribute msg) -> List (Html.Html msg) -> Element.Element msg


styleElement : String -> Element.Element msg
styleElement text =
    Element.html (Html.node "style" [] [ Html.text text ])


class : String -> Element.Attribute msg
class name =
    Element.attribute (Html.Attributes.class name)


style : List ( String, String ) -> Element.Attribute msg
style style =
    Element.attribute (Html.Attributes.style style)


h1 : List (Html.Attribute msg) -> List (Html.Html msg) -> Element.Element msg
h1 attributes children =
    Element.html <|
        Html.h1 attributes children


h2 : List (Html.Attribute msg) -> List (Html.Html msg) -> Element.Element msg
h2 attributes children =
    Element.html <|
        Html.h2 attributes children


h3 : List (Html.Attribute msg) -> List (Html.Html msg) -> Element.Element msg
h3 attributes children =
    Element.html <|
        Html.h3 attributes children


h4 : List (Html.Attribute msg) -> List (Html.Html msg) -> Element.Element msg
h4 attributes children =
    Element.html <|
        Html.h4 attributes children
