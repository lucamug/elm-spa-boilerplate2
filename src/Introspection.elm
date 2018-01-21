module Introspection exposing (Introspection, view)

import Element
import Element.Hack
import Html exposing (..)


type alias Introspection msg a =
    { name : String
    , signature : String
    , description : String
    , usage : String
    , types : List ( a, String )
    , example : a -> Element.Element msg
    , usageResult : Element.Element msg
    }


view : Introspection msg a -> Element.Element msg
view introspection =
    Element.column []
        [ Element.Hack.h3 [] [ text introspection.name ]
        , Element.paragraph [] [ Element.text <| "part : " ++ introspection.signature ]
        , Element.column []
            [ Element.paragraph [] [ Element.text introspection.description ]
            , Element.Hack.h4 [] [ text "Example code" ]
            , Element.paragraph [] [ Element.text <| "part " ++ introspection.usage ]
            , Element.Hack.h4 [] [ text "Result" ]
            , Element.el [] introspection.usageResult
            , Element.Hack.h4 [] [ text ((toString <| List.length introspection.types) ++ " types of " ++ introspection.name) ]
            , Element.row []
                (List.map
                    (\( type_, name ) ->
                        Element.column []
                            [ Element.el [] <| introspection.example type_
                            , Element.el [] <| Element.text <| name
                            ]
                    )
                    introspection.types
                )
            ]
        ]
