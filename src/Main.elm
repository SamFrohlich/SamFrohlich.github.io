module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view model =
    div [ class "jumbotron" ]
        [ h1 [] [ text "Sam Frohlich" ]
        , p []
            [ text "PhD Student" ]
        ]


main =
    view "dummy model"