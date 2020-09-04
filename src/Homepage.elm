module Homepage exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)


view model =
    div [ class "jumbotron" ]
        [ h1 [] [ text "Samantha Frohlich" ]
        , p []
            [ text "PhD Student" ]
        ]


main =
    view "dummy model"