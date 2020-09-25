module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

-- -----------------------------------------------------------------------------
-- Main
-- -----------------------------------------------------------------------------

main : Program () Details Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }

-- -----------------------------------------------------------------------------
-- Model
-- -----------------------------------------------------------------------------

type alias Details =
    { name : String
    , position : String
    , institution : String
    , interest : String
    }

init : flags -> (Details, Cmd Msg)
init _ = ( Details "Sam Frohlich"
                "Programming Languages PhD Student"
                "University of Bristol"
                ""
        , Cmd.none
        )

-- -----------------------------------------------------------------------------
-- Update
-- -----------------------------------------------------------------------------

-- AST of events that can happen.
type Msg = GetInterest

-- How we react to each possible event.
update : Msg -> Details -> (Details, Cmd Msg)
update msg model =
    case msg of
        GetInterest ->
            ( { model | interest = model.interest ++ "interest " }, Cmd.none )


-- -----------------------------------------------------------------------------
-- View
-- -----------------------------------------------------------------------------

view : Details -> Html Msg
view model =
    div [ class "jumbotron" ]
    -- TODO:- make me reactive
        [ -- Photo:
          img [ src "Content/Images/me.jpg", width 200, height 200 ] []
          -- Details:
        , h1 [] [ text model.name ]
        , p []
            [ text model.position ]
        , p []
            [ text model.institution ]
        --   -- Interests:
        -- , p [ class "text-center" ] [
        --     button [ class "btn btn-success", onClick GetInterest ] [ text "Grab an Interest!" ]
        -- ]
        -- , blockquote [] [
        --     p [] [text model.interest]
        -- ]
          -- Socials:
        , p [class "socials"]
            [ a [ href "https://www.linkedin.com/in/samantha-frohlich-a09a1b158" ]
                [ img [ src "Content/Images/linkedin.svg", width 30, height 30 ] [] ]
            , a [ href " mailto:sf16540@my.bristol.ac.uk" ]
                [ img [ src "Content/Images/envelope.svg", width 30, height 30 ] [] ]
            , a [ href "https://github.com/SamFrohlich" ]
                [ img [ src "Content/Images/github.svg", width 30, height 30 ] [] ]
            ]
    ]
