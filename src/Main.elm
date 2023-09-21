module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

{-
Website update plan:
* start using elm properly e.g. model is just what changes, and we have views of that
    - make viewSocials function
    - fill in socials
* add the things your website needs:
    - interests
    - link to PLRG
    - publications (publiocation record and view publication function, you'll want the cite, the pdf, the talk, the video)
    ? teaching stuff
* take advantage of elm
    - get some cute animations
* make things pretty
    - have clickable things go transparent on hover

-}

-- -----------------------------------------------------------------------------
-- Main
-- -----------------------------------------------------------------------------

main : Program () () Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }

-- -----------------------------------------------------------------------------
-- Fixed Information
-- -----------------------------------------------------------------------------

type alias Details =
    { name : String
    , position : String
    , institution : String
    , interest : String
    , socials : List Social
    }

type alias Social =
    { name : String
    , website : String
    }

fixedInfo : Details
fixedInfo = Details
    "Sam Frohlich"
    "Programming Languages PhD Student"
    "University of Bristol"
    ""
    []
    -- TODO fill in social info
    -- Social "LinkedIn" "https://www.linkedin.com/in/samantha-frohlich-a09a1b158"

-- -----------------------------------------------------------------------------
-- Model
-- -----------------------------------------------------------------------------

init : flags -> ((), Cmd Msg)
init _ = ( ()
        , Cmd.none
        )

-- -----------------------------------------------------------------------------
-- Update
-- -----------------------------------------------------------------------------

-- AST of events that can happen.
type Msg = GetInterest

-- How we react to each possible event.
update : Msg -> () -> ((), Cmd Msg)
update msg _ =
    case msg of
        GetInterest ->
            ( (), Cmd.none )


-- -----------------------------------------------------------------------------
-- View
-- -----------------------------------------------------------------------------

view : () -> Html Msg
view _ =
    div [ class "jumbotron" ]
    -- TODO:- make me reactive
        [ -- Photo:
          img [ src "Content/Images/me.jpg", width 200, height 200 ] []
          -- Details:
        , h1 [] [ text fixedInfo.name ]
        , p []
            [ text fixedInfo.position ]
        , p []
            [ text fixedInfo.institution ]
          -- Socials:
          -- TODO write a viewSocials that will create this from fixedInfo
        , p [class "socials"]
            [ a [ href "https://www.linkedin.com/in/samantha-frohlich-a09a1b158" ]
                [ img [ src "Content/Images/linkedin.svg"
                      , width 30, height 30
                      , alt "LinkedIn"
                      , title "LinkedIn"]
                      [] ]
            , a [ href "mailto:samantha.frohlich@bristol.ac.uk" ]
                [ img [ src "Content/Images/envelope.svg"
                      , width 30, height 30
                      , alt "Email"
                      , title "Email" ]
                      [] ]
            , a [ href "https://github.com/SamFrohlich" ]
                [ img [ src "Content/Images/github.svg"
                      , width 30, height 30
                      , alt "GitHub"
                      , title "GitHub" ]
                      [] ]
            , a [ href "https://orcid.org/0000-0002-4423-6918" ]
                [ img [ src "Content/Images/orcid.svg"
                      , width 30, height 30
                      , alt "ORCiD"
                      , title "ORCiD"]
                      [] ]
            ]
    ]
