module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import List as L
import Markdown

{-
Website update plan:
* add the things your website needs:
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
    , interests : List String
    , aboutMe : String
    , socials : List Social
    }

type alias Social =
    { name : String
    , website : String
    , pic : String
    }

fixedInfo : Details
fixedInfo = Details
    "Sam Frohlich"
    "Programming Languages PhD Student"
    "University of Bristol"
    ["Bidirectional programming", "Embembedded domain specific languages", "Functional programming", "Language design"]
    "My name is Sam (she/her), and I'm a PhD Student at the [University of Bristol](https://www.bristol.ac.uk/), in the [Programming Languages Research Group](https://plrg-bristol.github.io/), supervised by [Meng Wang](https://mengwangoxf.github.io/). I'm a highly creative researcher (you'll never see me with LaTeX slides) and love teaching. <br> Fun fact about me: I have represented Scotland internationally at [quadball](https://quadballuk.org/programmes/team-scotland) as their captain!"
    [ Social
        "LinkedIn"
        "https://www.linkedin.com/in/samantha-frohlich-a09a1b158"
        "Content/Images/linkedin.svg"
    , Social
        "Email"
        "mailto:samantha.frohlich@bristol.ac.uk"
        "Content/Images/envelope.svg"
    , Social
        "GitHub"
        "https://github.com/SamFrohlich"
        "Content/Images/github.svg"
    , Social
        "ORCiD"
        "https://orcid.org/0000-0002-4423-6918"
        "Content/Images/orcid.svg"
    ]


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
          -- About Me:
        , strong [] [text "Research Interests:"]
        , viewResearchInts fixedInfo.interests
        , p [style "padding" "0 10%"] (Markdown.toHtml Nothing fixedInfo.aboutMe)
          -- Socials:
        , p [class "socials"] (L.map viewSocial fixedInfo.socials)
    ]

viewSocial : Social -> Html Msg
viewSocial s
    = a [ href s.website ]
        [ img [ src s.pic
                , width 30, height 30
                , alt s.name
                , title s.name]
                [] ]

viewResearchInts : List String -> Html msg
viewResearchInts lst =
    ul [style "list-style-type" "none", style "padding-left" "2rem"]
        (List.map (\l -> li [] [ text l ]) lst)