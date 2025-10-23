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
    , pubs : List Publication
    , talks : List Talk
    , teaching : List Teaching
    }

type alias Social =
    { name : String
    , website : String
    , pic : String
    }

type alias Publication =
    { title : String
    , authors : String
    , venue : String
    , coreRanking : String
    , paperLink : String
    , presentationLink : String
    }

type alias Talk =
    { title : String
    , presenters : String
    , venue : String
    , slidesLink : String
    , videoLink : String
    }

type alias Teaching =
    { title : String
    , year : String
    , websiteLink : String
    }

fixedInfo : Details
fixedInfo = Details
    "Sam Frohlich"
    "Lecturer"
    "University of Bristol"
    ["Security and Privacy", "Bidirectional programming", "Embedded domain specific languages", "Functional programming", "Language design"]
    "My name is Sam (she/her), and I'm a lecturer at the [University of Bristol](https://www.bristol.ac.uk/) in the [Programming Languages Research Group](https://plrg-bristol.github.io/). I'm also finalising my PhD supervised by [Meng Wang](https://mengwangoxf.github.io/). I'm a highly creative researcher (you'll never see me with LaTeX slides) and I love teaching. <br> Fun fact about me: I have represented Scotland internationally at [quadball](https://quadballuk.org/programmes/team-scotland) as their captain!"
    [ Social
        "ORCiD"
        "https://orcid.org/0000-0002-4423-6918"
        "Content/Images/orcid.svg"
    , Social
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
    ]
    [ Publication
        "Embedding by Unembedding"
        "Kazutaka Matsuda, Samantha Frohlich, Meng Wang, Nick Wu"
        "ICFP 2023"
        "A"
        "Content/Papers/EmbeddingByUnembedding.pdf"
        "https://youtu.be/9vtoJrZxa0k?si=Fjdf1NevPgpIb4g4"
    , Publication
        "Reflecting on Random Generation (Distinguished Paper)"
        "Harrison Goldstein, Samantha Frohlich, Meng Wang, Benjamin C. Pierce"
        "ICFP 2023"
        "A"
        "Content/Papers/ReflectingOnRandomGeneration.pdf"
        "https://youtu.be/iutt_BKLgDk?si=0tUq7ZT5HymJq2Hr"
    , Publication
        "CircuitFlow: A Domain Specific Language for Dataflow Programming"
        "Riley Evans, Samantha Frohlich, Meng Wang"
        "PADL 2022"
        "B"
        "Content/Papers/CircuitFlow.pdf"
        "https://www.youtube.com/watch?v=LGaTnxYcdm4"
    ]
    [
        Talk
        "Reflections on Property Based Testing"
        "Samantha Frohlich"
        "VeTSS Annual Conference 2025"
        "https://docs.google.com/presentation/d/14WTcaoPMsGxLXBLH01UCraEEydDz47tk/edit?usp=sharing&ouid=106799023342350342037&rtpof=true&sd=true"
        "https://www.youtube.com/watch?v=IZjlfz_GwHc"
    ,   Talk
        "Getting the Most Out of ICFP"
        "Samantha Frohlich"
        "PLMW@ICFP24"
        "https://docs.google.com/presentation/d/1f53W7p2DlqpWQBbtYykaedsboNJyPEBIbD39WbDVbks/edit?usp=sharing"
        "https://www.youtube.com/watch?v=pS_vJAwjcM4&t=59m6s"
     ,   Talk
        "Consider Collaboration"
        "Samantha Frohlich, Harrison Goldstein"
        "PLMW@POPL24"
        "https://docs.google.com/presentation/d/1HrdGtbl5EW4M0XZ3hZJmi72lMAb2jrjcY_lXdcRzrjY/edit?usp=sharing"
        "https://youtu.be/sSl-856qUOA?si=eDFwmkAMMG2_ejUl"
    ]
    [ Teaching
       "Advanced Topics in Programming Languages"
       "2024-25 and 2025-26"
       "https://plrg-bristol.github.io/ATiPL/index.html"
    , Teaching
        "Advanced Haskell"
        "2024 and 2025"
        "https://github.com/plrg-bristol/advanced-haskell-2025"
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
        , h1 [] [ text fixedInfo.name , small [] [text " (she/her)"]]
        , p []
            [ text fixedInfo.position ]
        , p []
            [ text fixedInfo.institution ]
        -- About Me:
        , strong [] [text "Research Interests:"]
        , viewResearchInts fixedInfo.interests
        , p [style "padding" "0 20%"] (Markdown.toHtml Nothing fixedInfo.aboutMe)
        -- Publications
        , h2 [] [text "Publications:"]
        , p [] (L.map viewPub fixedInfo.pubs)
        -- Awards
        , h2 [] [text "Awards:"]
        , dl [] [ dt [] [text "2023"], dd [] [text "ICFP 2023 Distinguished paper"]
                , dt [] [text "2020"], dd [] [text "EPSRC Doctoral Training Partnership Studentship"]
                , dt [] [text "2019"], dd [] [text "ICFP 2019 SRC, Undergraduate Category, 1st Place"]
                ]
        -- Talks
        , h2 [] [text "Talks:"]
        , p [] (L.map viewTalk fixedInfo.talks)
        -- Teaching
        , h2 [] [text "Teaching:"]
        , p [] (L.map viewTeaching fixedInfo.teaching)
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

viewPub : Publication -> Html msg
viewPub pub = div []
    [ em [] [text pub.title]
    , p [] ([ text pub.authors
           , br [] []
           , text (pub.venue ++ " (Core Ranking: " ++ pub.coreRanking ++ ")")
           , br [] []]
           ++ (Markdown.toHtml Nothing ("([Paper](" ++ pub.paperLink ++ "), [Talk](" ++ pub.presentationLink ++ "))")))
    ]
viewTalk : Talk -> Html msg
viewTalk t = div []
    [ em [] [text t.title]
    , p [] ([ text t.presenters
           , br [] []
           , text t.venue
           , br [] []]
           ++ (Markdown.toHtml Nothing ("([Slides](" ++ t.slidesLink ++ "), [Recording](" ++ t.videoLink ++ "))")))
    ]
viewTeaching : Teaching -> Html msg
viewTeaching t = div []
    [ em [] [text t.title]
    , p [] ([ text t.year
           , br [] []]
           ++ (Markdown.toHtml Nothing ("([Website](" ++ t.websiteLink ++ "))")))
    ]