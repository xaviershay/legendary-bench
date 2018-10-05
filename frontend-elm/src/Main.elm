module Main exposing (Model, Msg(..), init, main, subscriptions, update, view, viewLink)

import Browser
import Browser.Navigation as Nav
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Http
import Json.Decode as Decode
import Url
import Url.Builder
import Url.Parser exposing ((</>), Parser, int, map, oneOf, s, string)


type Route
    = Game Int
    | Home


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Game (s "game" </> int)
        , map Home Url.Parser.top
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , seed : String
    , currentRoute : Route
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url "" Home, Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | SetSeed String
    | CreateGame
    | GameCreated (Result Http.Error String)


decoder : Decode.Decoder String
decoder =
    Decode.succeed "hi"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateGame ->
            ( model, Http.send GameCreated (Http.get "http://localhost:8080/games/1" decoder) )

        GameCreated _ ->
            ( { model | currentRoute = Game 1 }, Nav.pushUrl model.key (Url.Builder.absolute [ "game", String.fromInt 1 ] []) )

        SetSeed x ->
            ( { model | seed = x }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            case Url.Parser.parse routeParser url of
                Just x ->
                    ( { model | currentRoute = x }, Cmd.none )

                Nothing ->
                    Debug.todo "no route found"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.currentRoute of
        Home ->
            { title = "URL Interceptor"
            , body =
                [ text "The current URL is: "
                , b [] [ text (Url.toString model.url) ]
                , div []
                    [ input [ type_ "text", value model.seed, Html.Events.onInput SetSeed ] []
                    , button [ Html.Events.onClick CreateGame ] [ text "New Game" ]
                    ]
                , ul []
                    [ viewLink "/game/1"
                    ]
                ]
            }

        Game n ->
            { title = "Game"
            , body =
                [ text ("The game id is" ++ String.fromInt n)
                ]
            }


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]
