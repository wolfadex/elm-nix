module Main exposing (Model, Msg, Paused, Stage, main)

import Browser
import Browser.Events
import Element exposing (..)
import Element.Input as Input
import Length exposing (Length)
import Numeral
import Quantity


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { stage : Stage
    , elapsedGameTime : Float
    , totalElapsedTime : Float
    , remainingTickTime : Float
    , paused : Paused
    }


type Paused
    = Paused
    | Unpaused


type Stage
    = Menu
    | Running RunningModel


type alias RunningModel =
    { planet : Planet
    , distanceTraveled : Length
    }


type alias Planet =
    { name : String
    , distanceFromEarth : Length
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { stage = Menu
      , elapsedGameTime = 0
      , totalElapsedTime = 0
      , remainingTickTime = 0
      , paused = Unpaused
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta Frame


type Msg
    = Frame Float
    | GotPause Paused
    | SelectPlanet Planet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame deltaTime ->
            ( case ( model.paused, model.stage ) of
                ( Unpaused, Running runningModel ) ->
                    { model
                        | totalElapsedTime = model.totalElapsedTime + deltaTime
                        , elapsedGameTime = model.elapsedGameTime + deltaTime
                    }
                        |> runTick deltaTime runningModel

                _ ->
                    { model
                        | totalElapsedTime = model.totalElapsedTime + deltaTime
                    }
            , Cmd.none
            )

        GotPause paused ->
            ( { model | paused = paused }
            , Cmd.none
            )

        SelectPlanet planet ->
            case model.stage of
                Menu ->
                    ( { model
                        | stage =
                            Running
                                { planet = planet
                                , distanceTraveled = Length.meters 0
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


tickTime : Int
tickTime =
    3000


runTick : Float -> RunningModel -> Model -> Model
runTick deltaTime runningModel model =
    let
        totalTickTime : Int
        totalTickTime =
            floor (model.remainingTickTime + deltaTime)

        ticksToRun : Int
        ticksToRun =
            totalTickTime
                |> modBy tickTime

        remainingTickTime : Float
        remainingTickTime =
            totalTickTime
                |> remainderBy tickTime
                |> toFloat
    in
    { model
        | remainingTickTime = remainingTickTime
    }
        |> runTickHelper ticksToRun runningModel


runTickHelper : Int -> RunningModel -> Model -> Model
runTickHelper timesToRun runningModel model =
    if timesToRun > 0 then
        runTickHelper (timesToRun - 1)
            { runningModel
                | distanceTraveled =
                    runningModel.distanceTraveled
                        |> Quantity.plus (Length.kilometers 500)
            }
            model

    else
        { model | stage = Running runningModel }


view : Model -> Browser.Document Msg
view model =
    { title = "Oregon Space Trail"
    , body = [ layout [] (viewModel model) ]
    }


viewModel : Model -> Element Msg
viewModel model =
    case model.stage of
        Menu ->
            viewMenu model

        Running runningModel ->
            viewRunning model runningModel


viewMenu : Model -> Element Msg
viewMenu _ =
    planets
        |> List.map viewPlanetChoice
        |> column [ spacing 8 ]


viewPlanetChoice : Planet -> Element Msg
viewPlanetChoice planet =
    Input.button
        []
        { onPress = Just (SelectPlanet planet)
        , label =
            column
                []
                [ text ("Name: " ++ planet.name)
                , text
                    ("Distance: " ++ displayDistanceInLightYears planet.distanceFromEarth)
                ]
        }


viewRunning : Model -> RunningModel -> Element Msg
viewRunning model runningModel =
    column
        []
        [ Input.button
            []
            { label =
                text <|
                    case model.paused of
                        Paused ->
                            "Unpause"

                        Unpaused ->
                            "Pause"
            , onPress =
                Just <|
                    GotPause <|
                        case model.paused of
                            Paused ->
                                Unpaused

                            Unpaused ->
                                Paused
            }
        , text ("Destination: " ++ runningModel.planet.name)
        , text ("Distance traveled: " ++ displayDistanceInLightYears runningModel.distanceTraveled)
        , text
            ("Distance to go: "
                ++ displayDistanceInLightYears
                    (Quantity.difference
                        runningModel.planet.distanceFromEarth
                        runningModel.distanceTraveled
                    )
            )
        ]


planets : List Planet
planets =
    [ { name = "Proxima Centauri b"
      , distanceFromEarth = Length.lightYears 4.25
      }
    , { name = "Ross 128 b"
      , distanceFromEarth = Length.lightYears 11.03
      }
    ]


displayDistanceInLightYears : Length -> String
displayDistanceInLightYears distance =
    distance
        |> Length.inLightYears
        |> Numeral.format "0,0.00000[ly]"
