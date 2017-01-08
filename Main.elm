module Main exposing (..)

-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/time.html

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Svg exposing (svg, circle, line)
import Svg.Attributes
    exposing
        ( viewBox
        , width
        , cx
        , cy
        , r
        , fill
        , x1
        , y1
        , x2
        , y2
        , stroke
        , strokeWidth
        )
import Time exposing (Time, second)
import Date
import Date.Extra.Create exposing (dateFromFields)
import Date.Extra.Field
import Date.Extra.Format
import Date.Extra.Config.Config_en_us


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { time :
        Maybe Time
    , weekStart : Time
    }


init : ( Model, Cmd Msg )
init =
    ( { time = Nothing
      , weekStart = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            let
                weekStart =
                    Date.Extra.Field.fieldToDateClamp (Date.Extra.Field.DayOfWeek ( Date.Mon, Date.Mon )) (Date.fromTime newTime)

                weekTime =
                    dateFromFields (Date.year weekStart) (Date.month weekStart) (Date.day weekStart) 0 0 0 0
            in
                ( { model
                    | time = Just newTime
                    , weekStart = Date.toTime weekTime
                  }
                , Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick



-- VIEW


type alias DateDetails =
    { days : Int
    , hours : Int
    , minutes : Int
    , seconds : Int
    }


date28hTime : Time -> Time -> DateDetails
date28hTime weekStart time =
    let
        date =
            Date.fromTime time

        diff =
            (time - weekStart |> round) // 1000

        hours =
            diff // 3600

        days =
            hours // 28

        hoursToday =
            rem hours 28

        minutes =
            (diff - hours * 3600) // 60

        seconds =
            (diff - hours * 3600 - minutes * 60)
    in
        { days = days + 1
        , hours = hoursToday
        , minutes = minutes
        , seconds = seconds
        }


padded : Int -> String
padded s =
    s |> toString |> String.pad 2 '0'


indicator60 : List (Html Msg)
indicator60 =
    List.range 0 11
        |> List.map
            (\idx ->
                let
                    angle =
                        degrees (360 / 12 * (toFloat idx))

                    x_1 =
                        50 + 33 * cos angle |> toString

                    x_2 =
                        50 + 34 * cos angle |> toString

                    y_1 =
                        50 + 33 * sin angle |> toString

                    y_2 =
                        50 + 34 * sin angle |> toString
                in
                    line [ x1 x_1, x2 x_2, y1 y_1, y2 y_2, stroke "#FF0000" ] []
            )


indicator28hours : List (Html Msg)
indicator28hours =
    List.range 0 27
        |> List.map
            (\idx ->
                let
                    angle =
                        degrees (360 / 28 * (toFloat idx) - 90)

                    x_ =
                        50
                            + 40
                            * cos angle
                            |> toString

                    y_ =
                        51
                            + 40
                            * sin angle
                            |> toString
                in
                    Svg.text_
                        [ Svg.Attributes.x x_
                        , Svg.Attributes.y y_
                        , Svg.Attributes.fontSize "6px"
                        , Svg.Attributes.textAnchor "middle"
                        ]
                        [ Svg.text (toString idx) ]
            )


view : Model -> Html Msg
view model =
    case model.time of
        Nothing ->
            text ""

        Just t ->
            let
                date =
                    Date.fromTime t

                date28h =
                    date28hTime model.weekStart t

                angleHours =
                    degrees ((toFloat date28h.hours) * 360 / 28 - 90)

                hoursHandX =
                    toString (50 + 25 * cos angleHours)

                hoursHandY =
                    toString (50 + 25 * sin angleHours)

                angleMinutes =
                    degrees ((toFloat date28h.minutes) * 360 / 60 - 90)

                minutesHandX =
                    toString (50 + 28 * cos angleMinutes)

                minutesHandY =
                    toString (50 + 28 * sin angleMinutes)

                angleSeconds =
                    degrees ((toFloat date28h.seconds) * 360 / 60 - 90)

                secondsHandX =
                    toString (50 + 30 * cos angleSeconds)

                secondsHandY =
                    toString (50 + 30 * sin angleSeconds)
            in
                div
                    [ style
                        [ ( "display", "flex" )
                        , ( "height", "100%" )
                        , ( "font-size", "4em" )
                        , ( "align-items", "center" )
                        , ( "justify-content", "center" )
                        ]
                    ]
                    [ div []
                        [ svg [ viewBox "0 0 100 100", width "700px" ]
                            (List.concat
                                [ [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
                                  ]
                                , indicator28hours
                                , indicator60
                                , [ line [ x1 "50", y1 "50", x2 hoursHandX, y2 hoursHandY, stroke "#023963", strokeWidth "2" ] []
                                  , line [ x1 "50", y1 "50", x2 minutesHandX, y2 minutesHandY, stroke "#923963" ] []
                                  , line [ x1 "50", y1 "50", x2 secondsHandX, y2 secondsHandY, stroke "#323963" ] []
                                  , circle [ cx "50", cy "50", r "2", fill "#023963" ] []
                                  ]
                                ]
                            )
                        ]
                    , div
                        [ style
                            [ ( "display", "flex" )
                            , ( "flex-direction", "column" )
                            , ( "justify-content", "space-between" )
                            , ( "height", "50vh" )
                            ]
                        ]
                        [ div []
                            [ text
                                ("day "
                                    ++ (toString date28h.days)
                                    ++ " "
                                    ++ (padded date28h.hours)
                                    ++ ":"
                                    ++ (padded date28h.minutes)
                                )
                            ]
                        , div []
                            [ div [] [ text "normal time: " ]
                            , text (Date.Extra.Format.format Date.Extra.Config.Config_en_us.config "%A %d/%m/%Y %H:%M" date)
                            ]
                        ]
                    ]
