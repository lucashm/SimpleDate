module Main exposing (..)

import Html exposing (Html, Attribute, text, div, input, option, select)
import Html.Events exposing (onInput)
import Html.Attributes exposing (style, value, disabled, selected)
import SimpleDate exposing (..)
import Json.Decode as Json

---- MODEL ----


type alias Model =
    {simpleDate: SimpleDate}


init : ( Model, Cmd Msg )
init =
    (
    { simpleDate = {day = Nothing, month = Nothing, year = Nothing}
    }
    , Cmd.none )



---- UPDATE ----


type Msg
    = UpdateField String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
      UpdateField field value ->
          let
            newDate = dateUpdate model.simpleDate field value
          in
            {model | simpleDate = newDate} ! []



---- VIEW ----


view : Model -> Html Msg
view model =
    div [style [("margin", "10px")]]
        [ div [] [ text "Insert a Date:" ]
        , div [style [("margin", "5px")]]
          [ text " Year: "
          , input [onInput (UpdateField "year")] []
          , text " Month: "
          , monthPicker
          , text " Day: "
          , input [onInput (UpdateField "day")] []
          ]
        , div [style [("margin-top", "10px")]] [text ("Date(Will only appear if date is 100% valid):" ++ (show model.simpleDate))]
        , div [style [("margin-top", "10px")]] [ text ("Date toInt -> toString again:")]
        , div [] [text ("Year: " ++ toString (year model.simpleDate))]
        , div [] [text ("Month: " ++ toString (month model.simpleDate) ++ " / " ++ (monthName (model.simpleDate)))]
        , div [] [text ("Day: " ++ toString(day model.simpleDate))]
        , div [] [text ("Json format: ")]
        , div [] [text (toString (toJson model.simpleDate))]
        ]


-- Custom onChange event for select fields usage
onChange : (String -> msg) -> Attribute msg
onChange handler =
    Html.Events.on "change" <| Json.map handler <| Json.at [ "target", "value" ] Json.string

monthPicker : Html Msg
monthPicker =
    select [ Html.Attributes.name "Month", onChange (UpdateField "month") ]
        [ option [ value "", disabled True, selected True ] [ text "Month" ]
        , option [ value "01" ] [ text "January" ]
        , option [ value "02" ] [ text "February" ]
        , option [ value "03" ] [ text "March" ]
        , option [ value "04" ] [ text "April" ]
        , option [ value "05" ] [ text "May" ]
        , option [ value "06" ] [ text "June" ]
        , option [ value "07" ] [ text "July" ]
        , option [ value "08" ] [ text "August" ]
        , option [ value "09" ] [ text "September" ]
        , option [ value "10" ] [ text "October" ]
        , option [ value "11" ] [ text "November" ]
        , option [ value "12" ] [ text "December" ]
        ]


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
