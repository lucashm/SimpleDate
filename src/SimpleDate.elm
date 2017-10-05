module SimpleDate
  exposing
      ( SimpleDate
      , toJson
      , day
      , month
      , monthName
      , year
      , show
      , dateUpdate
      )

import Json.Decode as Dec exposing (Decoder)
import Json.Encode as Enc
import Json.Decode.Pipeline exposing (decode, required)
{-| Simple date type
  Represents a date in elm, but only day - month - year.
  All three values are String type.
-}
type alias SimpleDate =
  { month: String
  , day: String
  , year: String
  }


{-| Simple date Decoder
  This function takes a json and turns it into a SimpleDate type
-}
dateDecoder : Dec.Decoder SimpleDate
dateDecoder =
    decode SimpleDate
      |> required "month" Dec.string
      |> required "day" Dec.string
      |> required "year" Dec.string



{-| Simple date Encoder
  This function takes SimpleDate type and turns it into a json
-}

toJson : SimpleDate -> Dec.Value
toJson date =
    let
        int =
            Enc.int
    in
    Enc.object
        [ ( "month", int (month date))
        , ( "day", int (day date))
        , ( "year", int (year date))
        ]


{-| Returns the day in Int type
    if it fails, returns -1.
-}
day : SimpleDate -> Int
day date =
  let
    conversion = String.toInt(date.day)
  in
    case conversion of
      Ok result ->
        if result >= 1 && result <= 31 then
          result
        else
          -1
      Err _ ->
        -1


{-| Returns the month in Int type
    if it fails, returns -1.
-}
month : SimpleDate -> Int
month date =
  let
    conversion = String.toInt(date.month)
  in
    case conversion of
      Ok result ->
        if result >= 1 && result <= 12 then
          result
        else
          -1
      Err _ ->
        -1


{-| takes a date and returns its month name
    if it's invalid, will return "Invalid month"
-}
monthName: SimpleDate -> String
monthName date =
  let
    conversion = month date
  in
    case conversion of
      1 -> "January"
      2 -> "February"
      3 -> "March"
      4 -> "April"
      5 -> "May"
      6 -> "June"
      7 -> "July"
      8 -> "August"
      9 -> "September"
      10 -> "October"
      11 -> "November"
      12 -> "December"
      _ -> "Invalid month"


{-| Returns the year in Int type
    if it fails, returns -1.
-}
year : SimpleDate -> Int
year date =
  let
    conversion = String.toInt(date.year)
  in
    case conversion of
      Ok result ->
        if result >= 1 && result <= 2999 then
          result
        else
          -1
      Err _ ->
        -1

{-|
  This function takes a SimpleDate type and transforms it into a string.
  This string has ISO8601 format.
-}
show: SimpleDate -> String
show date =
  if date.year == "" || date.month == "" || date.day == "" then
    ""
  else
    date.year ++ "-" ++ date.month ++ "-" ++ date.day

{-|
  This function takes a simpleDate, a field to be edited and a value
  fields are = "day", "month" and "year"
-}
dateUpdate : SimpleDate -> String -> String -> SimpleDate
dateUpdate date field value =
    case field of
      "month" ->
          {date | month = value}
      "day" ->
          {date | day = value}
      "year" ->
          {date | year = value}
      _ ->
          date
