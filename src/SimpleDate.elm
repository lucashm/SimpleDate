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
{-| Elm SimpleDate type
This package try to work with date but in a simple way.
It only have day, month and year -> All Strings
The elm SimpleDate module can show date in ISO8601 format,
transform each day, month and year in Int, and
make a Json from a SimpleDate type.

@docs SimpleDate, show, day, month, monthName, year, toJson, dateUpdate
-}
import Json.Decode as Dec exposing (Decoder)
import Json.Encode as Enc
import Json.Decode.Pipeline exposing (decode, required)
{-| Simple date type
  Represents a date in elm, but only day - month - year.
  All three values are String type.
-}
type alias SimpleDate =
  { month: Maybe String
  , day: Maybe String
  , year: Maybe String
  }


{-| Simple date Decoder
  This function takes a json and turns it into a SimpleDate type
-}
dateDecoder : Dec.Decoder SimpleDate
dateDecoder =
    decode SimpleDate
      |> required "month" (Dec.maybe Dec.string)
      |> required "day" (Dec.maybe Dec.string)
      |> required "year" (Dec.maybe Dec.string)



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
        [ ( "year", maybeEncoder (year date) Enc.int)
        , ( "month", maybeEncoder (month date) Enc.int)
        , ( "day",  maybeEncoder (day date) Enc.int)
        ]


{-| Returns the day in Int type
    if it fails, returns -1.
-}
day : SimpleDate -> Maybe Int
day date =
  let
    conversion = String.toInt(maybeExtractor date.day)
  in
    case conversion of
      Ok result ->
        if result >= 1 && result <= 31 then
          Just result
        else
          Nothing
      Err _ ->
        Nothing


{-| Returns the month in Int type
    if it fails, returns -1.
-}
month : SimpleDate -> Maybe Int
month date =
  let
    conversion = String.toInt(maybeExtractor date.month)
  in
    case conversion of
      Ok result ->
        if result >= 1 && result <= 12 then
          Just result
        else
          Nothing
      Err _ ->
        Nothing


{-| takes a date and returns its month name
    if it's invalid, will return "Invalid month"
-}
monthName: SimpleDate -> String
monthName date =
  let
    conversion = month date
  in
    case conversion of
      Just 1 -> "January"
      Just 2 -> "February"
      Just 3 -> "March"
      Just 4 -> "April"
      Just 5 -> "May"
      Just 6 -> "June"
      Just 7 -> "July"
      Just 8 -> "August"
      Just 9 -> "September"
      Just 10 -> "October"
      Just 11 -> "November"
      Just 12 -> "December"
      Just _ -> "Invalid month"
      Nothing -> "Invalid month"


{-| Returns the year in Int type
    if it fails, returns -1.
-}
year : SimpleDate -> Maybe Int
year date =
  let
    conversion = String.toInt(maybeExtractor date.year)
  in
    case conversion of
      Ok result ->
        if result >= 1 && result <= 2999 then
          Just result
        else
          Nothing
      Err _ ->
        Nothing


maybeExtractor: Maybe String -> String
maybeExtractor value =
  case value of
    Nothing ->
      ""
    Just string ->
      string



maybeEncoder: Maybe a -> (a ->Enc.Value) -> Enc.Value
maybeEncoder a type_ =
  case a of
    Nothing ->
      Enc.null
    Just val ->
      type_ val


{-|
  This function takes a SimpleDate type and transforms it into a string.
  This string has ISO8601 format.
-}
show: SimpleDate -> String
show date =
  case (year date) of
    Just yearValue ->
      case (month date) of
        Just monthValue ->
          case (day date) of
            Just dayValue ->
              if toString(yearValue) /= "" && toString(monthValue) /= "" && toString(dayValue) /= "" then
                toString(yearValue) ++ "-" ++ toString(monthValue) ++ "-" ++ toString(dayValue)
              else
                ""
            Nothing->
              ""
        Nothing ->
          ""
    Nothing ->
      ""


{-|
  This function takes a simpleDate, a field to be edited and a value
  fields are = "day", "month" and "year"
-}
dateUpdate : SimpleDate -> String -> String -> SimpleDate
dateUpdate date field value =
    case field of
      "month" ->
          {date | month = Just value}
      "day" ->
          {date | day = Just value}
      "year" ->
          {date | year = Just value}
      _ ->
          date
