module DateType
  exposing
      ( SimpleDate

      )

import Json.Decode as Dec exposing (Decoder)
import Json.Encode as Enc
import Json.Decode.Pipeline exposing (decode, required)
{-| Simple date type
  Represents a date in elm, but only day - month - year.

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



{-| Simple date Decoder
  This function takes SimpleDate type and turns it into a json
-}

toJson : SimpleDate -> Dec.Value
toJson date =
    let
        str =
            Enc.string
    in
    Enc.object
        [ ( "month", str date.month)
        , ( "day", str date.day)
        , ( "year", str date.year)
        ]
