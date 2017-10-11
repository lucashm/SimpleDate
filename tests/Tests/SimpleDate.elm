module Tests.SimpleDate exposing (..)
-- import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Expect
-- import String
import SimpleDate exposing (..)

-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


emptyDate : String
emptyDate = "{ year = null, month = null, day = null }"

all : Test
all =
    describe "SimpleDate module"
      [ describe "SimpleDate Encoder"
          [ test "Encoding empty SimpleDate" <|
              \() ->
                  let
                    simpleDate = {day = Nothing, month = Nothing, year = Nothing}
                    encode = SimpleDate.toJson simpleDate
                  in
                    Expect.equal (toString(encode)) emptyDate
          , test "Encoding filled SimpleDate" <|
              \() ->
                  let
                    simpleDate = {day = Just 22, month = Just 05, year = Just 1995}
                    encode = SimpleDate.toJson simpleDate
                    stringDate = "{ year = 1995, month = 5, day = 22 }"
                  in
                    Expect.equal (toString(encode)) stringDate
          , test "Encoding invalid SimpleDate" <|
              \() ->
                  let
                    simpleDate = {day = Just 32, month = Just 05, year = Just 1995}
                    encode = SimpleDate.toJson simpleDate
                    stringDate = "{ year = 1995, month = 5, day = null }"
                  in
                    Expect.equal (toString(encode)) stringDate

          ]

      , describe "SimpleDate day"
        [ test "Returning day from SimpleDate type" <|
            \() ->
              let
                simpleDate = {day = Just 22, month = Just 05, year = Just 1995}
              in
                Expect.equal (day simpleDate) (Just 22)
        ]

      , describe "SimpleDate year"
          [ test "Returning year from SimpleDate type" <|
              \() ->
                let
                  simpleDate = {day = Just 22, month = Just 05, year = Just 1995}
                in
                  Expect.equal (year simpleDate) (Just 1995)
          ]

      , describe "SimpleDate month"
          [ test "Returning month from SimpleDate type" <|
              \() ->
                let
                  simpleDate = {day = Just 22, month = Just 05, year = Just 1995}
                in
                  Expect.equal (month simpleDate) (Just 5)
          , test "Returning month name from SimpleDate type" <|
              \()->
                let
                  simpleDate = {day = Just 22, month = Just 05, year = Just 1995}
                in
                  Expect.equal (monthName simpleDate) "May"
          ]

      , describe "SimpleDate show function"
          [ test "show valid SimpleDate" <|
              \() ->
                let
                  simpleDate = {day = Just 22, month = Just 05, year = Just 1995}
                  validOutput = "1995-5-22"
                in
                  Expect.equal (show simpleDate) validOutput
          , test "show with invalid Simpledate" <|
              \() ->
                let
                  simpleDate = {day = Just 22, month = Just 13, year = Just 1995}
                  expectedOutput = ""
                in
                  Expect.equal (show simpleDate) expectedOutput
          ]
      , describe "dateUpdate function"
          [ test "should update simpleDate day" <|
              \() ->
                let
                  simpleDate = {day = Just 22, month = Just 12, year = Just 1995}
                  updatedDay = dateUpdate simpleDate "day" "10"
                  expectedOutput = {day = Just 10, month = Just 12, year = Just 1995}
                in
                Expect.equal updatedDay expectedOutput
          , test "should update simpleDate month" <|
              \() ->
                let
                  simpleDate = {day = Just 22, month = Just 12, year = Just 1995}
                  updatedMonth = dateUpdate simpleDate "month" "10"
                  expectedOutput = {day = Just 22, month = Just 10, year = Just 1995}
                in
                Expect.equal updatedMonth expectedOutput
          , test "should update simpleDate year" <|
              \() ->
                let
                  simpleDate = {day = Just 22, month = Just 12, year = Just 1995}
                  updatedYear = dateUpdate simpleDate "year" "1990"
                  expectedOutput = {day = Just 22, month = Just 12, year = Just 1990}
                in
                Expect.equal updatedYear expectedOutput
          ]


      ]
