module Tests exposing (..)
import Test exposing (..)
import Expect
import Tests.SimpleDate exposing (..)


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "elm-form Suite"
        [ Tests.SimpleDate.all
        ]
