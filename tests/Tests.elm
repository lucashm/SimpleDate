module Tests exposing (..)

import Test exposing (..)
import Tests.SimpleDate


all : Test
all =
    describe "elm-form Suite"
        [ Tests.SimpleDate.all
        ]
