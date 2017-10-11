# SimpleDate [![Build Status](https://travis-ci.org/lucasssm/SimpleDate.svg?branch=master)](https://travis-ci.org/lucasssm/SimpleDate)

### Example
For a working example, check out the Example file

```elm
{ simpleDate = {day = Nothing, month = Nothing, year = Nothing}
```
This is a instance of simpleDate type.


```elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
      UpdateField field value ->
          let
            newDate = dateUpdate model.simpleDate field value
          in
            {model | simpleDate = newDate} ! []
```
Here you can work with the "dateUpdate" function alongside with a message type.
field is the type that you want to edit, passed as a string. types can be:
- "day"
- "month"
- "year"
