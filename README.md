# SimpleDate
All functionalities are explained in the SimpleDate module.

### Example
For a working example, check out the Example file

```elm
{ simpleDate = {day = "", month = "", year = ""}
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
Here you can work with the "dateUpdate" function alongside with a message type
