module Progbar exposing ( Model, Msg, init, update, view )

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (id, style, value)
import Html.Events exposing (onInput)
import Time exposing (Time, millisecond)
import String exposing (concat)


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Model =
  { text : String
  , isFilling : Bool
  , fillPercent : Int
  , msSinceLastStop : Int
  , pastMessages : List String
  }

init : (Model, Cmd Msg)
init =
  (Model "" False 0 0 [], Cmd.none)


-- UPDATE
type Msg
  = UserInput String
  | ProgbarSubmit
  | Tick Time
  | Fill

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UserInput userInput ->
      ({model | text = userInput
              , fillPercent = 0
              , isFilling = False
              , msSinceLastStop = 0}, Cmd.none)

    Tick _ ->
      -- If the progbar is already filling, keep on going
      if model.isFilling then
        update Fill model
      -- If the progbar has been typed in and it's been 200ms since start
      -- filling.
      else if not (model.text == "") && (model.msSinceLastStop >= 100) then
        update Fill {model | isFilling = True, msSinceLastStop = 0}
      -- Otherwise, no-op.
      else if not (model.text == "") then
        ({model | msSinceLastStop = (model.msSinceLastStop + 1)}, Cmd.none)
      else
        (model, Cmd.none)

    Fill ->
      let
        updatedModel = {model | fillPercent = (model.fillPercent + 1)}
      in
        if updatedModel.text == "" then
          ({model | fillPercent = 0, isFilling = False}, Cmd.none)
        else if updatedModel.fillPercent == 100 then
          update ProgbarSubmit updatedModel
        else
          (updatedModel, Cmd.none)

    ProgbarSubmit ->
      (Model "" False 0 0 (model.pastMessages ++ [model.text]), Cmd.none)


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every millisecond Tick


view : Model -> Html Msg
view model =
  div [containerCss]
    [ div [ (progressDivCss model.fillPercent) ]
        [ input [value model.text, onInput UserInput, inputCss] []
        ]
    , div [ id "messages"]
        (List.map viewMessage model.pastMessages)
    ]

viewMessage message =
  h2 [messageCss] [text message]

messageCss =
  style
    [ ("color",  "#FF2222")
    ]

containerCss =
  style
    [ ("width", "100%")
    ]

progressDivCss width =
  let
    widthPercent = concat [(toString width), "%"]
  in
    style
      [ ("background-color", "#999")
      , ("position", "relative")
      , ("top", "0")
      , ("left", "0")
      , ("width", widthPercent)
      ]

inputCss =
  style
    [ ("border", "1px solid #999")
    , ("background", "none")
    , ("outline", "none")
    , ("padding", "10px")
    , ("width", "400px")
    , ("font-size", "20px")
    , ("color", "#999")
    , ("margin", "0")
    ]
