import Html exposing (Html, Attribute, div, input, ul, li, text, img)
import Html.App as Html
import Html.Attributes
import Html.Events exposing (onInput)
import Http
import Json.Decode exposing (..)
import String
import Task

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none -- What the heck is \_ ???
    }

-- MODEL
type alias Edition =
  { image_url : String
  , multiverse_id : Int
  }


type alias Card =
  { name : String
  , editions : List Edition
  }

type alias Model =
  { search : String
  , cards : List Card
  }

init : (Model, Cmd Msg)
init =
  (Model "" [], Cmd.none)

-- UPDATE

type Msg
  = Change String
  | FetchSucceed (List Card)
  | FetchFail Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change query ->
      ({ model | search = query }, searchDeckbrew query)

    FetchSucceed cards ->
      ({ model | cards = cards }, Cmd.none)

    FetchFail _ ->
      (model, Cmd.none)


searchDeckbrew : String -> Cmd Msg
searchDeckbrew query =
  let
    url =
      "https://api.deckbrew.com/mtg/cards/typeahead?q=" ++ query
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeCards url)

editionDecoder : Decoder Edition
editionDecoder =
  object2 Edition
    ("image_url" := string)
    ("multiverse_id" := int)

cardDecoder : Decoder Card
cardDecoder =
  object2 Card
    ("name" := string)
    ("editions" := (list editionDecoder))

decodeCards : Decoder (List Card)
decodeCards =
  (list cardDecoder)

cardURL : Card -> String
cardURL c = 
  case List.head (List.filter (\a -> a.multiverse_id > 0) c.editions) of
    Just x -> x.image_url
    Nothing -> "https://image.deckbrew.com/mtg/multiverseid/0.jpg"

-- VIEW
toLi : Card -> Html msg
toLi c = 
  li [] [ img [Html.Attributes.src (cardURL c) ] [] ]

view : Model -> Html Msg
view model =
  div []
    [ input [ Html.Attributes.placeholder "Search for cards", onInput Change ] []
    , ul [] (List.map toLi model.cards)
    ]
