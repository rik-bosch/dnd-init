module Main exposing (..)

import Browser
import Character exposing (Character, viewList)
import Http
import Html exposing (Html, div, h1, h2, text)
import Json.Decode exposing (Decoder)
import Decode exposing (decodeCharacters)
import Combatant exposing (Combatant, initializeCombatant, viewCombatant)


main : Program () Model Msg
main =
  Browser.element
    { init = ( \_ -> init )
    , view = view
    , update = update
    , subscriptions = ( \_ -> Sub.none )
    }


type alias Model =
  { pcs: List Character
  , monsters : List Character
  , combatants: List Combatant
  , error: Maybe Http.Error
  }


init : ( Model, Cmd Msg )
init =
  ( Model [] [] [] Nothing
  , Cmd.batch
    [ Http.get
      { url = "/data/pcs.json"
      , expect = handleHttpResult LoadPcs decodeCharacters
      }
    , Http.get
      { url = "/data/monsters.json"
      , expect = handleHttpResult LoadMonsters decodeCharacters
      }
    ]
  )


type Msg
  = LoadPcs ( List Character )
  | LoadMonsters ( List Character )
  | HandleError Http.Error 
  | SelectCharacter Character


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LoadPcs pcs ->
      ( { model | pcs = pcs }
      , Cmd.none
      )

    LoadMonsters monsters ->
      ( { model | monsters = monsters }
      , Cmd.none
      )

    HandleError error ->
      ( { model | error = Just error }
      , Cmd.none
      )

    SelectCharacter character ->
      initializeCombatant character
        |> ( \combatant -> combatant :: model.combatants )
        |> ( \combatants ->
            ( { model | combatants = combatants }
            , Cmd.none
            )
           )


view : Model -> Html Msg
view model =
  case model.error of
    Just _ ->
      div [] [ text "Error" ]

    Nothing ->
      div []
        [ div []
          [ h1 [] [ text "Characters" ]
          , div []
            [ h2 [] [ text "Player Characters" ]
            , viewList SelectCharacter model.pcs
            ]
          , div []
            [ h2 [] [ text "Monsters / NPCs" ]
            , viewList SelectCharacter model.monsters
            ]
          ]
        , div []
          ( h1 [] [ text "Combatants" ]
          :: List.map viewCombatant model.combatants 
          )
        ]


-- Helpers
handleHttpResult : ( a -> Msg ) -> Decoder a -> Http.Expect Msg
handleHttpResult success decoder =
  let
    unpackResult result =
      case result of
        Ok content ->
          success content

        Err error ->
          HandleError error

  in
    Http.expectJson unpackResult decoder
