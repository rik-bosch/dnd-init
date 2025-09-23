module Main exposing (..)

import Browser
import Character exposing (Character, viewList)
import Http
import Html exposing (Html, div, h1, h2, text, node)
import Html.Attributes exposing (class, rel, href)
import Json.Decode exposing (Decoder)
import Decode exposing (decodeCharacters)
import Combatant exposing (Combatant, initializeCombatant, viewCombatant)
import Dict exposing (Dict)


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
  , combatants: Dict Int Combatant
  , error: Maybe Http.Error
  }


init : ( Model, Cmd Msg )
init =
  ( Model [] [] Dict.empty Nothing
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
  | AdjustCombatant Int Combatant.Adjustment


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    noCmd newModel =
      ( newModel, Cmd.none )
  
  in
    case msg of
      LoadPcs pcs ->
        noCmd { model | pcs = pcs }

      LoadMonsters monsters ->
        noCmd { model | monsters = monsters }

      HandleError error ->
        noCmd { model | error = Just error }

      SelectCharacter character ->
        initializeCombatant character
          |> ( \combatant -> Dict.insert ( nextKey model.combatants ) combatant model.combatants )
          |> ( \combatants -> { model | combatants = combatants } )
          |> noCmd

      _ ->
        noCmd model


view : Model -> Html Msg
view model =
  case model.error of
    Just _ ->
      div [] [ text "Error" ]

    Nothing ->
      div []
        [ node "link" [ rel "stylesheet", href "/app.css" ] []
        , div [ class "top-structure" ]
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
            :: List.map ( viewCombatant ( AdjustCombatant 0 ) ) ( sortCombatants model.combatants )
            )
          ]
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


nextKey : Dict Int a -> Int
nextKey aDict =
  Dict.keys aDict
    |> List.maximum
    |> Maybe.withDefault 0
    |> ( \key -> key + 1 )


sortCombatants : Dict Int Combatant -> List Combatant
sortCombatants combatants =
  combatants
    |> Dict.values
    |> List.sortBy ( \combatant -> combatant.character.init )
    |> List.reverse