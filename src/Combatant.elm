module Combatant exposing (..)

import Character exposing (Character, viewCharacter)
import Set exposing (Set)
import Html exposing (Html, div, progress, strong, text, br)
import Html.Attributes exposing (attribute, value, class)


type Stance
  = Idle
  | Melee
  | Ranged
  | Casting String


type alias Actions =
  { action : Bool
  , bonusAction : Bool
  , reaction : Bool
  }


initialActions : Actions
initialActions =
  Actions False False False


type alias Combatant =
  { character: Character
  , hp: Int
  , stance: Stance
  , conditions: Set String
  , actions: Actions
  }


initializeCombatant : Character -> Combatant
initializeCombatant character =
  { character = character
  , hp = character.hp
  , stance = Idle
  , conditions = Set.empty
  , actions = initialActions
  }


type Adjustment
  = ResetRound
  | AdjustHp Int
  | SetStance Stance
  | AddCondition String
  | RemoveCondition String
  | UseAction
  | UseBonusAction
  | UseReaction


update : Adjustment -> Combatant -> Combatant
update msg combatant =
  case msg of
    ResetRound ->
      { combatant
      | stance = Idle
      , actions = initialActions
      }

    AdjustHp hp ->
      { combatant
      | hp = hp
      }

    SetStance stance ->
      { combatant
      | stance = stance
      }

    AddCondition condition ->
      combatant.conditions
        |> Set.insert condition
        |> ( \conditions -> { combatant | conditions = conditions } )

    RemoveCondition condition ->
      combatant.conditions
        |> Set.remove condition
        |> ( \conditions -> { combatant | conditions = conditions } )

    UseAction ->
      combatant.actions
        |> ( \actions -> { actions | action = True } )
        |> ( \actions -> { combatant | actions = actions } )

    UseBonusAction ->
      combatant.actions
        |> ( \actions -> { actions | bonusAction  = True } )
        |> ( \actions -> { combatant | actions = actions } )

    UseReaction ->
      combatant.actions
        |> ( \actions -> { actions | reaction   = True } )
        |> ( \actions -> { combatant | actions = actions } )


viewHp : ( Adjustment -> msg ) -> Combatant -> Html msg
viewHp adjust combatant =
  div []
    [ strong [] [ text "Current Hit Points: " ]
    , text ( ( String.fromInt combatant.hp ) ++ " / " ++ ( String.fromInt combatant.character.hp ) )
    , br [] []
    , progress [ attribute "max" ( String.fromInt combatant.character.hp ), value ( String.fromInt combatant.hp ) ] []
    ]


viewCombatant : ( Adjustment -> msg ) -> Combatant -> Html msg
viewCombatant adjust combatant =
  div [ class "card" ]
    [ viewCharacter combatant.character
    , viewHp adjust combatant 
    ]
