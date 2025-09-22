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


resetRound : Combatant -> Combatant
resetRound combatant =
  { combatant
  | stance = Idle
  , actions = initialActions
  }


adjustHp : Int -> Combatant -> Combatant
adjustHp hp combatant =
  { combatant
  | hp = hp
  }


setStance : Stance -> Combatant -> Combatant
setStance stance combatant =
  { combatant
  | stance = stance
  }


addCondition : String -> Combatant -> Combatant
addCondition condition combatant =
  combatant.conditions
    |> Set.insert condition
    |> ( \conditions -> { combatant | conditions = conditions } )


removeCondition : String -> Combatant -> Combatant
removeCondition condition combatant =
  combatant.conditions
    |> Set.remove condition
    |> ( \conditions -> { combatant | conditions = conditions } )


useAction : Combatant -> Combatant
useAction combatant =
  combatant.actions
    |> ( \actions -> { actions | action = True } )
    |> ( \actions -> { combatant | actions = actions } )


useBonusAction : Combatant -> Combatant
useBonusAction combatant =
  combatant.actions
    |> ( \actions -> { actions | bonusAction  = True } )
    |> ( \actions -> { combatant | actions = actions } )


useReaction : Combatant -> Combatant
useReaction  combatant =
  combatant.actions
    |> ( \actions -> { actions | reaction   = True } )
    |> ( \actions -> { combatant | actions = actions } )


viewHp : Combatant -> Html msg
viewHp combatant =
  div []
    [ strong [] [ text "Current Hit Points: " ]
    , text ( ( String.fromInt combatant.hp ) ++ " / " ++ ( String.fromInt combatant.character.hp ) )
    , br [] []
    , progress [ attribute "max" ( String.fromInt combatant.character.hp ), value ( String.fromInt combatant.hp ) ] []
    ]


viewCombatant : Combatant -> Html msg
viewCombatant combatant =
  div [ class "card" ]
    [ viewCharacter combatant.character
    , viewHp combatant 
    ]
