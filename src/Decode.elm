module Decode exposing (..)

import Json.Decode exposing (..)
import Character exposing (Character)


decodeCharacter : Decoder Character
decodeCharacter =
  map4 Character
    ( field "name" string )
    ( field "armor-class" int )
    ( field "hit-points" int )
    ( field "initiative" int )


decodeCharacters : Decoder (List Character)
decodeCharacters =
  list decodeCharacter
