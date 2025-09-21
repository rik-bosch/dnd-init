module Character exposing (..)

import Html exposing (Html, ul, li, br, strong, text)
import Html.Events exposing (onClick)


type alias Character =
  { name: String
  , ac: Int
  , hp: Int
  , init: Int
  }


viewList : ( Character -> msg ) -> List Character -> Html msg
viewList selectCharacter characters =
  ul []
    ( characters |> List.map
      ( \character ->
        li [ onClick ( selectCharacter character ) ]
          [ text character.name
          , br [] []
          , strong [] [ text "AC " ]
          , text ( String.fromInt character.ac )
          , strong [] [ text "\tHP " ]
          , text ( String.fromInt character.hp )
          , strong [] [ text "\tInit " ]
          , text ( String.fromInt character.init )
          ]
      )
    )
