module Main exposing (main)


import Regex as Re

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events


type alias Flags =
  { template : String
  }



type alias Template = String


type Field =
  Field
    { t : String
    , q : String
    , a : String
    }


type Model =
  Model
    { template : Template
    , fields : List Field
    }


type Msg
  = UpdateField Field String


{-| init
-}
init : Flags -> ( Model, Cmd Msg )
init flags =
  let
    -- Assign model values
    ( template, fields ) = parseTemplate flags.template
    -- Construct model object
    model = Model
      { template = template
      , fields = fields
      }
  in
    model ! [ Cmd.none ]


{-| view
-}
view : Model -> Html Msg
view ( Model { template, fields } ) =
  Html.section [ Attr.id "template-editor" ]
    [ Html.div [ Attr.id "edit" ]
      [ Html.form [ ]
        ( List.map (\field -> fieldView field) fields )
      ]
    , Html.div [ Attr.id "preview" ] [ preview template fields ]
    ]


fieldView : Field -> Html Msg
fieldView (Field field) =
  Html.div [ ]
    [ Html.label [ ] [ Html.text field.q ]
    , Html.input
      [ Events.onInput (UpdateField (Field field))
      ] [ ]
    ]


{-| update
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( Model model ) =
  case msg of
    UpdateField field val ->
      let
        fields = List.map (\(Field f) ->
            if (Field f) == field then
              Field { f | a = val }
            else
              Field f
          ) model.fields
      in
        ( Model { model | fields = fields } ) ! [ Cmd.none ]


{-| main
-}
main : Program Flags Model Msg
main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
    }


-- Helper functions

parseTemplate : String -> ( Template, List Field )
parseTemplate str =
  let
    fields = List.map (\match ->
      let
        t = match.match
        q = match.submatches
          |> List.head
          |> Maybe.withDefault Nothing
          |> Maybe.withDefault "Question not set..."
      in
        Field { t = t, q = q , a = "" }

      ) (Re.find Re.All regex str)
  in
    ( str, fields )


preview : Template -> List Field -> Html Msg
preview template fields =
  let
    final = List.foldl (\(Field field) result ->
        -- Use Regex.replace if String.split proves too unflexible; I may need
        --   mode control over field.t in order to pull that of.
        -- Re.replace (Re.AtMost 1) field.re (\_ -> field.a) result
        String.split field.t result
          |> String.join field.a
      ) template fields
  in
    -- Switch to markdown rendering
    Html.text final


-- I'll need to construct a better regular expression, preferably one that's
--   unicode aware. More information on MDN at:
-- https://developer.mozilla.org/en/docs/Web/JavaScript/Guide/Regular_Expressions
regex : Re.Regex
regex =
  Re.regex "<=([\\w\\s]*)>"
