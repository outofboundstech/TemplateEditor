module Main exposing (main)


import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events

import Markdown

import Regex as Re

type alias Flags =
  { template : String
  }



type alias Template = String


type Field =
  Field
    { token : String
    , question : String
    , answer : String
    , type_ : String
    , placeholder : String
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
  Html.section [ Attr.id "template-editor", Attr.class "flex two" ]
    [ Html.div [ Attr.id "edit" ]
      [ Html.form [ ]
        ( List.map (\field -> fieldView field) fields )
      ]
    , Html.div [ Attr.id "preview" ]
      [ preview template fields ]
    ]


fieldView : Field -> Html Msg
fieldView (Field field) =
  Html.label [ ]
    [ Html.text field.question
    , if "textarea" == field.type_ then
        Html.textarea
        [ Events.onInput (UpdateField (Field field))
        , Attr.value field.answer
        , Attr.placeholder field.placeholder
        ] [ ]
      else
        Html.input
        [ Events.onInput (UpdateField (Field field))
        , Attr.type_ field.type_
        , Attr.value field.answer
        , Attr.placeholder field.placeholder
        ] [ ]
    ]


preview : Template -> List Field -> Html Msg
preview template fields =
  let
    withDefault = (\s def -> if String.isEmpty s then def else s)
    final = List.foldl (\(Field field) result ->
        -- Use Regex.replace if String.split proves too unflexible; I may need
        --   mode control over field.t in order to pull that of.
        -- Re.replace (Re.AtMost 1) field.re (\_ -> field.a) result
        String.split field.token result
          |> String.join (withDefault field.answer "&hellip;")
      ) template fields
  in
    Markdown.toHtml [ ] final


{-| update
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( Model model ) =
  case msg of
    UpdateField field val ->
      let
        fields = List.map (\(Field f) ->
            if (Field f) == field then
              Field { f | answer = val }
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
        -- utility function for extracting submatches
        submatch = (\i default ->
            match.submatches
            |> List.drop i
            |> List.head
            |> Maybe.withDefault Nothing
            |> Maybe.withDefault default
          )
        attrs = (\list i default ->
            list
            |> List.drop i
            |> List.head
            |> Maybe.withDefault ""
            |> (\val -> if (String.isEmpty val) then default else val)
          ) (String.split ":" (submatch 1 ""))
        -- value assignment
        token = match.match
        question = submatch 0 ""
        type_ = attrs 0 "text"
        value = attrs 1 ""
        placeholder = attrs 2 ""
        -- type_ = attrs |> List.drop 0 |> List.head |> Maybe.withDefault "text"
        -- placeholder = attrs |> List.drop 1 |> List.head |> Maybe.withDefault ""
      in
        Field
          { token = token
          , question = question
          , answer = value
          , type_ = type_
          , placeholder = placeholder
          }

      ) (Re.find Re.All regex str)
  in
    ( str, fields )


-- I'll need to construct a better regular expression, preferably one that's
--   unicode aware. More information on MDN at:
-- https://developer.mozilla.org/en/docs/Web/JavaScript/Guide/Regular_Expressions
regex : Re.Regex
regex =
  Re.regex "\\[=(.+?)\\](?:\\((.+?)\\))?"
  -- Re.regex "\\[=(.+?)\\](?:\\((.+?)\\))?"
