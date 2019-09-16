module Main exposing (main)

import Browser
import Html exposing (Html, button, div, form, input, li, span, text, ul)
import Html.Attributes exposing (class, id, style, type_, value)
import Html.Events exposing (..)


type alias Model =
    { tasks : List Task
    , textEntered : String
    }


type TaskStatus
    = Active
    | Done
    | Urgent


type alias Task =
    { label : String, status : TaskStatus }


initialModel : Model
initialModel =
    { tasks =
        [ { label = "Play Foot-Ball", status = Urgent }
        , { label = "Do homeword", status = Done }
        , { label = "Watch a movie", status = Active }
        ]
    , textEntered = ""
    }


type Msg
    = ClikedOnDoneButton Int
    | TextEntered String
    | TaskSubmitted


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClikedOnDoneButton idx ->
            let
                transformTask : Int -> Task -> Task
                transformTask i task =
                    if idx == i then
                        { task | status = Done }

                    else
                        task
            in
            { model
                | tasks = List.indexedMap transformTask model.tasks
            }

        TextEntered entered ->
            { model | textEntered = entered }

        TaskSubmitted ->
            { model
                | tasks = { label = model.textEntered, status = Active } :: model.tasks
                , textEntered = ""
            }


viewTask : Int -> Task -> Html Msg
viewTask id task =
    case task.status of
        Active ->
            li []
                [ text task.label
                , button [ onClick <| ClikedOnDoneButton id ] [ text "Done!" ]
                ]

        Done ->
            li [ class "done" ] [ text task.label ]

        Urgent ->
            li [ class "urgent" ]
                [ text (task.label ++ "⚠️")
                , button [ onClick <| ClikedOnDoneButton id ] [ text "Done!" ]
                ]


view : Model -> Html Msg
view model =
    div []
        [ form [ onSubmit TaskSubmitted ]
            [ input [ id "txt-entered", onInput TextEntered, value model.textEntered ] []
            , input [ id "btn-add", type_ "submit", value "+" ] []
            ]
        , text (String.fromInt (List.length model.tasks) ++ " TASKS TO DO:")
        , ul []
            (List.indexedMap viewTask model.tasks)
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
