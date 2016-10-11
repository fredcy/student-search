module Main exposing (main)

import Html
import Html.App
import Html.Events as HE
import Html.Attributes as HA
import Http
import Json.Decode as Json exposing ((:=))
import String
import Task exposing (Task)
import Autocomplete


type alias Student =
    { firstName : String
    , lastName : String
    }


type alias Model =
    { students : List Student
    , query : String
    , autoState : Autocomplete.State
    }


type Msg
    = NoOp
    | Error Http.Error
    | GotStudents (List Student)
    | SetQuery String
    | SelectStudent String
    | AutocompleteMsg Autocomplete.Msg


main =
    Html.App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( Model [] "" Autocomplete.empty
    , Task.perform Error GotStudents getStudents
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg |> Debug.log "msg" of
        GotStudents students ->
            { model | students = students } ! []

        SetQuery query ->
            { model | query = query } ! []

        AutocompleteMsg autoMsg ->
            let
                ( newState, maybeMsg ) =
                    Autocomplete.update updateConfig autoMsg 7 model.autoState (matchedStudents model.query model.students)
            in
                { model | autoState = newState } ! []

        _ ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.input [ HE.onInput SetQuery, HA.class "autocomplete-input" ] []
        , Html.div [ HA.class "autocomplete-menu" ]
            [ Html.App.map AutocompleteMsg (Autocomplete.view viewConfig 7 model.autoState (matchedStudents model.query model.students))
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


getStudents : Task Http.Error (List Student)
getStudents =
    Http.get studentsDecoder "http://app.devnet.imsa.edu:9080/students"


studentsDecoder : Json.Decoder (List Student)
studentsDecoder =
    let
        student =
            Json.object2 Student
                ("FirstName" := Json.string)
                ("LastName" := Json.string)
    in
        Json.list student


updateConfig : Autocomplete.UpdateConfig Msg Student
updateConfig =
    Autocomplete.updateConfig
        { toId = toId
        , onKeyDown =
            \code maybeId ->
                if code == 13 then
                    Maybe.map SelectStudent maybeId
                else
                    Nothing
        , onTooLow = Nothing
        , onTooHigh = Nothing
        , onMouseEnter = \_ -> Nothing
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \id -> Just <| SelectStudent id
        , separateSelections = False
        }


viewConfig : Autocomplete.ViewConfig Student
viewConfig =
    let
        customizedLi keySelected mouseSelected student =
            { attributes = [ HA.classList [ ( "autocomplete-item", True ), ( "is-selected", keySelected || mouseSelected ) ] ]
            , children = [ Html.text (toId student) ]
            }
    in
        Autocomplete.viewConfig
            { toId = toId
            , ul = [ HA.class "autocomplete-list" ]
            , li = customizedLi
            }


toId : Student -> String
toId student =
    student.lastName ++ ", " ++ student.firstName


matchedStudents : String -> List Student -> List Student
matchedStudents query students =
    let
        queryLower = String.toLower query
    in
        List.filter (String.contains queryLower << String.toLower << toId) students
