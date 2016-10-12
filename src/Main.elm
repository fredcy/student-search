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


type alias Id =
    String


type alias Model =
    { students : List Student
    , selected : Maybe Id
    , query : String
    , showMenu : Bool
    , autoState : Autocomplete.State
    }


type Msg
    = NoOp
    | Error Http.Error
    | GotStudents (List Student)
    | SetQuery String
    | SelectStudent Id
    | PreviewStudent Id
    | AutocompleteMsg Autocomplete.Msg
    | HandleEscape
    | Wrap Bool
    | Reset


main =
    Html.App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( Model [] Nothing "" False Autocomplete.empty
    , Task.perform Error GotStudents getStudents
    )


howMany =
    6


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg |> Debug.log "msg" of
        GotStudents students ->
            { model | students = students } ! []

        Error httpError ->
            --TODO
            model ! []

        SetQuery query ->
            let
                showMenu =
                    not << List.isEmpty <| matchedStudents query model.students
            in
                { model | query = query, selected = Nothing, showMenu = showMenu } ! []

        AutocompleteMsg autoMsg ->
            let
                choices =
                    matchedStudents model.query model.students

                ( newState, maybeMsg ) =
                    Autocomplete.update updateConfig autoMsg howMany model.autoState choices

                newModel =
                    { model | autoState = newState }
            in
                case maybeMsg of
                    Nothing ->
                        newModel ! []

                    Just updateMsg ->
                        update updateMsg newModel

        SelectStudent id ->
            ( setQuery model id |> resetMenu, Cmd.none )

        PreviewStudent id ->
            { model | selected = Just id } ! []

        Reset ->
            { model | autoState = Autocomplete.reset updateConfig model.autoState, selected = Nothing } ! []

        Wrap toTop ->
            case model.selected of
                Just _ ->
                    update Reset model

                Nothing ->
                    let
                        choices =
                            matchedStudents model.query model.students
                    in
                        if toTop then
                            { model
                                | autoState = Autocomplete.resetToLastItem updateConfig choices howMany model.autoState
                                , selected = Maybe.map toId <| List.head <| List.reverse <| List.take howMany choices
                            }
                                ! []
                        else
                            { model
                                | autoState = Autocomplete.resetToFirstItem updateConfig choices howMany model.autoState
                                , selected = Maybe.map toId <| List.head <| List.take howMany choices
                            }
                                ! []

        HandleEscape ->
            let
                haveMatches =
                    (not << List.isEmpty) (matchedStudents model.query model.students)

                handleEscape =
                    if haveMatches then
                        model |> removeSelection |> resetMenu
                    else
                        { model | query = "" } |> removeSelection |> resetMenu

                escapeModel =
                    case model.selected of
                        Just person ->
                            if model.query == person then
                                model |> resetInput
                            else
                                handleEscape

                        Nothing ->
                            handleEscape
            in
                escapeModel ! []

        _ ->
            model ! []


setQuery : Model -> Id -> Model
setQuery model id =
    { model | query = id, selected = Just id }


resetMenu : Model -> Model
resetMenu model =
    { model | autoState = Autocomplete.empty, showMenu = False }


removeSelection model =
    { model | selected = Nothing }


resetInput model =
    { model | query = "" } |> removeSelection |> resetMenu


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ viewSelected model.selected
        , viewAutocomplete model
        ]


viewSelected : Maybe Id -> Html.Html Msg
viewSelected idMaybe =
    Html.div [] [ Html.text (Maybe.withDefault "" idMaybe) ]


viewAutocomplete : Model -> Html.Html Msg
viewAutocomplete model =
    let
        autoStyle =
            [ ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "height", "200px" )
            , ( "width", "200px" )
            ]

        options =
            { preventDefault = True, stopPropagation = False }

        decodeKey =
            Json.customDecoder HE.keyCode
                (\code ->
                    let
                        _ =
                            Debug.log "code" code
                    in
                        if code == 38 || code == 40 then
                            Ok NoOp
                        else if code == 27 then
                            Ok HandleEscape
                        else
                            Err "not handling that key"
                )

        inputValue =
            case model.selected of
                Nothing ->
                    model.query

                Just id ->
                    id

        choices =
            matchedStudents model.query model.students

        menu =
            if model.showMenu then
                Html.div [ HA.class "autocomplete-menu" ]
                    [ Html.App.map AutocompleteMsg (Autocomplete.view viewConfig howMany model.autoState choices)
                    ]
            else
                Html.text ""
    in
        Html.div [ HA.style autoStyle ]
            [ Html.input
                [ HE.onInput SetQuery
                , HA.class "autocomplete-input"
                , HA.value inputValue
                , HE.onWithOptions "keydown" options decodeKey
                ]
                []
            , menu
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map AutocompleteMsg Autocomplete.subscription


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


enterKey =
    13


arrowUpKey =
    38


arrowDownKey =
    40


updateConfig : Autocomplete.UpdateConfig Msg Student
updateConfig =
    let
        handleKey code maybeId =
            let
                _ =
                    Debug.log "key" ( code, maybeId )
            in
                if code == enterKey then
                    Maybe.map SelectStudent maybeId
                else if code == arrowUpKey || code == arrowDownKey then
                    Maybe.map PreviewStudent maybeId
                else
                    Just Reset
    in
        Autocomplete.updateConfig
            { toId = toId
            , onKeyDown = handleKey
            , onTooLow = Just (Wrap False)
            , onTooHigh = Just (Wrap True)
            , onMouseEnter = Just << PreviewStudent
            , onMouseLeave = \_ -> Nothing
            , onMouseClick = Just << SelectStudent
            , separateSelections = False
            }


viewConfig : Autocomplete.ViewConfig Student
viewConfig =
    let
        customizedLi keySelected mouseSelected student =
            { attributes =
                [ HA.classList
                    [ ( "autocomplete-item", True )
                    , ( "key-selected", keySelected || mouseSelected )
                    ]
                ]
            , children = [ Html.text (toId student) ]
            }
    in
        Autocomplete.viewConfig
            { toId = toId
            , ul = [ HA.class "autocomplete-list" ]
            , li = customizedLi
            }


toId : Student -> Id
toId student =
    student.lastName ++ ", " ++ student.firstName


matchedStudents : String -> List Student -> List Student
matchedStudents query students =
    let
        queryLower =
            String.toLower query
    in
        List.filter (String.contains queryLower << String.toLower << toId) students
