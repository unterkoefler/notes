module Main exposing (..)

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events exposing (onResize)
import Colors
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
import Http
import Json.Decode
import Json.Encode
import Ports
import Prng.Uuid as Uuid exposing (Uuid)
import Random.Pcg.Extended as Random
import Task
import Time



-- MAIN


main : Program Json.Decode.Value Model Msg
main =
    Browser.document
        { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { notes : List Note
    , currentNoteId : Maybe Uuid
    , showSidebar : Bool
    , dimensions : Dimensions
    , seed : Random.Seed
    , isSyncing : Bool
    , errorMessage : Maybe String
    }


type alias Dimensions =
    { width : Int
    , height : Int
    }


type alias Note =
    { contents : String
    , id : Uuid
    , title : String
    , lastSynced : Maybe Time.Posix
    }


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        model =
            parseFlags flags
    in
    ( model
    , Cmd.none
    )


defaultModel : Dimensions -> Random.Seed -> Bool -> Maybe String -> Model
defaultModel dim seed isSyncing errorMessage =
    { notes = []
    , showSidebar = True
    , currentNoteId = Nothing
    , dimensions = dim
    , seed = seed
    , isSyncing = isSyncing
    , errorMessage = errorMessage
    }


parseFlags : Json.Decode.Value -> Model
parseFlags flags =
    let
        dimensions =
            Json.Decode.decodeValue dimensionsDecoder flags
                |> Result.withDefault defaultDimensions

        seedInt =
            Json.Decode.decodeValue (Json.Decode.field "seed" Json.Decode.int) flags
                |> Result.withDefault 0

        seedExtension =
            Json.Decode.decodeValue (Json.Decode.field "seedExtension" (Json.Decode.list Json.Decode.int)) flags
                |> Result.withDefault []

        seed =
            Random.initialSeed seedInt seedExtension

        makeModel =
            Json.Decode.decodeValue (Json.Decode.field "data" modelDecoder) flags
                |> Result.withDefault defaultModel
    in
    makeModel dimensions seed False Nothing


dimensionsDecoder : Json.Decode.Decoder Dimensions
dimensionsDecoder =
    Json.Decode.map2 Dimensions
        (Json.Decode.field "width" Json.Decode.int)
        (Json.Decode.field "height" Json.Decode.int)


defaultDimensions =
    { width = 500
    , height = 500
    }


noteDecoder : Json.Decode.Decoder Note
noteDecoder =
    Json.Decode.map4 Note
        (Json.Decode.field "contents" Json.Decode.string)
        (Json.Decode.field "id" Uuid.decoder)
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "lastSynced" (Json.Decode.nullable (Json.Decode.map Time.millisToPosix Json.Decode.int)))


noteEncoder : Note -> Json.Encode.Value
noteEncoder note =
    Json.Encode.object
        [ ( "id", Uuid.encode note.id )
        , ( "title", Json.Encode.string note.title )
        , ( "contents", Json.Encode.string note.contents )
        , ( "lastSynced", Maybe.withDefault Json.Encode.null <| Maybe.map (Json.Encode.int << Time.posixToMillis) note.lastSynced )
        ]


modelDecoder : Json.Decode.Decoder (Dimensions -> Random.Seed -> Bool -> Maybe String -> Model)
modelDecoder =
    Json.Decode.map3 Model
        (Json.Decode.field "notes" (Json.Decode.list noteDecoder))
        (Json.Decode.field "currentNoteId" (Json.Decode.nullable Uuid.decoder))
        (Json.Decode.field "showSidebar" Json.Decode.bool)


modelEncoder : Model -> Json.Encode.Value
modelEncoder model =
    Json.Encode.object
        [ ( "notes", Json.Encode.list noteEncoder model.notes )
        , ( "showSidebar", Json.Encode.bool model.showSidebar )
        , ( "currentNoteId"
          , model.currentNoteId
                |> Maybe.map Uuid.encode
                |> Maybe.withDefault Json.Encode.null
          )
        ]



-- UPDATE


type Msg
    = WindowResized Dimensions
    | AddNote
    | EditNoteContents Uuid String
    | EditNoteTitle Uuid String
    | SelectNote Uuid
    | CloseNote
    | ToggleSidebar
    | SaveData Time.Posix
    | SyncData
    | SyncedData (Result Http.Error ( List Note, List Uuid ))
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResized newDim ->
            ( { model | dimensions = newDim }, Cmd.none )

        AddNote ->
            let
                ( n, newSeed ) =
                    newNote model
            in
            ( { model
                | notes = n :: model.notes
                , currentNoteId = Just n.id
                , seed = newSeed
              }
            , focusElement noteContentsInputId
            )

        EditNoteContents id newContents ->
            ( { model | notes = editNoteContents id newContents model.notes }, Cmd.none )

        EditNoteTitle id newTitle ->
            ( { model | notes = editNoteTitle id newTitle model.notes }, Cmd.none )

        SelectNote id ->
            ( { model | currentNoteId = Just id }, Cmd.none )

        CloseNote ->
            ( { model | currentNoteId = Nothing }, Cmd.none )

        ToggleSidebar ->
            ( { model | showSidebar = not model.showSidebar }, Cmd.none )

        SaveData _ ->
            ( model, Ports.saveData <| modelEncoder model )

        SyncData ->
            ( { model | isSyncing = True }, beginSync model.notes [] )

        -- TODO: store deleted note ids
        SyncedData (Ok ( notes, _ )) ->
            ( { model | notes = notes, isSyncing = False }, Cmd.none )

        SyncedData (Err e) ->
            let
                _ =
                    Debug.log "error syncing" e
            in
            ( { model | errorMessage = Just <| Debug.toString e }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


focusElement : String -> Cmd Msg
focusElement id =
    Task.attempt (\_ -> NoOp) (Dom.focus id)


newNote : Model -> ( Note, Random.Seed )
newNote model =
    let
        ( id, newSeed ) =
            Random.step Uuid.generator model.seed

        title =
            "Note " ++ String.fromInt (List.length model.notes + 1)
    in
    ( { id = id
      , contents = ""
      , title = title
      , lastSynced = Nothing
      }
    , newSeed
    )


editNoteContents : Uuid -> String -> List Note -> List Note
editNoteContents id newContents notes =
    let
        ( maybeNote, notesWithoutCurrent ) =
            findWithRest (\n -> n.id == id) notes
    in
    case maybeNote of
        Nothing ->
            notes

        Just noteToEdit ->
            { noteToEdit | contents = newContents } :: notesWithoutCurrent


editNoteTitle : Uuid -> String -> List Note -> List Note
editNoteTitle id newTitle notes =
    let
        ( maybeNote, notesWithoutCurrent ) =
            findWithRest (\n -> n.id == id) notes
    in
    case maybeNote of
        Nothing ->
            notes

        Just noteToEdit ->
            { noteToEdit | title = newTitle } :: notesWithoutCurrent


findNote : Uuid -> List Note -> Maybe Note
findNote id notes =
    notes
        |> List.filter (\n -> n.id == id)
        |> List.head


findWithRest : (a -> Bool) -> List a -> ( Maybe a, List a )
findWithRest pred xs =
    findWithRestHelp pred xs []


findWithRestHelp : (a -> Bool) -> List a -> List a -> ( Maybe a, List a )
findWithRestHelp pred xs acc =
    case xs of
        [] ->
            ( Nothing, acc )

        x :: rest ->
            if pred x then
                ( Just x, acc ++ rest )

            else
                findWithRestHelp pred rest (x :: acc)


beginSync : List Note -> List Uuid -> Cmd Msg
beginSync notes deletedNoteIds =
    Http.post
        { url = "http://192.168.68.62:8001/sync" -- TODO: make configurable
        , body = Http.jsonBody <| syncDataEncoder notes deletedNoteIds
        , expect = Http.expectJson SyncedData syncedDataDecoder
        }


syncDataEncoder : List Note -> List Uuid -> Json.Encode.Value
syncDataEncoder notes deletedNoteIds =
    Json.Encode.object
        [ ( "notes", Json.Encode.list noteEncoder notes )
        , ( "deletedNotes", Json.Encode.list Uuid.encode deletedNoteIds )
        ]


syncedDataDecoder : Json.Decode.Decoder ( List Note, List Uuid )
syncedDataDecoder =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "notes" (Json.Decode.list noteDecoder))
        (Json.Decode.field "conflicts" (Json.Decode.list Uuid.decoder))



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Notes"
    , body = [ getBody model ]
    }


type ScreenSize
    = Mobile
    | Desktop


getScreenSize : Dimensions -> ScreenSize
getScreenSize dim =
    let
        { class, orientation } =
            classifyDevice dim
    in
    case ( class, orientation ) of
        ( Phone, Portrait ) ->
            Mobile

        ( Tablet, Portrait ) ->
            Mobile

        _ ->
            Desktop


baseFontSize =
    12


columnSpacing =
    30


basePadding =
    { top = 30, right = 5, left = 15, bottom = 0 }


baseSpacing =
    12


buttonFontSize =
    24


getBody : Model -> Html Msg
getBody model =
    let
        currentNote : Maybe Note
        currentNote =
            Maybe.andThen (\id -> findNote id model.notes) model.currentNoteId
    in
    Element.layoutWith
        { options =
            [ focusStyle
                { borderColor = Just Colors.white
                , backgroundColor = Nothing
                , shadow = Nothing --Just { color = Colors.secondary, offset = (0, 0), blur = 1, size = 1 }
                }
            ]
        }
        [ Font.size baseFontSize
        ]
    <|
        case currentNote of
            Nothing ->
                column
                    [ spacing 16
                    , width fill
                    , centerX
                    ]
                    [ header model
                    , column
                        [ width (pct 85 model.dimensions.width)
                        , centerX
                        , spacing 16
                        ]
                        [ row
                            [ spacing 48
                            , width fill
                            , paddingXY 24 0
                            ]
                            [ squareButton model.dimensions (Just AddNote) "Add note" Colors.primary
                            , syncDataButton model.dimensions model.isSyncing squareButton
                            ]
                        , case model.errorMessage of
                            Just err ->
                                text err

                            Nothing ->
                                Element.none
                        , el
                            [ paddingXY 24 0
                            ]
                          <|
                            notePreviews model.dimensions Home model.notes
                        ]
                    ]

            Just n ->
                row
                    [ height fill
                    , width fill
                    , spacing 24
                    ]
                    [ sidebar model.dimensions model.showSidebar model.isSyncing model.notes
                    , noteEditor model.dimensions n
                    ]


header : Model -> Element Msg
header model =
    row
        [ width fill
        , Background.color Colors.primary
        , Font.color Colors.white
        , Font.size 36
        , paddingXY 18 12
        ]
        [ paragraph
            []
            [ text "Notey Notes" ]
        ]


sidebar : Dimensions -> Bool -> Bool -> List Note -> Element Msg
sidebar dims showSidebar isSyncing notes =
    if not showSidebar then
        sidebarToggler showSidebar

    else
        let
            w =
                pct 25 dims.width |> maximum 300 |> minimum 160
        in
        row
            [ width w
            , height fill
            ]
            [ column
                [ spacing 8
                , alignTop
                , clipX
                , width fill
                ]
                [ Input.button
                    [ width fill
                    , Background.color Colors.primary
                    , Font.color Colors.white
                    , Font.size 24
                    , paddingXY 12 24
                    ]
                    { label = text "Notey Notes"
                    , onPress = Just CloseNote
                    }
                , column
                    [ paddingXY 24 24
                    , spacing 16
                    , width fill
                    ]
                    [ rectangleButton dims (Just AddNote) "Add note" Colors.primary
                    , syncDataButton dims isSyncing rectangleButton
                    , notePreviews dims Sidebar notes
                    ]
                ]
            , sidebarToggler showSidebar
            ]


type PreviewContext
    = Sidebar
    | Home


notePreviews : Dimensions -> PreviewContext -> List Note -> Element Msg
notePreviews dims ctx notes =
    column
        [ spacing 12
        , scrollbarY
        , height (px 800)
        , width fill
        ]
        (List.map (notePreview dims ctx) notes)


syncDataButton : Dimensions -> Bool -> (Dimensions -> Maybe Msg -> String -> Color -> Element Msg) -> Element Msg
syncDataButton dims isSyncing buttonF =
    if isSyncing then
        buttonF dims Nothing "syncing..." Colors.highlight

    else
        buttonF dims (Just SyncData) "Sync" Colors.highlight


sidebarToggler : Bool -> Element Msg
sidebarToggler showSidebar =
    let
        label =
            if showSidebar then
                "<"

            else
                ">"
    in
    Input.button
        [ height fill
        , Border.color Colors.secondary
        , Border.widthEach { bottom = 0, top = 0, left = 0, right = 1 }

        --        , padding 12
        , inFront <|
            Input.button
                [ Region.description "toggle sidebar"
                , Border.width 1
                , Border.rounded 500
                , paddingXY 12 10
                , moveLeft 15
                , moveDown 250
                , Font.color Colors.secondary
                , Border.color Colors.secondary
                , Background.color <| rgba 1.0 1.0 1.0 1.0
                ]
                { label = text label
                , onPress = Nothing --, onPress = Just ToggleSidebar
                }
        ]
        { label = Element.none
        , onPress = Just ToggleSidebar
        }


squareButton : Dimensions -> Maybe Msg -> String -> Element.Color -> Element Msg
squareButton dims onPress lbl color =
    let
        w =
            pct 30 dims.width |> maximum 300
    in
    Input.button
        [ Border.color color
        , Border.width 1
        , paddingXY 6 12
        , Font.center
        , Font.size buttonFontSize
        , Font.color color
        , Border.rounded 6
        , width w
        , height w
        ]
        { onPress = onPress
        , label = paragraph [ width shrink, centerX ] [ text lbl ]
        }


rectangleButton : Dimensions -> Maybe Msg -> String -> Element.Color -> Element Msg
rectangleButton dims onPress lbl color =
    Input.button
        [ Border.color color
        , Border.width 1
        , paddingXY 6 12
        , Font.center
        , Font.size buttonFontSize
        , Font.color color
        , Border.rounded 6
        , width fill
        ]
        { onPress = onPress
        , label = paragraph [] [ text lbl ]
        }


noteEditor : Dimensions -> Note -> Element Msg
noteEditor dims { id, contents, title } =
    let
        w =
            pct 60 dims.width |> maximum 800 |> minimum 300
    in
    column
        [ spacing 6
        , paddingXY 12 24
        , width w
        , centerX
        , height fill
        ]
        [ row
            [ width fill
            ]
            [ Input.text
                [ Font.size 28
                ]
                { label = Input.labelHidden "Note title"
                , onChange = EditNoteTitle id
                , placeholder = Nothing
                , text = title
                }
            , Input.button
                [ alignRight
                , padding 6
                ]
                { label = text "delete"
                , onPress = Nothing -- TODO
                }
            ]
        , Input.multiline
            [ width fill
            , height fill
            , Input.focusedOnLoad
            , htmlAttribute <| Html.Attributes.id noteContentsInputId
            , Font.size 14
            ]
            { label = Input.labelHidden "Note contents"
            , onChange = EditNoteContents id
            , placeholder = Just (Input.placeholder [] (text "The horrors of a blank page..."))
            , spellcheck = True
            , text = contents
            }
        ]


noteContentsInputId =
    "notes-content"


notePreview : Dimensions -> PreviewContext -> Note -> Element Msg
notePreview dims ctx { id, contents, title } =
    let
        label =
            column
                [ spacing 6
                , width fill
                ]
                [ paragraph
                    [ Font.size 18
                    , width fill
                    ]
                    [ text title ]
                , el
                    [ clip
                    , height (px 16)
                    , Font.color Colors.gray
                    , width fill
                    ]
                    (text <| trimContents dims ctx contents)
                ]
    in
    Input.button
        []
        { label = label
        , onPress = Just <| SelectNote id
        }


trimContents : Dimensions -> PreviewContext -> String -> String
trimContents dims ctx s =
    let
        width =
            case ctx of
                Sidebar ->
                    (25 * dims.width // 100) - 24

                -- TODO abstract
                Home ->
                    (75 * dims.width // 100) - 48

        breakpoint =
            floor (0.25 * toFloat width)

        -- TODO: fix
    in
    if String.length s < breakpoint then
        s

    else
        String.left (breakpoint - 4) s ++ "..."


pct : Int -> Int -> Length
pct p x =
    px (x * p // 100)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize (\w h -> WindowResized { width = w, height = h })
        , Time.every (1 * 1000) SaveData
        ]
