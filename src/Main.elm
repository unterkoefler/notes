module Main exposing (..)

import Browser exposing (Document)
import Browser.Events exposing (onResize)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Http
import Json.Decode
import Json.Encode
import Ports
import Prng.Uuid as Uuid exposing (Uuid)
import Random.Pcg.Extended as Random
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
            , Cmd.none
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
            ( model, Cmd.none )

        -- ( model, Ports.saveData <| modelEncoder model )
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


headingFontSize =
    2 * baseFontSize


buttonFontSize =
    24


headingSpacing =
    2 * baseSpacing


buttonSpacing =
    5


columnWidth : ScreenSize -> Int -> Length
columnWidth screenSize w =
    let
        max =
            w - (2 * (basePadding.left + basePadding.right))
    in
    case screenSize of
        Mobile ->
            px 400
                |> maximum max

        Desktop ->
            px 600
                |> maximum max


headingColor =
    Element.rgb 0.2 0.34 0.98


green =
    Element.rgb 0.4 0.78 0.4


purple =
    Element.rgb 0.61 0.33 0.88


teal =
    Element.rgb 0.4 0.78 0.8


getBody : Model -> Html Msg
getBody model =
    let
        screenSize =
            getScreenSize model.dimensions

        currentNote : Maybe Note
        currentNote =
            Maybe.andThen (\id -> findNote id model.notes) model.currentNoteId
    in
    Element.layout
        [ Font.size baseFontSize
        ]
    <|
        case currentNote of
            Nothing ->
                column
                    [ spacing 16
                    , width fill
                    , centerX
                    , centerX
                    , paddingXY 12 16
                    , paddingXY 12 16
                    ]
                    [ newNoteButton
                    , syncDataButton model.isSyncing
                    , case model.errorMessage of
                        Just err ->
                            text err

                        Nothing ->
                            Element.none
                    , notePreviews model.notes
                    ]

            Just n ->
                row
                    [ height fill
                    , width fill
                    , spacing 24
                    ]
                    [ sidebar model.showSidebar model.isSyncing model.notes
                    , noteEditor n
                    ]


responsiveColumnAttributes : ScreenSize -> List (Attribute Msg)
responsiveColumnAttributes screenSize =
    case screenSize of
        Mobile ->
            []

        Desktop ->
            [ centerX ]


heading : Element Msg
heading =
    el
        [ Region.heading 1
        , alignLeft
        , Font.size headingFontSize
        , spacing headingSpacing
        , Font.color headingColor
        ]
        (text "Notes")


sidebar : Bool -> Bool -> List Note -> Element Msg
sidebar showSidebar isSyncing notes =
    if not showSidebar then
        sidebarToggler showSidebar

    else
        row
            [ width (px 200)
            , height fill
            ]
            [ column
                [ spacing 16
                , alignTop
                , paddingXY 24 24
                , clipX
                , width fill
                ]
                [ newNoteButton
                , syncDataButton isSyncing
                , notePreviews notes
                ]
            , sidebarToggler showSidebar
            ]


notePreviews : List Note -> Element Msg
notePreviews notes =
    column
        [ spacing 12
        , scrollbarY
        , height (px 800)
        , width fill
        ]
        (List.map notePreview notes)


newNoteButton : Element Msg
newNoteButton =
    button (Just AddNote) "+ Add note" green


syncDataButton : Bool -> Element Msg
syncDataButton isSyncing =
    if isSyncing then
        button Nothing "syncing..." teal

    else
        button (Just SyncData) "Sync" teal


sidebarToggler : Bool -> Element Msg
sidebarToggler showSidebar =
    let
        label =
            if showSidebar then
                "<"

            else
                ">"
    in
    el
        [ height fill
        , Border.color purple
        , Border.widthEach { bottom = 0, top = 0, left = 0, right = 1 }
        , padding 12
        , inFront <|
            Input.button
                [ Region.description "toggle sidebar"
                , Border.width 1
                , Border.rounded 500
                , paddingXY 12 10
                , moveRight 6
                , moveDown 12
                , Font.color purple
                , Border.color purple
                , Background.color <| rgba 1.0 1.0 1.0 1.0
                ]
                { label = text label
                , onPress = Just ToggleSidebar
                }
        ]
        Element.none


button : Maybe Msg -> String -> Element.Color -> Element Msg
button onPress lbl color =
    Input.button
        [ Background.color color
        , paddingXY 6 12
        , Font.center
        , Font.size buttonFontSize
        , Border.rounded 6
        , width fill
        , Region.description "Close note editor"
        ]
        { onPress = onPress
        , label = Element.text lbl
        }


noteEditor : Note -> Element Msg
noteEditor { id, contents, title } =
    column
        [ spacing 6
        , width fill
        , height fill
        , padding 12
        ]
        [ row
            [ width fill
            ]
            [ Input.text
                [ Font.size 16, Font.underline ]
                { label = Input.labelHidden "Note title"
                , onChange = EditNoteTitle id
                , placeholder = Nothing
                , text = title
                }
            , Input.button
                [ alignRight
                , padding 6
                , Border.width 1
                , Border.rounded 200
                , Border.color (rgb255 200 25 25)
                ]
                { label = text "X"
                , onPress = Just CloseNote
                }
            ]
        , Input.multiline
            [ width fill
            , height fill
            ]
            { label = Input.labelHidden "Note contents"
            , onChange = EditNoteContents id
            , placeholder = Just (Input.placeholder [] (text "The horrors of a blank page..."))
            , spellcheck = True
            , text = contents
            }
        ]


notePreview : Note -> Element Msg
notePreview { id, contents, title } =
    let
        label =
            column
                [ spacing 6
                ]
                [ el
                    [ Font.size 16 ]
                    (text title)
                , el
                    [ clip
                    , height (px 16)
                    ]
                    (text <| trimContents contents)
                ]
    in
    Input.button
        []
        { label = label
        , onPress = Just <| SelectNote id
        }


trimContents : String -> String
trimContents s =
    if String.length s < 30 then
        s

    else
        String.left 26 s ++ "..."



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize (\w h -> WindowResized { width = w, height = h })
        , Time.every (1 * 1000) SaveData
        ]
