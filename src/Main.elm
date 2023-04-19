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
import Json.Decode as Json



-- MAIN


main : Program Json.Value Model Msg
main =
    Browser.document
        { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { dimensions : Dimensions
    , notes : List Note
    }


type alias Dimensions =
    { width : Int
    , height : Int
    }


type alias Note =
    { contents : String
    , id : Int
    }


init : Json.Value -> ( Model, Cmd Msg )
init flags =
    let
        dimensions =
            parseFlags flags
    in
    ( { dimensions = dimensions
      , notes = []
      }
    , Cmd.none
    )


parseFlags : Json.Value -> Dimensions
parseFlags flags =
    Json.decodeValue dimensionsDecoder flags
        |> Result.withDefault defaultDimensions


dimensionsDecoder : Json.Decoder Dimensions
dimensionsDecoder =
    Json.map2 Dimensions
        (Json.field "width" Json.int)
        (Json.field "height" Json.int)


defaultDimensions =
    { width = 500
    , height = 500
    }



-- UPDATE


type Msg
    = WindowResized Dimensions
    | AddNote
    | EditNote Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResized newDim ->
            ( { model | dimensions = newDim }, Cmd.none )

        AddNote ->
            ( { model | notes = newNote model :: model.notes }, Cmd.none )

        EditNote id newContents ->
            ( { model | notes = editNote id newContents model.notes }, Cmd.none )


newNote : Model -> Note
newNote model =
    let
        maxId =
            model.notes |> List.map .id |> List.maximum |> Maybe.withDefault 0
    in
    { id = maxId + 1
    , contents = ""
    }


editNote : Int -> String -> List Note -> List Note
editNote id newContents notes =
    let
        maybeNote : Maybe Note
        maybeNote =
            notes
                |> List.filter (\n -> n.id == id)
                |> List.head

        notesWithoutCurrent : List Note
        notesWithoutCurrent =
            notes
                |> List.filter (\n -> n.id /= id)
    in
    case maybeNote of
        Nothing ->
            notes

        Just noteToEdit ->
            { noteToEdit | contents = newContents } :: notesWithoutCurrent



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
    18


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
    in
    Element.layout
        [ Font.size baseFontSize
        , paddingEach basePadding
        ]
    <|
        Element.column
            ([ width <| columnWidth screenSize model.dimensions.width
             , spacing columnSpacing
             ]
                ++ responsiveColumnAttributes screenSize
            )
            [ heading
            , buttons screenSize model.dimensions model.notes
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


buttons : ScreenSize -> Dimensions -> List Note -> Element Msg
buttons screenSize dim notes =
    let
        w =
            columnWidth screenSize dim.width

        group =
            case screenSize of
                Mobile ->
                    column

                Desktop ->
                    row
    in
    group
        [ spacing buttonSpacing
        , width w
        ]
        ([ button (Just AddNote) "Add note" green
         ]
            ++ (notes
                    |> List.sortBy .id
                    |> List.map note
               )
        )


button : Maybe Msg -> String -> Element.Color -> Element Msg
button onPress lbl color =
    Input.button
        [ Background.color color
        , paddingXY 0 12
        , Font.center
        , Font.size buttonFontSize
        , Border.rounded 6
        , width fill
        ]
        { onPress = onPress
        , label = Element.text lbl
        }


note : Note -> Element Msg
note { id, contents } =
    column
        [ spacing 6
        ]
        [ text <| "Note #" ++ String.fromInt id
        , Input.multiline
            []
            { label = Input.labelHidden "Note contents"
            , onChange = EditNote id
            , placeholder = Just (Input.placeholder [] (text "The horrors of a blank page..."))
            , spellcheck = True
            , text = contents
            }
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    onResize (\w h -> WindowResized { width = w, height = h })
