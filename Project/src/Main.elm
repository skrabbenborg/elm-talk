module Main exposing (..)

import Browser
import Debug exposing (toString)
import Html exposing (Html)
import Html.Attributes as Attribute
import Http
import Json.Decode as Decode exposing (Decoder)


------ DATA OBJECTS ------


type alias Data =
    { people : ResponseStatus Http.Error (List Person)
    }


type alias Person =
    { name : String
    , age : Int
    , birthday: String
    }


type Model
    = Initializing
    | Initialized Data


type Msg
    = GotData (Result Http.Error (List Person))


type ResponseStatus error result
    = Failure error
    | Success result



------- INIT -------


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( Initializing
    , requestData
    )



------ UPDATE ------


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case ( message, model ) of
        ( GotData result, Initializing ) ->
            let
                retrievedPeople =
                    case result of
                        Ok results ->
                            Success results

                        Err error ->
                            Failure error
            in
            ( Initialized { people = retrievedPeople }, Cmd.none )

        _ ->
            ( model, Cmd.none )



------ VIEW ------


view : Model -> Html Msg
view model =
    case model of
        Initializing ->
            viewDataLoading

        Initialized data ->
            viewDataLoaded data


viewDataLoading : Html msg
viewDataLoading =
    Html.text "Fetching data. Just a moment!"


viewDataLoaded : Data -> Html msg
viewDataLoaded data =
    case data.people of
        Success entries ->
            viewDataTable entries

        Failure e ->
            Html.text <| "Failed to fetch data! " ++ toString e


viewDataTable : List Person -> Html msg
viewDataTable entries =
    Html.table [ Attribute.class "overview-table" ]
        [ Html.thead []
            [ Html.tr []
                [ Html.td [] [ Html.text "name" ]
                , Html.td [] [ Html.text "age" ]
                , Html.td [] [ Html.text "birthday" ]
                ]
            ]
        , Html.tbody [] <| List.map viewDataRow entries
        ]


viewDataRow : Person -> Html msg
viewDataRow entry =
    let
        name =
            toString entry.name

        age =
            toString entry.age

        birthday =
            toString entry.birthday
    in
    Html.tr []
        [ Html.td [] [ Html.text name ]
        , Html.td [] [ Html.text age ]
        , Html.td [] [ Html.text birthday ]
        ]



------ HTTP AND JSON DECODING ------


requestData : Cmd Msg
requestData =
    Http.get
        { url = "https://exdbxzxcae.execute-api.eu-central-1.amazonaws.com/Prod/birthdays"
        , expect = Http.expectJson GotData <| Decode.list personDecoder
        }


personDecoder : Decoder Person
personDecoder =
    Decode.map3 personFrom
        (Decode.field "name" Decode.string)
        (Decode.field "age" Decode.int)
        (Decode.field "birthday" Decode.string)


personFrom : String -> Int  -> String -> Person
personFrom name age birthday =
    { name = name
    , age = age
    , birthday = birthday
    }



------ SUBSCRIPTIONS ------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
