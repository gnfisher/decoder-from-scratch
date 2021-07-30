module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes


type Json
    = JsonString String
    | JsonInt Int
    | JsonBoolean Bool
    | JsonNull
    | JsonObject (Dict String Json)


extractAt : String -> Decoder a -> Decoder a
extractAt key (Decoder fn) =
    Decoder
        (\json ->
            case json of
                JsonObject dict ->
                    case Dict.get key dict of
                        Just innerJson ->
                            fn innerJson

                        Nothing ->
                            Err "Key not found"

                _ ->
                    Err "Not an object"
        )


extractInt : Decoder Int
extractInt =
    Decoder
        (\json ->
            case json of
                JsonInt int ->
                    Ok int

                _ ->
                    Err "Not an int!"
        )


extractString : Decoder String
extractString =
    Decoder
        (\json ->
            case json of
                JsonString str ->
                    Ok str

                _ ->
                    Err "Not an string!"
        )


type Decoder a
    = Decoder (Json -> Result String a)


map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 fn (Decoder aFn) (Decoder bFn) =
    Decoder
        (\json -> Result.map2 fn (aFn json) (bFn json))


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap valueDecoder fnDecoder =
    map2 (\fn val -> fn val) fnDecoder valueDecoder


map3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
map3 fn dA dB dC =
    success fn
        |> andMap dA
        |> andMap dB
        |> andMap dC


decode : Decoder a -> Json -> Result String a
decode (Decoder fn) json =
    fn json


success : a -> Decoder a
success a =
    Decoder (\_ -> Ok a)


{-|
  Next time!

  - oneOf
  - andThen
  - or
  - fold
-}


type alias User =
    { name : String, age : Int }


userJson : Json
userJson =
    JsonObject (Dict.fromList [ ( "name", JsonString "Ralph" ), ( "age", JsonInt 18 ) ])


userDecoder : Decoder User
userDecoder =
    map2 User (extractAt "name" extractString) (extractAt "age" extractInt)


buildUser : Result String User
buildUser =
    decode userDecoder userJson


main : Html a
main =
    Html.text <| Debug.toString buildUser
