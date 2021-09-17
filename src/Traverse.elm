module Traverse exposing (main)

import Html exposing (Html, text)


main : Html a
main =
    text <| Debug.toString example2



-- List (Maybe a)
-- Maybe (List a)


listMaybe : List (Maybe Int)
listMaybe =
    [ Just 1, Just 3, Just 5 ]



-- Two options to deal with this
-- 1. Drop nothings, make a list of Ints
-- 2. Aggregate the individual Maybes into one Maybe (Fold)
-- 2. (in gregs brain) Wrap list of Ints in maybe


step : Maybe a -> Maybe (List a) -> Maybe (List a)
step m mAcc =
    Maybe.map2 (\i acc -> i :: acc) m mAcc



-- sequence
-- intersection of map2 and fold


aggregate : List (Maybe a) -> Maybe (List a)
aggregate mList =
    List.foldl step (Just []) mList


sequence : List (Maybe a) -> Maybe (List a)
sequence =
    List.foldr (Maybe.map2 (::)) (Just [])


example : Maybe (List Int)
example =
    sequence listMaybe


listString : List String
listString =
    [ "1", "3", "5" ]



-- Parse strings into list of maybes
-- Aggregate the whole


example2 : Maybe (List Int)
example2 =
    sequenceMap String.toInt listString



-- Traverse


sequenceMap : (a -> Maybe b) -> List a -> Maybe (List b)
sequenceMap fn list =
    sequence <| List.map fn list


step2 : (a -> Maybe b) -> a -> Maybe (List b) -> Maybe (List b)
step2 fn val mAcc =
    Maybe.map2 (\i acc -> i :: acc) (fn val) mAcc



-- Traverse - efficient


sequenceMap2 : (a -> Maybe b) -> List a -> Maybe (List b)
sequenceMap2 fn list =
    List.foldr (step2 fn) (Just []) list


sequence2 : List (Maybe a) -> Maybe (List a)
sequence2 list =
    sequenceMap2 identity list
