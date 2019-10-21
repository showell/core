module Test.Dict exposing (tests)

import Basics exposing (..)
import Data.Dict as Dict exposing (..)
import List
import Maybe exposing (..)
import Test exposing (..)
import Expect


animals : Dict.Dict String String
animals =
    Dict.fromList [ ( "Tom", "cat" ), ( "Jerry", "mouse" ) ]


tests : Test
tests =
    let
        buildTests =
            describe "build Tests"
                [ test "empty" <| \() -> Expect.equal (Dict.fromList []) (Dict.empty)
                , test "singleton" <| \() -> Expect.equal (Dict.fromList [ ( "k", "v" ) ]) (Dict.singleton "k" "v")
                , test "insert" <| \() -> Expect.equal (Dict.fromList [ ( "k", "v" ) ]) (Dict.insert "k" "v" Dict.empty)
                , test "deep insert tests" <| \() -> deepInsertAnalysis
                , test "insert replace" <| \() -> Expect.equal (Dict.fromList [ ( "k", "vv" ) ]) (Dict.insert "k" "vv" (Dict.singleton "k" "v"))
                , test "update" <| \() -> Expect.equal (Dict.fromList [ ( "k", "vv" ) ]) (Dict.update "k" (\v -> Just "vv") (Dict.singleton "k" "v"))
                , test "update Nothing" <| \() -> Expect.equal Dict.empty (Dict.update "k" (\v -> Nothing) (Dict.singleton "k" "v"))
                , test "remove" <| \() -> Expect.equal Dict.empty (Dict.remove "k" (Dict.singleton "k" "v"))
                , test "remove not found" <| \() -> Expect.equal (Dict.singleton "k" "v") (Dict.remove "kk" (Dict.singleton "k" "v"))
                ]

        queryTests =
            describe "query Tests"
                [ test "member 1" <| \() -> Expect.equal True (Dict.member "Tom" animals)
                , test "member 2" <| \() -> Expect.equal False (Dict.member "Spike" animals)
                , test "get 1" <| \() -> Expect.equal (Just "cat") (Dict.get "Tom" animals)
                , test "get 2" <| \() -> Expect.equal Nothing (Dict.get "Spike" animals)
                , test "size of empty dictionary" <| \() -> Expect.equal 0 (Dict.size Dict.empty)
                , test "size of example dictionary" <| \() -> Expect.equal 2 (Dict.size animals)
                ]

        combineTests =
            describe "combine Tests"
                [ test "union" <| \() -> Expect.equal animals (Dict.union (Dict.singleton "Jerry" "mouse") (Dict.singleton "Tom" "cat"))
                , test "union collison" <| \() -> Expect.equal (Dict.singleton "Tom" "cat") (Dict.union (Dict.singleton "Tom" "cat") (Dict.singleton "Tom" "mouse"))
                , test "intersect" <| \() -> Expect.equal (Dict.singleton "Tom" "cat") (Dict.intersect animals (Dict.singleton "Tom" "cat"))
                , test "diff" <| \() -> Expect.equal (Dict.singleton "Jerry" "mouse") (Dict.diff animals (Dict.singleton "Tom" "cat"))
                ]

        transformTests =
            describe "transform Tests"
                [ test "filter" <| \() -> Expect.equal (Dict.singleton "Tom" "cat") (Dict.filter (\k v -> k == "Tom") animals)
                , test "partition" <| \() -> Expect.equal ( Dict.singleton "Tom" "cat", Dict.singleton "Jerry" "mouse" ) (Dict.partition (\k v -> k == "Tom") animals)
                ]

        mergeTests =
            let
                insertBoth key leftVal rightVal dict =
                    Dict.insert key (leftVal ++ rightVal) dict

                s1 =
                    Dict.empty |> Dict.insert "u1" [ 1 ]

                s2 =
                    Dict.empty |> Dict.insert "u2" [ 2 ]

                s23 =
                    Dict.empty |> Dict.insert "u2" [ 3 ]

                b1 =
                    List.map (\i -> ( i, [ i ] )) (List.range 1 10) |> Dict.fromList

                b2 =
                    List.map (\i -> ( i, [ i ] )) (List.range 5 15) |> Dict.fromList

                bExpected =
                    [ ( 1, [ 1 ] ), ( 2, [ 2 ] ), ( 3, [ 3 ] ), ( 4, [ 4 ] ), ( 5, [ 5, 5 ] ), ( 6, [ 6, 6 ] ), ( 7, [ 7, 7 ] ), ( 8, [ 8, 8 ] ), ( 9, [ 9, 9 ] ), ( 10, [ 10, 10 ] ), ( 11, [ 11 ] ), ( 12, [ 12 ] ), ( 13, [ 13 ] ), ( 14, [ 14 ] ), ( 15, [ 15 ] ) ]
            in
                describe "merge Tests"
                    [ test "merge empties" <|
                        \() ->
                            Expect.equal (Dict.empty)
                                (Dict.merge Dict.insert insertBoth Dict.insert Dict.empty Dict.empty Dict.empty)
                    , test "merge singletons in order" <|
                        \() ->
                            Expect.equal [ ( "u1", [ 1 ] ), ( "u2", [ 2 ] ) ]
                                ((Dict.merge Dict.insert insertBoth Dict.insert s1 s2 Dict.empty) |> Dict.toList)
                    , test "merge singletons out of order" <|
                        \() ->
                            Expect.equal [ ( "u1", [ 1 ] ), ( "u2", [ 2 ] ) ]
                                ((Dict.merge Dict.insert insertBoth Dict.insert s2 s1 Dict.empty) |> Dict.toList)
                    , test "merge with duplicate key" <|
                        \() ->
                            Expect.equal [ ( "u2", [ 2, 3 ] ) ]
                                ((Dict.merge Dict.insert insertBoth Dict.insert s2 s23 Dict.empty) |> Dict.toList)
                    , test "partially overlapping" <|
                        \() ->
                            Expect.equal bExpected
                                ((Dict.merge Dict.insert insertBoth Dict.insert b1 b2 Dict.empty) |> Dict.toList)
                    ]
    in
        describe "Dict Tests"
            [ buildTests
            , queryTests
            , combineTests
            , transformTests
            , mergeTests
            ]


type alias DictGeneration =
    { aliases : Dict String String
    , humanOutput : String
    , interestingLists : List (List String)
    , n : Int
    }


type alias SingleGenerationResult =
    { humanOutput : String
    , aliases : Dict String String
    , interestingLists : List (List String)
    }


dictString : Dict String String -> Dict k v -> String
dictString aliases dct =
    let
        toRepr =
            Dict.internalRepresentationOfTheUnderlyingRedBlackTreeForTestingPurposes

        remap s =
            get s aliases |> Maybe.withDefault s

        reprToString repr =
            let
                helper color left right =
                    let
                        l =
                            left |> reprToString |> remap

                        r =
                            right |> reprToString |> remap
                    in
                    "(" ++ l ++ " " ++ color ++ " " ++ r ++ ")"
            in
            case repr of
                InternalDictRepr (Just ( color, _, ( left, right ) )) ->
                    case ( left, right ) of
                        ( InternalDictRepr Nothing, InternalDictRepr Nothing ) ->
                            color

                        ( InternalDictRepr Nothing, InternalDictRepr (Just ( "R", _, _ )) ) ->
                            -- Red is never a right child
                            "ERROR"

                        ( InternalDictRepr (Just ( "R", _, ( _, _ ) )), InternalDictRepr Nothing ) ->
                            if color == "R" then
                                "ERROR"

                            else
                                helper color left right

                        _ ->
                            helper color left right

                InternalDictRepr Nothing ->
                    "_"
    in
    dct |> toRepr |> reprToString


uniqueReverseMap : List ( input, comparable ) -> Dict comparable input
uniqueReverseMap tupList =
    -- Take a list of tuples and do a reverse mapping of
    -- output back to input, taking only the first example
    -- for each group of inputs that all have the same output.
    -- We use this to compress the list of things to test
    -- after each successive generation of bigger and bigger dicts.
    -- It helps us avoid combinatorial explosion.
    let
        f d candidates =
            case candidates of
                [] ->
                    d

                ( input, output ) :: rest ->
                    case get output d of
                        Just _ ->
                            -- we already have an example
                            -- for this particular representation
                            d

                        Nothing ->
                            f (Dict.insert output input d) rest
    in
    f empty tupList


permuteList : List String -> List (List String)
permuteList lst =
    let
        baseList =
            lst
                |> List.map (\s -> s ++ "1")

        successors =
            lst
                |> List.map (\s -> s ++ "2")

        newElements =
            "a" :: successors

        newLists =
            newElements
                |> List.map (\elem -> baseList ++ [ elem ])
    in
    newLists


letterFromIdx : Int -> String
letterFromIdx idx =
    case idx of
        0 ->
            "a"

        1 ->
            "b"

        2 ->
            "c"

        _ ->
            "?"

analyzeDict : Int -> DictGeneration
analyzeDict maxN=
    let
        toReprTuple aliases lst =
            let
                dictRepr =
                    lst
                        |> List.map (\k -> ( k, "" ))
                        |> fromList
                        |> dictString aliases
            in
            ( lst, dictRepr )

        makeGeneration : DictGeneration -> SingleGenerationResult
        makeGeneration params =
            let
                aliases =
                    params.aliases

                n =
                    params.n

                prevLists =
                    params.interestingLists

                newLists =
                    prevLists
                        |> List.map permuteList
                        |> List.concat

                prefix =
                    "T" ++ String.fromInt n

                reprTuples =
                    newLists
                        |> List.map (toReprTuple aliases)

                reverseMap =
                    reprTuples
                        |> uniqueReverseMap

                interestingLists =
                    values reverseMap

                uniqueStrings =
                    reverseMap
                        |> keys
                        |> List.sort

                makeId idx =
                    prefix ++ letterFromIdx idx

                makeTup idx s =
                    ( s, makeId idx )

                newAliasTuples =
                    uniqueStrings
                        |> List.indexedMap makeTup

                humanOutput =
                    "\n\n==============\n"
                        ++ String.fromInt n
                        ++ " nodes:\n"
                        ++ (newAliasTuples
                                |> List.map (\( val, name ) -> "\n" ++ name ++ " = " ++ val)
                                |> List.foldr (++) ""
                           )
            in
            { humanOutput = humanOutput
            , aliases = fromList newAliasTuples
            , interestingLists = interestingLists
            }

        accum : DictGeneration -> DictGeneration
        accum params =
            if params.n > maxN then
                params

            else
                let
                    newTrees =
                        makeGeneration params

                    updatedHumanOutput =
                        params.humanOutput ++ newTrees.humanOutput

                    updatedAliases =
                        union
                            params.aliases
                            newTrees.aliases
                in
                accum
                    { humanOutput = updatedHumanOutput
                    , aliases = updatedAliases
                    , interestingLists = newTrees.interestingLists
                    , n = params.n + 1
                    }

        result =
            accum
                { interestingLists = [ [ "b" ] ]
                , aliases = Dict.empty
                , n = 2
                , humanOutput = ""
                }
    in
    result

deepInsertAnalysis =
    let
        actualTreeShapes =
            (analyzeDict 64).humanOutput
                |> String.trim
    in
    Expect.equal treeShapes (analyzeDict 64).humanOutput


treeShapes =
    String.trim """
==============
2 nodes:

T2a = (R B _)

==============
3 nodes:

T3a = (B B B)

==============
4 nodes:

T4a = (B B T2a)
T4b = (T2a B B)

==============
5 nodes:

T5a = ((B R B) B B)
T5b = (T2a B T2a)

==============
6 nodes:

T6a = ((B R T2a) B B)
T6b = ((T2a R B) B B)

==============
7 nodes:

T7a = ((T2a R T2a) B B)
T7b = (T3a B T3a)

==============
8 nodes:

T8a = (T3a B T4b)
T8b = (T4b B T3a)

==============
9 nodes:

T9a = (T3a B T5a)
T9b = (T4b B T4b)

==============
10 nodes:

T10a = (T3a B T6b)
T10b = (T4b B T5a)

==============
11 nodes:

T11a = ((T3a R T3a) B T3a)
T11b = (T4b B T6b)

==============
12 nodes:

T12a = ((T3a R T4b) B T3a)
T12b = ((T4b R T3a) B T3a)

==============
13 nodes:

T13a = ((T3a R T5a) B T3a)
T13b = ((T4b R T4b) B T3a)

==============
14 nodes:

T14a = ((T3a R T6b) B T3a)
T14b = ((T4b R T5a) B T3a)

==============
15 nodes:

T15a = ((T4b R T6b) B T3a)
T15b = (T7b B T7b)

==============
16 nodes:

T16a = ((T5a R T6b) B T3a)
T16b = (T8b B T7b)

==============
17 nodes:

T17a = ((T5a B T3a) B T7b)
T17b = ((T6b R T6b) B T3a)

==============
18 nodes:

T18a = ((T5a B T4b) B T7b)
T18b = ((T6b B T3a) B T7b)

==============
19 nodes:

T19a = ((T5a B T5a) B T7b)
T19b = ((T6b B T4b) B T7b)

==============
20 nodes:

T20a = ((T5a B T6b) B T7b)
T20b = ((T6b B T5a) B T7b)

==============
21 nodes:

T21a = (((T5a R T3a) B T3a) B T7b)
T21b = ((T6b B T6b) B T7b)

==============
22 nodes:

T22a = (((T5a R T4b) B T3a) B T7b)
T22b = (((T6b R T3a) B T3a) B T7b)

==============
23 nodes:

T23a = (((T5a R T5a) B T3a) B T7b)
T23b = (((T6b R T4b) B T3a) B T7b)

==============
24 nodes:

T24a = (((T6b R T5a) B T3a) B T7b)
T24b = (T16a B T7b)

==============
25 nodes:

T25a = ((T7b R (T5a B T3a)) B T7b)
T25b = (T17b B T7b)

==============
26 nodes:

T26a = ((T7b R (T6b B T3a)) B T7b)
T26b = ((T8b R (T5a B T3a)) B T7b)

==============
27 nodes:

T27a = ((T7b R T11a) B T7b)
T27b = ((T8b R (T6b B T3a)) B T7b)

==============
28 nodes:

T28a = ((T7b R T12b) B T7b)
T28b = ((T8b R T11a) B T7b)

==============
29 nodes:

T29a = ((T7b R ((T5a R T3a) B T3a)) B T7b)
T29b = ((T8b R T12b) B T7b)

==============
30 nodes:

T30a = ((T7b R ((T6b R T3a) B T3a)) B T7b)
T30b = ((T8b R ((T5a R T3a) B T3a)) B T7b)

==============
31 nodes:

T31a = ((T8b R ((T6b R T3a) B T3a)) B T7b)
T31b = (T15b B T15b)

==============
32 nodes:

T32a = (((T5a B T3a) R ((T6b R T3a) B T3a)) B T7b)
T32b = (T16b B T15b)

==============
33 nodes:

T33a = (((T6b B T3a) R ((T6b R T3a) B T3a)) B T7b)
T33b = (T17a B T15b)

==============
34 nodes:

T34a = ((T11a R ((T6b R T3a) B T3a)) B T7b)
T34b = (T18b B T15b)

==============
35 nodes:

T35a = ((T11a B T7b) B T15b)
T35b = ((T12b R ((T6b R T3a) B T3a)) B T7b)

==============
36 nodes:

T36a = ((T11a B T8b) B T15b)
T36b = ((T12b B T7b) B T15b)

==============
37 nodes:

T37a = ((T11a B (T5a B T3a)) B T15b)
T37b = ((T12b B T8b) B T15b)

==============
38 nodes:

T38a = ((T11a B (T6b B T3a)) B T15b)
T38b = ((T12b B (T5a B T3a)) B T15b)

==============
39 nodes:

T39a = ((T11a B T11a) B T15b)
T39b = ((T12b B (T6b B T3a)) B T15b)

==============
40 nodes:

T40a = ((T11a B T12b) B T15b)
T40b = ((T12b B T11a) B T15b)

==============
41 nodes:

T41a = ((T11a B ((T5a R T3a) B T3a)) B T15b)
T41b = ((T12b B T12b) B T15b)

==============
42 nodes:

T42a = ((T11a B ((T6b R T3a) B T3a)) B T15b)
T42b = ((T12b B ((T5a R T3a) B T3a)) B T15b)

==============
43 nodes:

T43a = (((T11a R T7b) B T7b) B T15b)
T43b = ((T12b B ((T6b R T3a) B T3a)) B T15b)

==============
44 nodes:

T44a = (((T11a R T8b) B T7b) B T15b)
T44b = (((T12b R T7b) B T7b) B T15b)

==============
45 nodes:

T45a = (((T11a R (T5a B T3a)) B T7b) B T15b)
T45b = (((T12b R T8b) B T7b) B T15b)

==============
46 nodes:

T46a = (((T11a R (T6b B T3a)) B T7b) B T15b)
T46b = (((T12b R (T5a B T3a)) B T7b) B T15b)

==============
47 nodes:

T47a = (((T11a R T11a) B T7b) B T15b)
T47b = (((T12b R (T6b B T3a)) B T7b) B T15b)

==============
48 nodes:

T48a = (((T11a R T12b) B T7b) B T15b)
T48b = (((T12b R T11a) B T7b) B T15b)

==============
49 nodes:

T49a = (((T11a R ((T5a R T3a) B T3a)) B T7b) B T15b)
T49b = (((T12b R T12b) B T7b) B T15b)

==============
50 nodes:

T50a = (((T12b R ((T5a R T3a) B T3a)) B T7b) B T15b)
T50b = (T34a B T15b)

==============
51 nodes:

T51a = (((((T5a R T3a) B T3a) R ((T5a R T3a) B T3a)) B T7b) B T15b)
T51b = (T35b B T15b)

==============
52 nodes:

T52a = (((((T5a R T3a) B T3a) R ((T6b R T3a) B T3a)) B T7b) B T15b)
T52b = (((((T6b R T3a) B T3a) R ((T5a R T3a) B T3a)) B T7b) B T15b)

==============
53 nodes:

T53a = (((((T6b R T3a) B T3a) R ((T6b R T3a) B T3a)) B T7b) B T15b)
T53b = ((T21a R T15b) B T15b)

==============
54 nodes:

T54a = ((T15b R T22b) B T15b)
T54b = ((T22b R T15b) B T15b)

==============
55 nodes:

T55a = ((T15b R ((T7b R T7b) B T7b)) B T15b)
T55b = ((T16b R T22b) B T15b)

==============
56 nodes:

T56a = ((T15b R ((T8b R T7b) B T7b)) B T15b)
T56b = ((T16b R ((T7b R T7b) B T7b)) B T15b)

==============
57 nodes:

T57a = ((T15b R (((T5a B T3a) R T7b) B T7b)) B T15b)
T57b = ((T16b R ((T8b R T7b) B T7b)) B T15b)

==============
58 nodes:

T58a = ((T15b R (((T6b B T3a) R T7b) B T7b)) B T15b)
T58b = ((T16b R (((T5a B T3a) R T7b) B T7b)) B T15b)

==============
59 nodes:

T59a = ((T15b R ((T11a R T7b) B T7b)) B T15b)
T59b = ((T16b R (((T6b B T3a) R T7b) B T7b)) B T15b)

==============
60 nodes:

T60a = ((T15b R ((T12b R T7b) B T7b)) B T15b)
T60b = ((T16b R ((T11a R T7b) B T7b)) B T15b)

==============
61 nodes:

T61a = ((T15b R ((((T5a R T3a) B T3a) R T7b) B T7b)) B T15b)
T61b = ((T16b R ((T12b R T7b) B T7b)) B T15b)

==============
62 nodes:

T62a = ((T15b R ((((T6b R T3a) B T3a) R T7b) B T7b)) B T15b)
T62b = ((T16b R ((((T5a R T3a) B T3a) R T7b) B T7b)) B T15b)

==============
63 nodes:

T63a = ((T16b R ((((T6b R T3a) B T3a) R T7b) B T7b)) B T15b)
T63b = (T31b B T31b)

==============
64 nodes:

T64a = ((T17a R ((((T6b R T3a) B T3a) R T7b) B T7b)) B T15b)
T64b = (T32b B T31b)
"""
