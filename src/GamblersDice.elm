module GamblersDice exposing (..)

import Array exposing (Array)
import Random


type alias GamblersDieState =
    Array Int


sum : GamblersDieState -> Int
sum state =
    Array.foldr (+) 0 state


generateSteps : Int -> (Int -> msg) -> Cmd msg
generateSteps stateSum msg =
    Random.generate msg (Random.int 0 stateSum)


roll : GamblersDieState -> Int -> ( GamblersDieState, Int )
roll state steps =
    let
        result =
            findResult state steps 0
    in
        ( (updateState state (result - 1)), result )


findResult : GamblersDieState -> Int -> Int -> Int
findResult state steps target =
    if steps <= 0 then
        target
    else
        let
            targetValue =
                extractFromState state target
        in
            findResult state (steps - targetValue) (target + 1)


extractFromState : GamblersDieState -> Int -> Int
extractFromState state key =
    Maybe.withDefault 0 (Array.get key state)


updateState : GamblersDieState -> Int -> GamblersDieState
updateState state target =
    Array.indexedMap
        (\index value ->
            if index == target then
                value - 1
            else
                value + 1
        )
        state
