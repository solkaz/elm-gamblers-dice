module Roll exposing (rollGamblerDie)

import Array exposing (Array)


type alias GamblerDieState =
    Array Int


rollGamblerDie : GamblerDieState -> Int -> ( GamblerDieState, Int )
rollGamblerDie state steps =
    let
        result =
            findGamblerResult state steps 0
    in
        ( (updateGamblerDieState state (result - 1)), result )


findGamblerResult : GamblerDieState -> Int -> Int -> Int
findGamblerResult state steps target =
    if steps <= 0 then
        target
    else
        let
            targetValue =
                extractFromGamblerDieState state target
        in
            findGamblerResult state (steps - targetValue) (target + 1)


extractFromGamblerDieState : GamblerDieState -> Int -> Int
extractFromGamblerDieState state key =
    Maybe.withDefault 0 (Array.get key state)


updateGamblerDieState : GamblerDieState -> Int -> GamblerDieState
updateGamblerDieState state target =
    Array.indexedMap
        (\index value ->
            if index == target then
                value - 1
            else
                value + 1
        )
        state
