module NormalDie exposing (..)

import Random


roll : Int -> (Int -> msg) -> Cmd msg
roll dieFaces msg =
    Random.generate msg (Random.int 1 dieFaces)
