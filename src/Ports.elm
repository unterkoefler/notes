port module Ports exposing (..)

import Json.Encode


port saveData : Json.Encode.Value -> Cmd msg
