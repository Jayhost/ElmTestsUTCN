module MyTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Html exposing (select)
import Html.Attributes
import Json.Encode exposing (Value)
import Main
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig as Config
import ProgramTest
import SimulatedEffect
import Test exposing (..)
import Test.Html.Event as Evt
import Test.Html.Query as Q
import Test.Html.Selector as S
import TestData as Data
import TestUtils as Utils
import Time exposing (Month(..))
import Util.Time exposing (Date(..))
import View.Posts as Posts


checkConfigChangeEffect : List S.Selector -> ( String, Value ) -> (Q.Single Msg -> Expectation) -> (Q.Single Msg -> Expectation) -> Expectation
checkConfigChangeEffect selector event preCond postCond =
    SimulatedEffect.fromLoadedState
        |> ProgramTest.ensureView (Q.find selector >> preCond)
        |> ProgramTest.simulateDomEvent (Q.find selector) event
        |> ProgramTest.expectView (Q.find selector >> postCond)

check = checkConfigChangeEffect
                        [ S.tag "input", S.attribute (Html.Attributes.type_ "checkbox"), S.id "checkbox-show-job-posts" ]
                        (Evt.check True)
                        (Q.has [ S.checked False ])
                        (Q.has [ S.checked True ])