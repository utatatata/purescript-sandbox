module Test.State where

import Prelude
import State as S
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

testState :: Spec Unit
testState =
  describe "State" do
    it "test" do
      S.evalState
        ( S.get
            `S.bind`
              \n1 ->
                S.modify (\n -> n + n1)
                  `S.bind`
                    \n2 -> S.pure $ n2 - 5
        )
        3
        `shouldEqual`
          1
