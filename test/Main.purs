module Test.Main where

import Prelude hiding (div)

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Hedwig as H
import Hedwig.Element (styleEl)
import Html.Styled (renderHtmlToString, PseudoSelector(..), Style(..), button, div, span, toUnstyled)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
foreign import neverUse :: String

main :: Effect Unit
main = run [consoleReporter] do
  describe "main" do
    it "should convert styled element with no styles to a Hedwig Html element" do

      renderHtmlToString (toUnstyled (div [] [] [span [] [] []]) )
        `shouldEqual`
        renderHtmlToString (H.div [] [H.span [] []])

    it "should create a Hedwig Html element from a styled element with a style" do

      let
        input =
          div
            []
            []
            [ button [Style Nothing "padding" "20px"] [] []
            ]

        expectedResult =
          H.div
            []
            [ styleEl [] [H.text styleContent]
            , H.button [H.class' "_1424543658"] []
            ]

        styleContent = """._1424543658 {
    padding:20px;
}"""

      renderHtmlToString (toUnstyled input) `shouldEqual` renderHtmlToString expectedResult

    it "should create a Hedwig Html element from a styled element with a multiple styles" do

      let
        input =
          div
            [Style Nothing "margin" "2rem"]
            []
            [ button [Style Nothing "padding" "20px", Style Nothing "color" "blue"] [] []
            ]

        expectedResult =
          H.div
            [H.class' "_1127099563"]
            [ styleEl [] [H.text styleContent]
            , H.button [H.class' "_652870858"] []
            ]

        styleContent = """._1127099563 {
    margin:2rem;
}
._652870858 {
    padding:20px;
    color:blue;
}"""

      renderHtmlToString (toUnstyled input) `shouldEqual` renderHtmlToString expectedResult

    it "should not create duplicate classes for duplicate styles" do

      let
        input =
          div
            [Style Nothing "margin" "2rem"]
            []
            [ button [Style Nothing "margin" "2rem"] [] []
            ]

        expectedResult =
          H.div
            [H.class' "_1127099563"]
            [ styleEl [] [H.text styleContent]
            , H.button [H.class' "_1127099563"] []
            ]

        styleContent = """._1127099563 {
    margin:2rem;
}"""

      renderHtmlToString (toUnstyled input) `shouldEqual` renderHtmlToString expectedResult

    it "should create pseudo selector classes for pseudo selector styles" do


      let
        input =
          div
            []
            []
            [ button [Style (Just Hover) "padding" "20px"] [] []
            ]

        expectedResult =
          H.div
            []
            [ styleEl [] [H.text styleContent]
            , H.button [H.class' "_1375398950"] []
            ]

        styleContent = """._1375398950:hover {
    padding:20px;
}"""

      renderHtmlToString (toUnstyled input) `shouldEqual` renderHtmlToString expectedResult
