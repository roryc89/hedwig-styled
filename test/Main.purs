module Test.Main where

import Prelude hiding (div)

import Data.Array (replicate, (..))
import Data.DateTime.Instant (unInstant)
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Hedwig as H
import Html.PseudoClass (PseudoClass(..))
import Html.Styled (Style(..), button, div, renderHtmlToString, span, styleEl, toUnstyled)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
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

    it "should not take too long" do

        (Milliseconds start) <- unInstant <$> liftEffect now

        let
          input =
            div
              []
              []
              ( replicate 200 (button [Style (Just Hover) "padding" "20px"] [] [])
              <> replicate 200 (button [Style Nothing "margin" "3rem"] [] [])
              <> (mapFlipped (0..200) (\i -> (button [Style Nothing "margin" (show i <> "px")] [] [])))
              )


        let _ = renderHtmlToString (toUnstyled input)

        (Milliseconds end) <-  unInstant <$> liftEffect now

        let duration = end - start

        log $ "duration: " <> show duration

        when (duration > 50.0) $
          fail ("test took too long. time: " <> show duration)
