module Html.Styled where

import Prelude

import Data.Array (filter, nub, null, snoc)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Hedwig (Html, Trait, element)
import Hedwig as H
import Hedwig.Element (Element)
import Html.Styled.PseudoClass (PseudoClass(..), pseudoClassString)

foreign import quickHash :: String -> Int

data Style
  = Style (Maybe PseudoClass) String String

style :: String -> String -> Style
style = Style Nothing

styles :: Array (Tuple String String) -> Array Style
styles = map \(Tuple key val) -> style key val

pseudo :: PseudoClass -> String -> String -> Style
pseudo = Style <<< Just

pseudos :: PseudoClass -> Array (Tuple String String) -> Array Style
pseudos pCl = map \(Tuple key val) -> pseudo pCl key val

hovers :: Array (Tuple String String) -> Array Style
hovers = pseudos Hover

getPseudoClass :: Style -> Maybe PseudoClass
getPseudoClass (Style ps _ _ ) = ps

derive instance genericStyle :: Generic Style _

derive instance eqStyle :: Eq Style

derive instance ordStyle :: Ord Style

instance showStyle :: Show Style where
  show = genericShow

data StyledHtml msg
  = StyledHtml String (Array Style) (Array (Trait msg)) (Array (StyledHtml msg))
  | StyledText String

derive instance genericStyledHtml :: Generic (StyledHtml msg) _

toUnstyled :: forall msg. StyledHtml msg -> H.Html msg
toUnstyled styledHtml =

  case styledHtml of
   StyledHtml name styles_ traits children ->

      H.element
        name
        (if null styles_ then traits else traits <> [H.class' (generateClassName styles_)])
        (styleNode <> map go children)

   StyledText str ->
     H.text str

  where
    styleContent = getStyles styledHtml
      # filter (not <<< null)
      # nub
      >>= getClasses
      # joinWith "\n"

    styleNode = if styleContent == ""
        then []
        else [styleEl [] [H.text styleContent]]

    go (StyledHtml name_ styles_ traits_ children_) =
        H.element
          name_
          (if null styles_ then traits_ else snoc traits_ (H.class' (generateClassName styles_)))
          (map go children_)

    go (StyledText str) = H.text str

getClasses :: Array Style -> Array String
getClasses styles_ =
    (if null noPseudo then [] else [getClassString classSelector noPseudo])
    <> (getPseudoClassString Active)
    <> (getPseudoClassString Checked)
    <> (getPseudoClassString Disabled)
    <> (getPseudoClassString Enabled)
    <> (getPseudoClassString First)
    <> (getPseudoClassString FirstChild)
    <> (getPseudoClassString FirstOfType)
    <> (getPseudoClassString Focus)
    <> (getPseudoClassString FocusWithin)
    <> (getPseudoClassString Hover)
    <> (getPseudoClassString Indeterminate)
    <> (getPseudoClassString InRange)
    <> (getPseudoClassString Invalid)
    <> (getPseudoClassString LastChild)
    <> (getPseudoClassString LastOfType)
    <> (getPseudoClassString OnlyChild)
    <> (getPseudoClassString Optional)
    <> (getPseudoClassString OutOfRange)
    <> (getPseudoClassString ReadOnly)
    <> (getPseudoClassString ReadWrite)
    <> (getPseudoClassString Required)
    <> (getPseudoClassString Valid)
    <> (getPseudoClassString Visited)
    where
      classSelector = "." <> generateClassName styles_
      noPseudo = filter (getPseudoClass >>> eq Nothing) styles_

      getPseudoClassString pseudo_ =
        if null pseudoStyles
          then []
          else [getClassString (classSelector <> ":" <> pseudoClassString pseudo_) pseudoStyles]
        where
          pseudoStyles = filter (getPseudoClass >>> eq (Just pseudo_)) styles_

getClassString :: String -> Array Style -> String
getClassString selector styles_ = selector <> """ {
""" <> stylesString <> """
}"""
  where
    stylesString = map (\(Style _ key val) -> "    " <> key <> ":" <> val <> ";") styles_
       # joinWith "\n"

getStyles :: forall msg. StyledHtml msg -> Array (Array Style)
getStyles (StyledHtml _ styles_ _ children) = [styles_] <> bind children getStyles
getStyles _ = []

generateClassName :: Array Style -> String
generateClassName = show >>> quickHash >>> show >>> (<>) "_"

--

styleEl :: forall msg. Element msg
styleEl = element "style"

foreign import getElementName :: forall msg. Html msg -> String
foreign import getElementTraits :: forall msg. Html msg -> Array {key :: String, val:: String}
foreign import getElementChildren :: forall msg. Html msg -> Array (Html msg)
foreign import getElementText :: forall msg. Html msg ->  Nullable String

getElementTraitsString :: forall msg. Html msg -> String
getElementTraitsString h = getElementTraits h
  <#> (\{key:k, val} -> " " <> (if k == "className" then "class" else k) <> "=" <> show val)
  # joinWith ""

renderHtmlToString :: forall msg. Html msg -> String
renderHtmlToString html = case toMaybe $ getElementText html of
    Just t -> t
    Nothing -> "<"
      <> getElementName html
      <> getElementTraitsString html
      <> ">"
      <> joinWith "" (map renderHtmlToString (getElementChildren html))
      <> "</"
      <> getElementName html
      <> ">"
