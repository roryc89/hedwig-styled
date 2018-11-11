module Html.Styled where

import Prelude

import Data.Array (filter, nub, null)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.String (joinWith)
import Effect (Effect)
import Hedwig (Html, Trait, element, (:>))
import Hedwig as H
import Hedwig.Element (Element)

foreign import quickHash :: String -> Int

type Model = Int

init :: Model
init = 0

data Msg = Increment | Decrement

update :: Model -> Msg -> Model
update model = case _ of
  Increment -> model + 1
  Decrement -> model - 1

view :: Model -> H.Html Msg
view model = toUnstyled $
 div [Style Nothing "margin" "auto"] []
   [ button
     [ Style Nothing "color" "red"
     , Style (Just Hover) "color" "blue"
     ]
     []
     [ StyledText "helllloooo" ],
    button [] [H.onClick Decrement] [StyledText "-"],
    StyledText (show model),
    button [] [H.onClick Increment] [StyledText "+"]
   ]
 -- H.main [H.id "main"] [
 --    H.button [H.onClick Decrement] [H.text "-"],
 --    H.text (show model),
 --    H.button [H.onClick Increment] [H.text "+"]
 --  ]

main :: Effect Unit
main = do
  H.mount "main" {
    init: init :> [],
    update: \msg model -> update msg model :> [],
    view
  }


multiply :: Number -> Number -> Number
multiply x y = x * y

times2 :: Number -> Number
times2 = multiply 2.0

data Style
  = Style (Maybe PseudoSelector) String String
  -- | PseudoStyle PseudoSelector String String

getPseudoSelector :: Style -> Maybe PseudoSelector
getPseudoSelector (Style ps _ _ ) = ps

derive instance genericStyle :: Generic Style _

derive instance eqStyle :: Eq Style

derive instance ordStyle :: Ord Style

instance showStyle :: Show Style where
  show = genericShow

data PseudoSelector = Hover | Active

derive instance genericPseudoSelector :: Generic PseudoSelector _

derive instance eqPseudoSelector :: Eq PseudoSelector

derive instance ordPseudoSelector :: Ord PseudoSelector

instance showPseudoSelector :: Show PseudoSelector where
  show = genericShow

data StyledHtml msg
  = StyledHtml String (Array Style) (Array (Trait msg)) (Array (StyledHtml msg))
  | StyledText String

derive instance genericStyledHtml :: Generic (StyledHtml msg) _

button :: forall msg. Array Style -> Array (Trait msg) -> Array (StyledHtml msg) -> StyledHtml msg
button = StyledHtml "button"

div :: forall msg. Array Style -> Array (Trait msg) -> Array (StyledHtml msg) -> StyledHtml msg
div = StyledHtml "div"

span :: forall msg. Array Style -> Array (Trait msg) -> Array (StyledHtml msg) -> StyledHtml msg
span = StyledHtml "span"

toUnstyled :: forall msg. StyledHtml msg -> H.Html msg
toUnstyled styledHtml =

  case styledHtml of
   StyledHtml name styles traits children ->

      H.element
        name
        (if null styles then traits else traits <> [H.class' (generateClassName styles)])
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
          (if null styles_ then traits_ else traits_ <> [ H.class' (generateClassName styles_) ])
          (map go children_)

    go (StyledText str) = H.text str

getClasses :: Array Style -> Array String
getClasses styles =
    (if null noPseudo then [] else [getClassString classSelector noPseudo])
    <> (if null hover then [] else [getClassString (classSelector <> ":hover") hover])
    where
      classSelector = "." <> generateClassName styles
      noPseudo = filter (getPseudoSelector >>> eq Nothing) styles
      hover = filter (getPseudoSelector >>> eq (Just Hover)) styles


getClassString :: String -> Array Style -> String
getClassString selector styles = selector <> """ {
""" <> stylesString <> """
}"""
  where
    stylesString = map (\(Style _ key val) -> "    " <> key <> ":" <> val <> ";") styles
       # joinWith "\n"

getStyles :: forall msg. StyledHtml msg -> Array (Array Style)
getStyles (StyledHtml _ styles _ children) = [styles] <> bind children getStyles
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
