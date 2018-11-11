module Html.Styled.PseudoClass where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data PseudoClass
  = Active
  | Checked
  | Disabled
  | Enabled
  | First
  | FirstChild
  | FirstOfType
  | Focus
  | FocusWithin
  | Hover
  | Indeterminate
  | InRange
  | Invalid
  | LastChild
  | LastOfType
  | OnlyChild
  | Optional
  | OutOfRange
  | ReadOnly
  | ReadWrite
  | Required
  | Valid
  | Visited

pseudoClassString :: PseudoClass -> String
pseudoClassString = case _ of
  Active -> "active"
  Checked -> "checked"
  Disabled -> "disabled"
  Enabled -> "enabled"
  First -> "first"
  FirstChild -> "first-child"
  FirstOfType -> "first-of-type"
  Focus -> "focus"
  FocusWithin -> "focus-within"
  Hover -> "hover"
  Indeterminate -> "indeterminate"
  InRange -> "in-range"
  Invalid -> "invalid"
  LastChild -> "last-child"
  LastOfType -> "last-of-type"
  OnlyChild -> "only-child"
  Optional -> "optional"
  OutOfRange -> "out-of-range"
  ReadOnly -> "read-only"
  ReadWrite -> "read-write"
  Required -> "required"
  Valid -> "valid"
  Visited -> "visited"

derive instance genericPseudoClass :: Generic PseudoClass _

derive instance eqPseudoClass :: Eq PseudoClass

derive instance ordPseudoClass :: Ord PseudoClass

instance showPseudoClass :: Show PseudoClass where
  show = genericShow
