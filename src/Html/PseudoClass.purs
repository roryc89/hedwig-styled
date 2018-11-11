module Html.PseudoClass where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data PseudoClass
  = Active
  | Blank
  | Checked
  | Default
  | Defined
  | Dir
  | Disabled
  | Empty
  | Enabled
  | First
  | FirstChild
  | FirstOfType
  | Focus
  | FocusVisible
  | FocusWithin
  | Host
  | Hover
  | Indeterminate
  | InRange
  | Invalid
  | Lang
  | LastChild
  | LastOfType
  | Left
  | Link
  | Matches
  | Not
  | NthChild
  | NthLastChild
  | NthLastOfType
  | NthOfType
  | OnlyChild
  | OnlyOfType
  | Optional
  | OutOfRange
  | ReadOnly
  | ReadWrite
  | Required
  | Right
  | Root
  | Scope
  | Target
  | Valid
  | Visited

pseudoClassString :: PseudoClass -> String
pseudoClassString p = case p of
  Active -> "active"
  Blank -> "blank"
  Checked -> "checked"
  Default -> "default"
  Defined -> "defined"
  Dir -> "dir"
  Disabled -> "disabled"
  Empty -> "empty"
  Enabled -> "enabled"
  First -> "first"
  FirstChild -> "first-child"
  FirstOfType -> "first-of-type"
  Focus -> "focus"
  FocusVisible -> "focus-visible"
  FocusWithin -> "focus-within"
  Host -> "host"
  Hover -> "hover"
  Indeterminate -> "indeterminate"
  InRange -> "in-range"
  Invalid -> "invalid"
  Lang -> "lang"
  LastChild -> "last-child"
  LastOfType -> "last-of-type"
  Left -> "left"
  Link -> "link"
  Matches -> "matches"
  Not -> "not"
  NthChild -> "nth-child"
  NthLastChild -> "nth-last-child"
  NthLastOfType -> "nth-last-of-type"
  NthOfType -> "nth-of-type"
  OnlyChild -> "only-child"
  OnlyOfType -> "only-of-type"
  Optional -> "optional"
  OutOfRange -> "out-of-range"
  ReadOnly -> "read-only"
  ReadWrite -> "read-write"
  Required -> "required"
  Right -> "right"
  Root -> "root"
  Scope -> "scope"
  Target -> "target"
  Valid -> "valid"
  Visited -> "visited"

derive instance genericPseudoClass :: Generic PseudoClass _

derive instance eqPseudoClass :: Eq PseudoClass

derive instance ordPseudoClass :: Ord PseudoClass

instance showPseudoClass :: Show PseudoClass where
  show = genericShow
