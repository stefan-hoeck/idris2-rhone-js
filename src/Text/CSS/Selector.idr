module Text.CSS.Selector

import Data.List
import Data.String
import Text.CSS.Property
import Web.Dom

%default total

||| CSS [pseudo classes](https://developer.mozilla.org/en-US/docs/Learn/CSS/Building_blocks/Selectors/Pseudo-classes_and_pseudo-elements#pseudo-classes)
||| Docstrings taken from the linked resource.
public export
data PseudoClass : Type where
  ||| Matches when the user activates (for example clicks on) an element.
  Active : PseudoClass
  ||| Matches both the :link and :visited states of a link.
  AnyLink : PseudoClass
  ||| Matches an <input> element whose input value is empty.
  Blank : PseudoClass
  ||| Matches a radio button or checkbox in the selected state.
  Checked : PseudoClass
  ||| Matches the element, or an ancestor of the element, that is currently being displayed.
  Current : PseudoClass
  ||| Matches the one or more UI elements that are the default among a set of similar elements.
  Default : PseudoClass
  ||| Select an element based on its directionality (value of the HTML dir attribute or CSS direction property).
  Dir : Direction -> PseudoClass
  ||| Matches user interface elements that are in an disabled state.
  Disabled : PseudoClass
  ||| Matches an element that has no children except optionally white space.
  Empty : PseudoClass
  ||| Matches user interface elements that are in an enabled state.
  Enabled : PseudoClass
  ||| In Paged Media, matches the first page.
  First : PseudoClass
  ||| Matches an element that is first among its siblings.
  FirstChild : PseudoClass
  ||| Matches an element which is first of a certain type among its siblings.
  FirstOfType : PseudoClass
  ||| Matches when an element has focus.
  Focus : PseudoClass
  ||| Matches when an element has focus and the focus should be visible to the user.
  FocusVisible : PseudoClass
  ||| Matches an element with focus plus an element with a descendent that has focus.
  FocusWithin : PseudoClass
  ||| Matches the elements after the current element.
  Future : PseudoClass
  ||| Matches when the user hovers over an element.
  Hover : PseudoClass
  ||| Matches UI elements whose value is in an indeterminate state, usually checkboxes.
  Indeterminate : PseudoClass
  ||| Matches an element with a range when its value is in-range.
  InRange : PseudoClass
  ||| Matches an element, such as an <input>, in an invalid state.
  Invalid : PseudoClass
  ||| Matches an element based on language (value of the HTML lang attribute).
  Lang : String -> PseudoClass
  ||| Matches an element which is last among its siblings.
  LastChild : PseudoClass
  ||| Matches an element of a certain type that is last among its siblings.
  LastOfType : PseudoClass
  ||| In Paged Media, matches left-hand pages.
  Left : PseudoClass
  ||| Matches unvisited links.
  Link : PseudoClass
  ||| Matches links pointing to pages that are in the same site as the current document.
  LocalLink : PseudoClass
  |||Matches elements from a list of siblings — the siblings are matched by a formula of the form an+b (e.g. 2n + 1 would match elements 1, 3, 5, 7, etc. All the odd ones.)
  NthChild : String -> PseudoClass
  |||Matches elements from a list of siblings — the siblings are matched by a formula of the form an+b (e.g. 2n + 1 would match elements 1, 3, 5, 7, etc. All the odd ones.)
  NthOfType : String -> PseudoClass
  ||| Matches elements from a list of siblings, counting backwards from the end. The siblings are matched by a formula of the form an+b (e.g. 2n + 1 would match the last element in the sequence, then two elements before that, then two elements before that, etc. All the odd ones, counting from the end.)
  NthLastChild : String -> PseudoClass
  ||| Matches elements from a list of siblings that are of a certain type (e.g. <p> elements), counting backwards from the end. The siblings are matched by a formula of the form an+b (e.g. 2n + 1 would match the last element of that type in the sequence, then two elements before that, then two elements before that, etc. All the odd ones, counting from the end.)
  NthLastOfType : String -> PseudoClass
  ||| Matches an element that has no siblings.
  OnlyChild : PseudoClass
  ||| Matches an element that is the only one of its type among its siblings.
  OnlyOfType : PseudoClass
  ||| Matches form elements that are not required.
  Optional : PseudoClass
  ||| Matches an element with a range when its value is out of range.
  OutOfRange : PseudoClass
  ||| Matches the elements before the current element.
  Past : PseudoClass
  ||| Matches an input element that is showing placeholder text.
  PlaceholderShown : PseudoClass
  ||| Matches an element representing an audio, video, or similar resource that is capable of being “played” or “paused”, when that element is “playing”.
  Playing : PseudoClass
  ||| Matches an element representing an audio, video, or similar resource that is capable of being “played” or “paused”, when that element is “paused”.
  Paused : PseudoClass
  ||| Matches an element if it is not user-alterable.
  ReadOnly : PseudoClass
  ||| Matches an element if it is user-alterable.
  ReadWrite : PseudoClass
  ||| Matches form elements that are required.
  Required : PseudoClass
  ||| In Paged Media, matches right-hand pages.
  Right : PseudoClass
  ||| Matches an element that is the root of the document.
  Root : PseudoClass
  ||| Matches any element that is a scope element.
  Scope : PseudoClass
  ||| Matches an element such as an <input> element, in a valid state.
  Valid : PseudoClass
  ||| Matches an element if it is the target of the current URL (i.e. if it has an ID matching the current URL fragment).
  Target : PseudoClass
  ||| Matches visited links.
  Visited : PseudoClass

namespace PseudoClass
  export
  render : PseudoClass -> String
  render Active            = "active"
  render AnyLink           = "any-link"
  render Blank             = "blank"
  render Checked           = "checked"
  render Current           = "current"
  render Default           = "default"
  render (Dir x)           = #"dir(\#{render x})"#
  render Disabled          = "disabled"
  render Empty             = "empty"
  render Enabled           = "enabled"
  render First             = "first"
  render FirstChild        = "first-child"
  render FirstOfType       = "first-of-type"
  render Focus             = "focus"
  render FocusVisible      = "focus-visible"
  render FocusWithin       = "focus-within"
  render Future            = "future"
  render Hover             = "hover"
  render Indeterminate     = "indeterminate"
  render InRange           = "in-range"
  render Invalid           = "invalid"
  render (Lang x)          = #"lang(\#{x})"#
  render LastChild         = "last-child"
  render LastOfType        = "last-of-type"
  render Left              = "left"
  render Link              = "link"
  render LocalLink         = "local-link"
  render (NthChild x)      = #"nth-child(\#{x})"#
  render (NthOfType x)     = #"nth-of-type(\#{x})"#
  render (NthLastChild x)  = #"nth-last-child(\#{x})"#
  render (NthLastOfType x) = #"nth-last-of-type(\#{x})"#
  render OnlyChild         = "only-child"
  render OnlyOfType        = "only-of-type"
  render Optional          = "optional"
  render OutOfRange        = "out-of-range"
  render Past              = "past"
  render PlaceholderShown  = "placeholder-shown"
  render Playing           = "playing"
  render Paused            = "paused"
  render ReadOnly          = "read-only"
  render ReadWrite         = "read-write"
  render Required          = "required"
  render Right             = "right"
  render Root              = "root"
  render Scope             = "scope"
  render Valid             = "valid"
  render Target            = "target"
  render Visited           = "visited"

public export
data Selector :  (dept : Nat)
              -> (hasPseudoClass : Bool)
              -> (hasPseudoElem  : Bool)
              -> Type where
  Star   : Selector 0 b1 b2
  Id     : String -> Selector 0 b1 b2
  Class  : String -> Selector 0 b1 b2
  Elem   : {str : _} -> (0 tpe : ElementType str t) -> Selector 0 b1 b2
  Many   : List (Selector 0 True True) -> Selector 1 True True
  Pseudo : Selector 0 False False -> PseudoClass -> Selector 0 True b2

export %inline
class : String -> Selector 0 False False
class = Class

export %inline
classes : List String -> Selector 1 True True
classes = Many . map Class

export %inline
elem : {str : _} -> (0 tpe : ElementType str t) -> Selector 0 False False
elem = Elem

export %inline
id : String -> Selector 0 False False
id = Id

export
render : Selector n b1 b2 -> String
render Star           = "*"
render (Id x)         = "#" ++ x
render (Class x)      = "." ++ x
render (Elem {str} _) = str
render (Many ss)      = fastConcat . intersperse ", " $ map render ss
render (Pseudo s p)   = #"\#{render s}:\#{render p}"#
