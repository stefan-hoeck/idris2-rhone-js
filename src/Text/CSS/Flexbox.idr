module Text.CSS.Flexbox

%default total

namespace FlexDirection
  public export
  data FlexDirection =
      Row
    | RowReverse
    | Column
    | ColumnReverse

  export
  render : FlexDirection -> String
  render Row           = "row"
  render RowReverse    = "row-reverse"
  render Column        = "column"
  render ColumnReverse = "column-reverse"

namespace FlexAlign
  public export
  data FlexAlign =
      Normal
    | Stretch
    | Center
    | Start
    | End
    | FlexStart
    | FlexEnd
    | Baseline
    | FirstBaseline
    | LastBaseline

  export
  render : FlexAlign -> String
  render Normal        = "normal"
  render Stretch       = "stretch"
  render Center        = "center"
  render Start         = "start"
  render End           = "end"
  render FlexStart     = "flex-start"
  render FlexEnd       = "flex-end"
  render Baseline      = "baseline"
  render FirstBaseline = "first baseline"
  render LastBaseline  = "last baseline"

namespace FlexJustify
  public export
  data FlexJustify =
      Center
    | Start
    | End
    | FlexStart
    | FlexEnd
    | Left
    | Right
    | Normal
    | SpaceBetween
    | SpaceAround
    | SpaceEvenly
    | Stretch

  export
  render : FlexJustify -> String
  render Center       = "center"
  render Start        = "start"
  render End          = "end"
  render FlexStart    = "flex-start"
  render FlexEnd      = "flex-end"
  render Left         = "left"
  render Right        = "right"
  render Normal       = "normal"
  render SpaceBetween = "space-between"
  render SpaceAround  = "space-around"
  render SpaceEvenly  = "space-evenly"
  render Stretch      = "stretch"
