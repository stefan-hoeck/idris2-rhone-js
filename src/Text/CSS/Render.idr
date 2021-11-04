module Text.CSS.Render

||| An interface for rendering CSS rules, selectors and
||| properties.
public export
interface Render t where
  render : t -> String
