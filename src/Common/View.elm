module Common.View (px, pct) where


px : Int -> String
px n =
  toString n ++ "px"


pct : Int -> String
pct n =
  toString n ++ "%"
