module Util

open FSharpQt.MiscTypes

let dist (p1: Point) (p2: Point) =
    let dx = p1.X - p2.X
    let dy = p1.Y - p2.Y
    (dx * dx + dy * dy) |> float |> sqrt
    
let dist2 (p1: PointF) (p2: PointF) =
    let dx = p1.X - p2.X
    let dy = p1.Y - p2.Y
    (dx * dx + dy * dy) |> float |> sqrt
  
