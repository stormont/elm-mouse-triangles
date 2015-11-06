
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse
import Window


maxPathLength = 2  -- Define our path to be limited to two points (zero-indexed).


-- MODEL

type alias MouseX = Int
type alias MouseY = Int
type alias Point = (MouseX, MouseY)                     -- Mouse position (x,y).
type alias WasClick = Bool                              -- Was the last Signal a click?
type Model = NoPath       WasClick                      -- No current path.
           | ActivePath   WasClick (List Point, Point)  -- An active path, plus an actively moving position.
           | FinishedPath WasClick (List Point)         -- A completed (inactive) path.


-- UPDATE

update : (Point, Point) -> Model -> Model
update (p,movePoint) model =
  let
    isClick = p == movePoint  -- Check if this is a mouse click (defined by our mouse
                              --   position being equal to our last click).
  in
    case model of             -- Just pass through the various arguments to update.
      NoPath       _                 -> updateEmptyPath    isClick          p
      ActivePath   wasClick (ps, mp) -> updateActivePath   isClick wasClick ps (p,movePoint)
      FinishedPath wasClick ps       -> updateFinishedPath isClick wasClick ps model


updateEmptyPath : Bool -> Point -> Model
updateEmptyPath isClick p =
 if isClick                        -- If a mouse click has occurred...
    then ActivePath True ([p], p)  --   ... define a new path, otherwise...
    else NoPath False              --   ... it was just a mouse movement, so we can
                                   --   ignore it.


updateActivePath : Bool -> Bool -> List Point -> (Point,Point) -> Model
updateActivePath isClick wasClick ps (p,movePoint) =
  if not isClick || wasClick                          -- Were either of the previous Signals a click?
    then ActivePath False (ps, movePoint)             -- If not, just return the model.
    else
      let
        path = p :: ps                                -- Prepend the new mouse Signal.
      in
        if List.length path <= maxPathLength          -- If we're not at the bounds of the path...
          then ActivePath   True (path, p)            --   ... Update the current path, otherwise...
          else FinishedPath True (completePath path)  --   ... Complete the path.


completePath : List Point -> List Point
completePath points =
  case (List.head <| List.reverse points) of  -- Get the very first point and...
    Nothing -> points                         --   ... if none exists, we have nothing to update, otherwise...
    Just h  -> h :: points                    --   ... add the point to the existing points.


updateFinishedPath : Bool -> Bool -> List Point -> Model -> Model
updateFinishedPath isClick wasClick points model =
  if not isClick                              -- If no mouse click just occurred...
    then model                                --   ... it's just a mouse movement;
                                              --   return the existing model.
    else case wasClick of                     -- If the last path Signal was a click...
          True  -> FinishedPath False points  --   ... set up the path to be restarted, otherwise...
          False -> NoPath       False         --   ... restart at an empty path.


-- VIEW

lineStyle : LineStyle
lineStyle =
  { defaultLine          -- Extend the "default" definition.
      | width <- 10.0    -- Line width.
      , color <- blue    -- Assign the color.
      , cap   <- Round   -- The shape of the end points.
      , join  <- Smooth  -- The shape of the joints.
  }


drawLineSegments : (Int,Int) -> List Point -> Form
drawLineSegments (w,h) points =
  List.map (\(x,y) -> (toFloat x, toFloat -y)) points  -- Convert the mouse points to
                                                       --   "model" coordinates.
    |> path                                            -- Build a path from the points.
    |> traced lineStyle                                -- Trace the line with defined form.
    |> move (-(toFloat w) / 2, (toFloat h) / 2)        -- Move drawing from middle to upper
                                                       --   left ("screen" coordinates).


view : (Int,Int) -> Model -> Element
view (w,h) model =
  case model of
    NoPath       _         ->                           -- If no path is currently defined...
      collage w h []                                    --   ... build an empty canvas.
    ActivePath   _ (ps, p) ->                           -- If an actively moving path is defined...
      collage w h [ drawLineSegments (w,h) (p :: ps) ]  --   ... draw the line segments, with the
                                                        --   active motion.
    FinishedPath _ ps      ->                           -- If a completed path is defined...
      collage w h [ drawLineSegments (w,h) ps ]         --   ... draw the line segments, with no
                                                        --   active motion.


-- SIGNALS

sampleOnClick : Signal Point
sampleOnClick =
  Signal.sampleOn   -- Each time a...
    Mouse.clicks    --   ... mouse click occurs, return...
    Mouse.position  --   ... the mouse position.


mergeMouse : Signal (Point, Point)
mergeMouse =
  Signal.map2       -- Each time either event happens...
    (,)             --   ... collect both...
    sampleOnClick   --   ... the last mouse click...
    Mouse.position  --   ... and the current mouse position.


mousePositions : Signal Model
mousePositions =
  Signal.foldp      -- Fold each signal into an accumulated model...
    update          --   ... through the update function.
    (NoPath False)  -- Start with an empty path.
    mergeMouse      -- Updates given by mouse position changes.


main : Signal Element
main =
  Signal.map2          -- Map two signals together...
    view               --   ... through the view function.
    Window.dimensions  -- Use updates to the window dimensions as the first signal.
    mousePositions     -- Use updates to mouse positions as the second signal.
