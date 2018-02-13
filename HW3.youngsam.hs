-- Sam Young: youngsam
-- Bradford Wong: wongbra
-- Benjamin Richards: richaben

module HW3 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--   * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--     functions for generating MiniMiniLogo programs. It contains the type
--     definitions for Mode, Cmd, and Prog.
--   * Render.hs contains code for rendering the output of a MiniMiniLogo
--     program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen Up) (x,(y,z)) = ((Up,(y,z)),Nothing)
cmd (Pen x) (Up,(y,z)) = ((x,(y,z)),Nothing)
cmd (Move x y) (Up,(a,b)) = ((Up,(x,y)),Nothing)
cmd (Move x y) (Down,(a,b)) = ((Down,(x,y)),Just((a,b),(x,y)))
-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
prog :: Prog -> State -> (State, [Line])
prog z x = case z of
  [] -> (x, [])
  (l:ls) -> case cmd l x of
    (newstate, Nothing) -> prog ls newstate
    (newstate, Just y) -> (\(x, ls) -> (x, y:ls)) (prog ls newstate)


--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
amazing :: Prog
amazing =  line 10 20 15 27 ++ line 15 27 25 27 ++ line 25 27 30 20 ++ line 30 20 30 8
           ++ line 30 8 25 1 ++ line 25 1 15 1 ++ line 15 1 10 8 ++ line 10 8 10 20
           ++ osubox 15 5 10 18
           ++ line 33 1 50 1 ++ line 50 1 50 14 ++ line 50 14 35 14
           ++ line 35 14 35 25 ++ line 35 25 50 25 ++ line 50 27 50 25 ++ line 50 27 33 27 ++ line 33 12 33 27
           ++ line 33 12 48 12 ++ line 48 12 48 3 ++ line 48 3 33 3 ++ line 33 3 33 1
           ++ line 53 27 53 1 ++ line 53 1 73 1 ++ line 73 1 73 27 ++ line 73 27 68 27 ++ line 68 27 68 11
           ++ line 68 11 65 7 ++ line 65 7 61 7 ++ line 61 7 58 11 ++ line 58 11 58 27 ++ line 58 27 53 27
           ++ nix 40 30 8 7
line:: Int -> Int -> Int -> Int -> Prog
line x1 y1 x2 y2 = [Pen Up, Move x1 y1, Pen Down, Move x2 y2, Pen Up]

osubox :: Int -> Int -> Int -> Int -> Prog
osubox x y a b = [Pen Up, Move x y, Pen Down,
           Move (x+a) y, Move (x+a) (y+b), Move x (y+b), Move x y]
