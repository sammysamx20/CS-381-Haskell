-- Sam Young: youngsam
-- Bradford Wong: wongbra
-- Benjamin Richards: richaben

module MiniLogo where

import Data.List
import Prelude hiding (Num)

-- Step 1:
type Num = Int
type Macro = String
type Var = String

type Prog = [Cmd]

data Mode = Up | Down
            deriving(Eq,Show)

data Expr = Var Var
          | Num Num
          | Add Expr Expr
          deriving(Eq,Show)

data Cmd = Pen Mode
           | Move (Expr,Expr)
           | Define Macro [Var] Prog
           | Call Macro [Expr]
           deriving(Eq,Show)


-- Step 2:
--      Concrete syntax in a comment:
--      define line (x1,y1,x2,y2) {
--          pen up; move (x1,y1);
--          pen down; move (x2,y2);
--          pen up
--        }
--
--
--

line = Define "line" ["x1","x2","y1","y2"] [Pen Up, Move (Var "x1", Var "y1"), Pen Down, Move (Var "x2",Var "y2"), Pen Up]


-- Step 3:
--      Concrete syntax in a comment:
--      define nix (x,y,w,h) {
--          line(x, y, x+w, y+h);
--          line(x, y+h, x+w, y)
--        }
--
--
--


nix = Define "nix" ["x","y","w","h"] [Call "line" [Var "x", Var "y", Add (Var "x") (Var "w"), Add  (Var "y") (Var "h")], Call "line" [Var "x", Add (Var "y") (Var "h"), Add (Var "x") (Var "w"), Var "y"]]

-- Step 4:

steps :: Int -> Prog
steps 0 = [Pen Up, Move (Num (0), Num (0)), Pen Down, Pen Down]
steps x = reverse( drop 1 (reverse (steps (x-1)))) ++ [Move (Num (x-1), Num x) , Move (Num x, Num x), Pen Up]

-- Step 5:

macros :: Prog -> [Macro]
findMacros :: Cmd -> String
macros x = map findMacros x
findMacros (Define x _ _) = x
findMacros (Pen _) = []
findMacros (Move _) = []
findMacros (Call _ _) = []

-- Step 6:

pretty :: Prog -> String
pretty [] = [];
pretty (Define x y z:xs) = "Define " ++ x ++ show y ++ "{\n" ++ pretty z ++ "\n}\n" ++ pretty xs
pretty (Pen Up:xs) = "\nPen Up " ++ pretty xs
pretty (Pen Down:xs) = "\nPen Down " ++ pretty xs
pretty (Move x:xs) = "Move " ++ show x ++ " " ++ pretty xs
pretty (Call x y:xs) = "Call " ++ x ++ show y ++ " " ++ pretty xs


-- Bonus 1:

optE :: Expr -> Expr
optE (Add (Num y) (Num x)) = Num  (y + x)
optE (Add(Add (Num y) (Num x)) (Var z)) = Add(addExpr x y) (Var z)
optE x = x

-- This function is used to turn the expression into an expression so the add function can take it as an argument
addExpr :: Int -> Int -> Expr
addExpr x y = Num (x+y)


-- Bonus 2:

optP :: Prog -> Prog
optP [] = []
optP (x:xs) = oCmd x : optP xs

--helper function to turn the argument list in optp into a Cmd
oCmd :: Cmd -> Cmd
oCmd (Call x y) = Call x (map optE y)
oCmd (Define x y z) = Define x y (optP z)
oCmd (Move (x,y)) = Move ((optE x),(optE y))
oCmd (Pen x) = (Pen x)
