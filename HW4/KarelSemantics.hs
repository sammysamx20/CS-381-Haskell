-- Sam Young: youngsam
-- Bradford Wong: wongbra
-- Benjamin Richards: richaben


module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not x) y z = not (test x y z)
test (Facing x) y (p,c,b) = (c == x)
test (Clear x) y (p,c,b) = isClear (neighbor(cardTurn x c) p) y
test (Beeper) y (p,c,b) = hasBeeper p y
test (Empty) y (p,c,b) = (b <= 0)

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt Move _ w r = let p = relativePos Front r
                          in if isClear p w
                                then OK (w) (setPos p r)
                                else Error ("Blocked at: " ++ show p)
stmt PutBeeper _ w r = let p = getPos r
                       in if (getBag r) > 0
                              then OK (incBeeper p w) (decBag r)
                              else Error ("No beeper to put.")
stmt (Turn d) _ w r =  OK w (setFacing (cardTurn d (getFacing r)) r)
stmt (Block []) _ w r = OK w r
stmt (Block (s:ss)) d w r = case stmt s d w r of
                            OK w1 r1 -> stmt (Block ss) d w1 r1
                            nope -> nope
stmt (If (t) f s) d w r = if (test t w r)
                            then stmt f d w r
                            else stmt s d w r
stmt (Call m) d w r = case (lookup m d) of
                      Just a -> stmt a d w r
                      Nothing -> Error ("Undefined macro: " ++ m)
stmt (Iterate 1 com) d w r = stmt com d w r
stmt (Iterate num com) d w r = case stmt com d w r of
                            OK w1 r1 -> stmt (Iterate (num-1) com) d w1 r1
                            nope -> nope
stmt (While tst sta) d w r = case stmt sta d w r of
                               OK w1 r1 -> if (test tst w1 r1)
                                            then stmt (While tst sta) d w1 r1
                                            else stmt sta d w r
                               nope -> nope


-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
