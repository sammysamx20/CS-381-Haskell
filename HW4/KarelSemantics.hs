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
                                else Error ("Can't move forward to spot at: " ++ show p)
stmt PutBeeper _ w r = let p = getPos r
                       in if (getBag r) > 0
                              then OK (incBeeper p w) (decBag r)
                              else Error ("Robot doesn't have any beepers!")
stmt (Turn d) _ w r =  OK w (setFacing (cardTurn d (getFacing r)) r)
stmt (Block []) _ w r = OK w r
stmt (Block (s:ss)) d w r = case stmt s d w r of
                            OK w1 r1 -> stmt (Block ss) d w1 r1
                            nope -> nope


-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
