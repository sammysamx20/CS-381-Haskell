module Nat where

import Prelude hiding (Enum(..), sum)


--
-- * Part 2: Natural numbers
--

-- | The natural numbers.
data Nat = Zero
         | Succ Nat
  deriving (Eq,Show)

-- | The number 1.
one :: Nat
one = Succ Zero

-- | The number 2.
two :: Nat
two = Succ one

-- | The number 3.
three :: Nat
three = Succ two

-- | The number 4.
four :: Nat
four = Succ three


-- | The predecessor of a natural number.
--
--   >>> pred Zero
--   Zero
--
--   >>> pred three
--   Succ (Succ Zero)
--
pred :: Nat -> Nat
pred Zero = Zero
pred (Succ x) = x
-- | the Succ x is a function which takes the argument and then calls the nat before




-- | True if the given value is zero.
--
--   >>> isZero Zero
--   True
--
--   >>> isZero two
--   False
--
isZero :: Nat -> Bool
isZero Zero = True
isZero x = False


-- | Convert a natural number to an integer.
--
--   >>> toInt Zero
--   0
--
--   >>> toInt three
--   3
--
toInt :: Nat -> Int
toInt Zero = 0
toInt  x =  1 + toInt (pred x)

-- this uses recursion to keep adding an integer of 1 until it hits the nat number Zero

-- | Add two natural numbers.
--
--   >>> add one two
--   Succ (Succ (Succ Zero))
--
--   >>> add Zero one == one
--   True
--
--   >>> add two two == four
--   True
--
--   >>> add two three == add three two
--   True
--
add :: Nat -> Nat -> Nat
add (x) (y) =
  if (y) == Zero
    then x
    else
        add ( Succ x) (pred y)

-- | Subtract the second natural number from the first. Return zero
--   if the second number is bigger.
--
--   >>> sub two one
--   Succ Zero
--
--   >>> sub three one
--   Succ (Succ Zero)
--
--   >>> sub one one
--   Zero
--
--   >>> sub one three
--   Zero
--
sub :: Nat -> Nat -> Nat
sub (x) (y) =
  if ( y) == Zero
    then x
    else
        sub ( pred x) (pred y)


-- | Is the left value greater than the right?
--
--   >>> gt one two
--   False
--
--   >>> gt two one
--   True
--
--   >>> gt two two
--   False
--
gt :: Nat -> Nat -> Bool
gt (x) (y) =
  if (x) == Zero
  then False
    else if (y) == Zero
      then True
        else
          gt (pred x) (pred y)

-- | Multiply two natural numbers.
--
--   >>> mult two Zero
--   Zero
--
--   >>> mult Zero three
--   Zero
--
--   >>> toInt (mult two three)
--   6
--
--   >>> toInt (mult three three)
--   9

mult :: Nat -> Nat -> Nat
mult Zero y = Zero
mult x Zero = Zero
mult x y = add( x )(mult ( x) (pred  y))



-- | Compute the sum of a list of natural numbers.
--
--   >>> sum []
--   Zero
--
--   >>> sum [one,Zero,two]
--   Succ (Succ (Succ Zero))
--
--   >>> toInt (sum [one,two,three])
--   6
--
sum :: [Nat] -> Nat
sum [] = Zero
sum (x:y) = add (x) (sum y)

-- | An infinite list of all of the *odd* natural numbers, in order.
--
--   >>> map toInt (take 5 odds)
--   [1,3,5,7,9]
--
--   >>> toInt (sum (take 100 odds))
--   10000
--
odds :: [Nat]
odds  = iterate everyother one

-- | this is the helper function for odds
everyother :: Nat -> Nat
everyother x = Succ(Succ x)
