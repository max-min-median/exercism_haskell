module RationalNumbers
(Rational,
 abs,
 numerator,
 denominator,
 add,
 sub,
 mul,
 div,
 pow,
 expRational,
 expReal,
 rational) where

import Prelude hiding (div, abs, Rational)
import qualified Prelude as P

-- Data definition -------------------------------------------------------------
data Rational a = a :/ a deriving (Eq, Show)

rational :: Integral a => (a, a) -> Rational a
rational (a, b) = let a' = P.abs a
                      b' = P.abs b
                      sgn = if a*b < 0 then -1 else 1
                      g = gcd a' b'
                  in  (sgn * a' `P.div` g) :/ (b' `P.div` g)

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (a :/ b) = P.abs a :/ P.abs b

numerator :: Integral a => Rational a -> a
numerator (a :/ _) = a

denominator :: Integral a => Rational a -> a
denominator (_ :/ a) = a

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (a :/ b) (c :/ d) = rational (a*d + b*c, b*d)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (a :/ b) (c :/ d) = rational (a*d - b*c, b*d)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (a :/ b) (c :/ d) = rational (a*c, b*d)

div :: Integral a => Rational a -> Rational a -> Rational a
div (a :/ b) (c :/ d) = rational (a*d, b*c)

pow :: Integral a => Rational a -> a -> Rational a
pow (a :/ b) n
  | n < 0     = pow (rational (b, a)) (-n)
  | otherwise = (a^n) :/ (b^n)

expRational :: Integral a => Floating b => Rational a -> b -> b
expRational (a :/ b) n = (fromIntegral a / fromIntegral b) ** n

expReal :: Floating a => Integral b => a -> Rational b -> a
expReal a (b :/ c) = a ** (fromIntegral b / fromIntegral c)
