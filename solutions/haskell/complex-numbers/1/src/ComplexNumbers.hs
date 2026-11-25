module ComplexNumbers
(Complex,
 conjugate,
 abs,
 exp,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 complex) where

import Prelude hiding (div, abs, exp)
import qualified Prelude as P

-- Data definition -------------------------------------------------------------
data Complex a = !a :+ !a deriving (Eq, Show)

complex :: (a, a) -> Complex a
complex = uncurry (:+)

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (x :+ y) = x :+ (-y)

abs :: Floating a => Complex a -> a
abs (x :+ y) = sqrt $ x*x + y*y

real :: Num a => Complex a -> a
real (x :+ _) = x

imaginary :: Num a => Complex a -> a
imaginary (_ :+ y) = y

-- e^a * (cos(b) + i * sin(b))
exp :: Floating a => Complex a -> Complex a
exp (a :+ b) = let eA = P.exp a in (eA*cos b) :+ (eA*sin b)

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (a :+ b) (c :+ d) = (a*c - b*d) :+ (a*d + b*c)

add :: Num a => Complex a -> Complex a -> Complex a
add (a :+ b) (c :+ d) = (a+c) :+ (b+d)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (a :+ b) (c :+ d) = (a-c) :+ (b-d)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (a :+ b) (c :+ d) = let denom = c*c + d*d in ((a*c + b*d)/denom) :+ ((b*c - a*d)/denom)