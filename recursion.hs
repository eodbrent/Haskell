-- recursion - need to clean up
-- BEGIN PROVIDED CODE --
import Debug.Trace
type PN = [Char]
type Vec = [PN]

--Debug
sh0 :: PN -> PN
sh0 x = trace ("- " ++ x) x

sh1 :: PN -> PN
sh1 x = trace ("---- " ++ x) x

-- 1) Zero function
z :: Vec -> PN
z x = "0"

-- 2a) Successor function
s :: Vec -> PN
s x = ('S':(head x))

-- 3) Projection function
pr :: Int -> (Vec -> PN)
pr n (x:xs)
    | n <= 1 = x
    | otherwise = pr (n - 1) xs

-- 4) Composition
circ :: (Vec -> PN) -> [(Vec -> PN)] -> (Vec -> PN)
circ f gs = \x -> f $ sequence gs x

-- 5) Primitive recursion operator
rho :: (Vec -> PN) -> (Vec -> PN) -> (Vec -> PN)
rho f g = h
    where h ("0":xs) =  sh0 $ f xs
          h (('S':n):xs) =  sh1 $ g $ n:((h (n:xs)):xs)

-- Addition
add :: Vec -> PN
add vec = case vec of
    [x, y] -> rho f_add g_add [y, "0", x]
f_add :: Vec -> PN
f_add vec = last vec
g_add :: Vec -> PN
g_add vec = s [pr 2 vec]
-- END PROVIDED CODE --

-- 2b) Predecessor function
--     takes a Vec of PNs, gets the first PN and returns its predecessor (-1)
--     If the first PN is "0", returns "0", otherwise removes one "S"
p :: Vec -> PN
p ("0":_) = "0"  -- base case, predecessor of 0 is 0
p (('S':n):_) = n

-- Subtraction
--  takes vec of two PNs, x and y.
--  returns result of x - y
sub :: Vec -> PN
sub vec = case vec of -- need to remove 'case of' edited code and forgot to remove this where i used it.
    [x, y] -> rho f_sub g_sub [y, "0", x]
-- subtraction base_case
--   returns last vec element (x - 0 = x)
f_sub :: Vec -> PN
f_sub vec = last vec
-- subtraction recursive_case
-- uses predecessor (p) instead of successor (s)
g_sub :: Vec -> PN
g_sub vec = p [pr 2 vec]

-- Multiplication
--  takes vec of two PNs, x and y
--  returns product x * y
mult :: Vec -> PN
mult vec = case vec of
    [x, y] -> rho f_mult (g_mult x) [y, "0"]
-- mult base_case
-- returns 0 if 0 - as in multiplication
f_mult :: Vec -> PN
f_mult _ = "0"
-- mult recursive case - uses addition:
-- x + x, y times
g_mult :: PN -> Vec -> PN
g_mult x vec = add [x, pr 2 vec]

-- Exponentiation
--  takes vec of two PNs, x and y
--  returns result x^y
expo :: Vec -> PN
expo vec = case vec of
    [x, y] -> rho f_expo (g_expo x) [y, "S0"]
-- expo base_case
--  any number to the power 0 is 1
f_expo :: Vec -> PN
f_expo _ = "S0"
-- expo recursive_case
-- x1 * x2 (x1 + x1, x2 times), y times
g_expo :: PN -> Vec -> PN
g_expo x vec = mult [x, pr 2 vec]

-- boolean signature
--   takes a Vec of PNs and returns a PN.
--   Returns "0" for 0, returns "S0" (1) for any nonzero
sig :: Vec -> PN
sig vec = case vec of
    ["0"] -> "0"
    _     -> "S0"

-- Equality
--  takes vec of two PNs, x and y
--  returns "0" (0) if equal, "S0" (1) otherwise
--  uses subtraction and boolean sig to determine equality
eq :: Vec -> PN
eq vec = case vec of
    [x, y] -> circ sig [subFn] [x, y]
-- equality helper function
--  applies subtraction function to vec
subFn :: Vec -> PN
subFn vec = sub vec
