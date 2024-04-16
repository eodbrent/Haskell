{-
Computation Theory
Lambda Calculus
-}
import Text.Show.Functions

add = \x -> \y -> \a -> \b -> (x a) (y a b)

s :: ((a -> a) -> a -> a) -> (a -> a) -> a -> a
s = \x -> \y -> \z -> y (x y z)

true = \x -> \y -> x
false = \x -> \y -> y

ifThenElse :: (c -> c -> c) -> c -> c -> c
ifThenElse boolExpr trueExpr falseExpr = boolExpr trueExpr falseExpr

church 0 = \x -> \y -> y -- provided
church n = \f -> \x -> f (church (n - 1) f x)
-- peano PROVIDED --
peano lexp = lexp (\xs -> ('S':xs)) "0"

myLambdaProgram = \m n -> m (\_ -> add n (church 7)) (add n (church 3))
