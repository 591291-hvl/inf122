module Week41Exercise2 where

import Data.Map (Map)
import qualified Data.Map as Map

data Expr a
  = Var a
  | Lit Integer
  | Mul (Expr a) (Expr a)
  | Add (Expr a) (Expr a)
  deriving (Eq, Show)

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

eval :: (Ord variable, Num value) => Expr variable -> Map variable value -> Maybe value
eval (Var varible) map = if isNothing (Map.lookup varible map) then Nothing else Just $ (\(Just a) -> a) (Map.lookup varible map)
eval (Lit value) map = Just $ fromIntegral value
eval (Mul exprLeft exprRight) map = mulFunc (eval exprLeft map) (eval exprRight map)
eval (Add exprLeft exprRight) map = addFunc (eval exprLeft map) (eval exprRight map)

mulFunc :: (Num value) => Maybe value -> Maybe value -> Maybe value
mulFunc Nothing _ = Nothing
mulFunc _ Nothing = Nothing
mulFunc (Just expr1) (Just expr2) = Just (expr1 * expr2)

addFunc :: (Num value) => Maybe value -> Maybe value -> Maybe value
addFunc Nothing _ = Nothing
addFunc _ Nothing = Nothing
addFunc (Just expr1) (Just expr2) =  Just (expr1 + expr2)


m1 = Map.fromList [("x", 2), ("y", 4)]

m2 = Map.fromList [("x", 2)]

e = Add (Mul (Var "x") (Lit 3)) (Add (Lit 15) (Var "y"))

main = print $ (eval e m1)
