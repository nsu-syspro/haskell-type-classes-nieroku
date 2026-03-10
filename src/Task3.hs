{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Task3 where

import Data.List (sort)
import Data.List.NonEmpty (group)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromJust, fromMaybe)
import Task1 (Parse (..))
import Task2

instance Parse Bool where
  parse = const Nothing

data BoolOp = And | Or | Xor
  deriving (Show)

instance Parse BoolOp where
  parse "and" = Just And
  parse "or" = Just Or
  parse "xor" = Just Xor
  parse _ = Nothing

instance Eval Bool BoolOp where
  evalBinOp And = (&&)
  evalBinOp Or = (||)
  evalBinOp Xor = (/=)

variableNames :: Expr a op -> [String]
variableNames = ((map NonEmpty.head) . group . sort . go)
  where
    go (Lit _) = []
    go (Var name) = [name]
    go (BinOp _ x y) = (concat . (map variableNames)) [x, y]

-- | Solves SAT problem for given boolean formula written in Reverse Polish Notation
--
-- Returns whether given formula is satifiable
-- wrapped into 'Maybe' with 'Nothing' indicating parsing failure.
--
-- Only following binary operations are allowed:
-- - and
-- - or
-- - xor
--
-- All other words are considered as variables.
--
-- Usage example:
--
-- >>> solveSAT "x y and"
-- Just True
-- >>> solveSAT "x y xor x y and and"
-- Just False
-- >>> solveSAT "x y xor x y and or"
-- Just True
-- >>> solveSAT "x xor"
-- Nothing
solveSAT :: String -> Maybe Bool
solveSAT rawExpr = sat <$> parsedExpr
  where
    sat expr = or $ map (fromJust . flip evalExpr expr) allMappings

    parsedExpr :: Maybe (Expr Bool BoolOp)
    parsedExpr = parse rawExpr

    variables = fromMaybe [] (variableNames <$> parsedExpr)

    allMappings = map (zip variables) (cartesianPower [False, True] (length variables))

    cartesianPower :: [a] -> Int -> [[a]]
    cartesianPower _ n | n < 0 = undefined
    cartesianPower _ 0 = [[]]
    cartesianPower set n = [x : xs | x <- set, xs <- cartesianPower set (n - 1)]
