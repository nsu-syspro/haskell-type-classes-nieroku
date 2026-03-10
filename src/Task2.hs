-- The above pragma enables all warnings
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Task2 where

import Control.Applicative ((<|>))
import Data.Function ((&))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Task1 (Parse (..))

-- * Expression data type

-- | Generalized representation of expressions comprising
-- - Literals of type 'a'
-- - Variables with arbitrary 'String' names
-- - Binary operations of type 'op'
data Expr a op
  = Lit a
  | Var String
  | BinOp op (Expr a op) (Expr a op)
  deriving (Show)

-- | Integer binary operations
data IntOp = Add | Mul | Sub
  deriving (Show)

instance Parse IntOp where
  parse "+" = Just Add
  parse "-" = Just Sub
  parse "*" = Just Mul
  parse _ = Nothing

-- * Parsing

-- | Parses given expression in Reverse Polish Notation
-- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
--
-- Usage example:
--
-- >>> parse "2" :: Maybe (Expr Integer IntOp)
-- Just (Lit 2)
-- >>> parse "2 3 -" :: Maybe (Expr Integer IntOp)
-- Just (BinOp Sub (Lit 2) (Lit 3))
-- >>> parse "3 2 * 3 +" :: Maybe (Expr Integer IntOp)
-- Just (BinOp Add (BinOp Mul (Lit 3) (Lit 2)) (Lit 3))
-- >>> parse "2 +" :: Maybe (Expr Integer IntOp)
-- Nothing
-- >>> parse "2 3" :: Maybe (Expr Integer IntOp)
-- Nothing
instance (Parse a, Parse op) => Parse (Expr a op) where
  parse = result . (foldl' (&) (Just [])) . (map parseStackOp) . words
    where
      put :: Expr a op -> Maybe [Expr a op] -> Maybe [Expr a op]
      put = fmap . (:)

      performBinOp :: op -> Maybe [Expr a op] -> Maybe [Expr a op]
      performBinOp op (Just (rhs : lhs : stack)) = Just ((BinOp op lhs rhs) : stack)
      performBinOp _ _ = Nothing

      parseStackOp :: String -> (Maybe [Expr a op] -> Maybe [Expr a op])
      parseStackOp word = lit <|> binOp & (fromMaybe var)
        where
          lit = put . Lit <$> parse word
          binOp = performBinOp <$> parse word
          var = put $ Var word

      result :: Maybe [Expr a op] -> Maybe (Expr a op)
      result (Just [expr]) = Just expr
      result _ = Nothing

-- * Evaluation

-- | Class of evaluatable types
class Eval a op where
  -- | Evaluates given binary operation with provided arguments
  evalBinOp :: op -> a -> a -> a

instance Eval Integer IntOp where
  evalBinOp Add = (+)
  evalBinOp Sub = (-)
  evalBinOp Mul = (*)

-- | Evaluates given 'Expr' using given association list of variable values
--
-- Returns 'Nothing' in case appropriate variable value is missing.
--
-- Usage example:
--
-- >>> evalExpr [] (Lit 2 :: Expr Integer IntOp)
-- Just 2
-- >>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "x")) :: Maybe Integer
-- Just 5
-- >>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "y")) :: Maybe Integer
-- Nothing
evalExpr :: (Eval a op) => [(String, a)] -> Expr a op -> Maybe a
evalExpr variables = eval'
  where
    eval' (Lit value) = Just value
    eval' (Var var) = snd <$> find ((== var) . fst) variables
    eval' (BinOp op lhs rhs) = liftA2 (evalBinOp op) (eval' lhs) (eval' rhs)

-- | Parses given integer expression in Reverse Polish Notation and evaluates it
-- using given association list of variable values
--
-- Returns 'Nothing' in case the expression could not be parsed
-- or in case appropriate variable value is missing.
--
-- Usage example:
--
-- >>> evaluateInteger [] "2"
-- Just 2
-- >>> evaluateInteger [("x", 3)] "2 x -"
-- Just (-1)
-- >>> evaluateInteger [("x", 3)] "2 y -"
-- Nothing
-- >>> evaluateInteger [] "3 2 * 3 +"
-- Just 9
-- >>> evaluateInteger [] "2 +"
-- Nothing
-- >>> evaluateInteger [] "2 3"
-- Nothing
evaluateInteger :: [(String, Integer)] -> String -> Maybe Integer
evaluateInteger = evaluate @_ @IntOp

-- | Parses given expression in Reverse Polish Notation and evaluates it
-- using given association list of variable values
--
-- Returns 'Nothing' in case the expression could not be parsed
-- or in case appropriate variable value is missing.
--
-- The 'forall a op.' part is required to define generic type
-- of intermediate 'Expr' expression that uses scoped type variables 'a' and 'op'.
evaluate :: forall a op. (Eval a op, Parse a, Parse op) => [(String, a)] -> String -> Maybe a
evaluate m s = case parse s of
  Just e -> evalExpr m (e :: Expr a op)
  Nothing -> Nothing
