{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall #-}
module Lang.Imp where

import qualified Map.TreeMap as M
import Data.Bits (Bits(xor))

todo :: a
todo = error "TODO"


type Name = String
type State = M.Map Name Integer

emptyState :: State
emptyState = M.empty

data Expr a where
    BNot :: Expr Bool -> Expr Bool
    BAnd :: Expr Bool -> Expr Bool -> Expr Bool
    BOr  :: Expr Bool -> Expr Bool -> Expr Bool

    REq :: Expr Integer -> Expr Integer -> Expr Bool
    RLt :: Expr Integer -> Expr Integer -> Expr Bool

    AConst :: Integer -> Expr Integer
    AVar   :: Name -> Expr Integer
    APlus  :: Expr Integer -> Expr Integer -> Expr Integer
    AMinus :: Expr Integer -> Expr Integer -> Expr Integer
    AMul   :: Expr Integer -> Expr Integer -> Expr Integer
    ADiv   :: Expr Integer -> Expr Integer -> Expr Integer
    AMod   :: Expr Integer -> Expr Integer -> Expr Integer

deriving instance Eq (Expr a) 
deriving instance Show (Expr a)

-- Cmd describes the available commands
data Cmd = 
    CSkip -- No operation, has no effect
  | CAssign Name (Expr Integer) -- Assigns the result of evaluating (`Expr Integer`) to the variable `Name`
  | CIfThEl (Expr Bool) Cmd Cmd -- If then else: evaluates the condition and, depending on the result, executes the first Cmd (then branch) or the second (else branch)
  | CWhile (Expr Bool) Cmd -- While loop
  | CSeq [Cmd] -- Sequence of commands

eval :: Expr a -> State -> a
eval (BNot x) st = not (eval x st)
eval (BAnd x y) st = (eval x st) && (eval y st)
eval (BOr x y) st = (eval x st) || (eval y st)

eval (REq x y) st = (eval x st) == (eval y st)
eval (RLt x y) st = (eval  x st) < (eval y st)

eval (AConst x) _ = x
eval (AVar name) st = case M.lookup name st of
  Just x -> x
  Nothing -> error "Variable not defined"
eval (APlus x y) st = (eval x st) + (eval y st)
eval (AMinus x y) st = (eval x st) - (eval y st)
eval (AMul x y) st = (eval x st) * (eval y st)
eval (ADiv x y) st = div (eval x st) (eval y st)
eval (AMod x y) st = mod (eval x st) (eval y st)

exec :: Cmd -> State -> State
exec CSkip st = st
exec (CAssign name expr) st = M.insert name (eval expr st) st
exec (CIfThEl expr cmd1 cmd2) st | eval expr st = exec cmd1 st
                                 | otherwise    = exec cmd2 st
exec (CWhile expr cmd) st | eval expr st  = exec (CWhile expr cmd) (exec cmd st)
                          | otherwise     = st
exec (CSeq cmds) st = foldl (flip exec) st cmds
