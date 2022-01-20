module InterpIMP where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import ParserIMP
import Data.Typeable

type MState = Map String Int

lookupVariable :: MState -> String -> Int
lookupVariable s0 var = 
    case Map.lookup var s0 of
        Just x -> x
        Nothing -> error $ "Variable " ++ var ++" not found"

assignVariable :: MState -> String -> AExp -> MState
assignVariable s0 v a = Map.insert v a' s0 where a' = evalAExp s0 a

evalAExp :: MState -> AExp -> Int
evalAExp s0 (Num n) = n 
evalAExp s0 (Var s) = lookupVariable s0 s
evalAExp s0 (Add exp1 exp2) = evalAExp s0 exp1 + evalAExp s0 exp2
evalAExp s0 (Sub exp1 exp2) = evalAExp s0 exp1 - evalAExp s0 exp2 
evalAExp s0 (Mult exp1 exp2) = evalAExp s0 exp1 * evalAExp s0 exp2 

evalBExp :: MState -> BExp -> Bool
evalBExp s0 (TruthVal False) = False
evalBExp s0 (TruthVal True) = True
evalBExp s0 (Equals exp1 exp2) = evalAExp s0 exp1 == evalAExp s0 exp2
evalBExp s0 (AND exp1 exp2) = evalBExp s0 exp1 && evalBExp s0 exp2
evalBExp s0 (LoE exp1 exp2) = evalAExp s0 exp1 <= evalAExp s0 exp2
evalBExp s0 (NOT exp1) = not (evalBExp s0 exp1)

evalCom :: MState -> Com -> MState
evalCom s0 (Skip) = s0
evalCom s0 (Assign v a) = assignVariable s0 v a
evalCom s0 (Seq c1 c2) = evalCom s0' c2 where s0' = evalCom s0  c1
evalCom s0 (IfTE b c1 c2) = case evalBExp s0 b of
                                    True -> evalCom s0 c1
                                    False -> evalCom s0 c2
evalCom s0 (While b c1) = case evalBExp s0 b of 
                                    True -> evalCom s0' (While b c1) where s0' = evalCom s0 c1
                                    False -> evalCom s0 Skip
evalCom s0 (Or (While (TruthVal True) c1) c2) = evalCom s0 c2
evalCom s0 (Or c1 c2) = if evalCom s0 c1 == s0 then evalCom s0 c2
                        else evalCom s0 c1