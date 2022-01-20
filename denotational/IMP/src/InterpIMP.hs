module InterpIMP where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import ParserIMP
import Data.Typeable


type MState = Map String Int

lookupVariable :: String -> MState -> Int
lookupVariable var s0 = 
    case Map.lookup var s0 of
        Just x -> x
        Nothing -> error $ "Variable " ++ var ++" not found"

assignVariable :: String -> AExp -> MState -> MState
assignVariable v a s0 = Map.insert v a' s0 where a' = evalAExp a s0

fixPoint:: (a -> a) -> a
fixPoint fix = fix (fixPoint fix)

evalAExp :: AExp -> MState -> Int
evalAExp (Num n) s0 = n 
evalAExp (Var s) s0 = lookupVariable s s0
evalAExp (Add exp1 exp2) s0 = evalAExp exp1 s0 + evalAExp exp2 s0
evalAExp (Sub exp1 exp2) s0 = evalAExp exp1 s0 - evalAExp exp2 s0 
evalAExp (Mult exp1 exp2) s0 = evalAExp exp1 s0 * evalAExp exp2 s0 

evalBExp :: BExp -> MState -> Bool
evalBExp (TruthVal False) s0 = False
evalBExp (TruthVal True) s0 = True
evalBExp (Equals exp1 exp2) s0 = evalAExp exp1 s0 == evalAExp exp2 s0
evalBExp (AND exp1 exp2) s0 = evalBExp exp1 s0 && evalBExp exp2 s0
evalBExp (LoE exp1 exp2) s0 = evalAExp exp1 s0 <= evalAExp exp2 s0
evalBExp (NOT exp1) s0 = not (evalBExp exp1 s0)

evalCond :: ((MState -> Bool),(MState -> MState),(MState -> MState)) -> MState -> MState
evalCond (b,g1,g2) s0
        | b s0 =g1 s0
        | otherwise = g2 s0

evalCom :: Com -> MState -> MState
evalCom (Skip) s0 = s0
evalCom (Assign v a) s0 = assignVariable v a s0
evalCom (Seq c1 c2) s0 = evalCom c2 s0' where s0' = evalCom c1 s0
evalCom (IfTE b c1 c2) s0 = evalCond (evalBExp b, evalCom c1, evalCom c2) s0
evalCom (While b c1) s0 = fixPoint f s0
                                 where f g = evalCond ((evalBExp b),( g . evalCom c1), (evalCom Skip))