module Main where
    
import ParserIMP
import LexerIMP
import InterpIMP
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
    s0 <- readState
    com <- readComs
    let code = evalCom s0 com
    let res = Map.toList code
    print res

readComs :: IO Com
readComs = do 
            input <- getContents
            let parsed = parseComs (alexScanTokens input)
            return parsed

readState :: IO MState
readState = do 
            input <- getLine
            let parsed = parseState(alexScanTokens input)
            let state = Map.fromList parsed
            return state
