module Main where

import Transformers
import qualified Data.Map as Map

exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
exampleExpBad = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Var "x" `Plus` Lit 2))
exampleExpBad2 = Plus (Lit 12) (Abs "x" (Var "x"))

main :: IO ()
main = do
    print $ runEval4 Map.empty 0 (eval4 exampleExp)
