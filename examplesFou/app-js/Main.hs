module Main where

import MainW (mainW)
import Language.Javascript.JSaddle (liftJSM)

main :: IO ()
main = liftJSM mainW
