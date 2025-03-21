module Main (main) where

import Build_doctests (flags, pkgs, module_sources)
import Test.DocTest (doctest)

main :: IO ()
main = doctest ("-XGHC2024" : flags ++ pkgs ++ module_sources)
