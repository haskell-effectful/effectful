module Main (main) where

import Test.Tasty

import AsyncTests
import ErrorTests
import StateTests

main :: IO ()
main = defaultMain $ testGroup "effectful"
  [ asyncTests
  , errorTests
  , stateTests
  ]
