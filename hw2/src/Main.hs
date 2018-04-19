{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Tasty (defaultMain, testGroup)
import           TestFile2  (propertyBinTree, specEvalTree,
                             specParseBracketsTree, specParseIntTree,
                             specParseListsTree, specStringSumTree)

main :: IO ()
main = specEvalTree >>= \e ->
       specStringSumTree >>= \s ->
       specParseBracketsTree >>= \b ->
       specParseIntTree >>= \i ->
       specParseListsTree >>= \l ->
       let allTests = testGroup "All blocks" [e, propertyBinTree, s, b, i, l]
       in defaultMain allTests
