{-# LANGUAGE QuasiQuotes #-}

module CallStackTest where

import Atom
import Control.Monad
import Control.Monad.Trans.Except
import Errors
import Eval
import LParser
import Test.Hspec
import Text.RawString.QQ

listExpression = AtomSymbol . SymbolExpression

tests = do
  let callstack = map (scopeFromPairs 5)
      merge a b = mergeCallStack (callstack a) (callstack b)
   in xdescribe "mergeCallStack" $ do
        it "should merge empty cells" $ do
          merge [[("value", AtomInt 5)]] [] `shouldBe` callstack [[("value", AtomInt 5)]]
          merge [] [[("value", AtomInt 5)]] `shouldBe` callstack [[("value", AtomInt 5)]]
        it "should merge flat call stack" $ do
          merge
            [[("value", AtomInt 5), ("a", AtomInt 1)]]
            [[("value", AtomInt 8), ("b", AtomInt 2)]]
            `shouldBe` callstack [[("value", AtomInt 8), ("a", AtomInt 1), ("b", AtomInt 2)]]
        it "should merge stack one down (closure deeper than caller)" $ do
          merge
            [[("value", AtomNil), ("a", AtomNil)], [("value", AtomInt 5), ("a", AtomNil)]]
            [[("value", AtomInt 8), ("b", AtomNil)]]
            `shouldBe` callstack
              [ [("a", AtomNil), ("value", AtomNil)],
                [("a", AtomNil), ("b", AtomNil), ("value", AtomInt 8)]
              ]
        it "should merge stack one down (caller deeper than closure)" $ do
          merge
            [[("value", AtomInt 8), ("b", AtomNil)]]
            [[("value", AtomNil), ("a", AtomNil)], [("value", AtomInt 5), ("a", AtomNil)]]
            `shouldBe` callstack
              [ [("a", AtomNil), ("value", AtomNil)],
                [("a", AtomNil), ("b", AtomNil), ("value", AtomInt 5)]
              ]

---
---
---
---
---
