{-# LANGUAGE QuasiQuotes #-}

module StdlibTest where

import Atom
import Control.Monad
import Errors
import Eval
import LParser
import Test.Hspec
import Text.RawString.QQ

stdlibTests = do
  let eval = evaluate <=< tokenize
   in describe "stdlib" $ do
        describe "core#and" $ do
          it "should and values" $ do
            eval "(and T T)" `shouldBe` Right (AtomBool True)
            eval "(and T F)" `shouldBe` Right (AtomBool False)
            eval "(and F T)" `shouldBe` Right (AtomBool False)
            eval "(and F F)" `shouldBe` Right (AtomBool False)
        describe "core#or" $ do
          it "should and values" $ do
            eval "(or T T)" `shouldBe` Right (AtomBool True)
            eval "(or T F)" `shouldBe` Right (AtomBool True)
            eval "(or F T)" `shouldBe` Right (AtomBool True)
            eval "(or F F)" `shouldBe` Right (AtomBool False)
        describe "core#not" $ do
          it "should return F for T and T for F and T for Nil" $ do
            eval "(not F)" `shouldBe` Right (AtomBool True)
            eval "(not T)" `shouldBe` Right (AtomBool False)
            eval "(not Nil)" `shouldBe` Right (AtomBool True)
          it "should work for expressions" $ do
            eval "(not (= 5 2))" `shouldBe` Right (AtomBool True)
          it "should be false for other atoms" $ do
            eval "(not 50)" `shouldBe` Right (AtomBool False)
            eval "(not 0)" `shouldBe` Right (AtomBool False)
            eval "(not \"hellow\")" `shouldBe` Right (AtomBool False)

        describe "list#null?" $ do
          it "should return false for list with any number of items" $ do
            eval "(null? (quote (1 2 3 4 5)))" `shouldBe` Right (AtomBool False)
          it "should return true for no items" $ do
            eval "(null? Nil)" `shouldBe` Right (AtomBool True)
            eval "(null? (quote ()))" `shouldBe` Right (AtomBool True)

--
