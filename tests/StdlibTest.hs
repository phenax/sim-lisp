{-# LANGUAGE QuasiQuotes #-}

module StdlibTest where

import Atom
import Control.Monad
import Control.Monad.Trans.Except
import Errors
import Eval
import LParser
import Test.Hspec
import Text.RawString.QQ

stdlibTests = do
  let eval = runExceptT . (evaluate <=< (except . tokenize))
   in describe "stdlib" $ do
        describe "core#and" $ do
          it "should and values" $ do
            pending
            eval "(and T T)" `shouldReturn` Right (AtomBool True)
            eval "(and T F)" `shouldReturn` Right (AtomBool False)
            eval "(and F T)" `shouldReturn` Right (AtomBool False)
            eval "(and F F)" `shouldReturn` Right (AtomBool False)
        describe "core#or" $ do
          it "should and values" $ do
            eval "(or T T)" `shouldReturn` Right (AtomBool True)
            eval "(or T F)" `shouldReturn` Right (AtomBool True)
            eval "(or F T)" `shouldReturn` Right (AtomBool True)
            eval "(or F F)" `shouldReturn` Right (AtomBool False)
        describe "core#not" $ do
          it "should return F for T and T for F and T for Nil" $ do
            eval "(not F)" `shouldReturn` Right (AtomBool True)
            eval "(not T)" `shouldReturn` Right (AtomBool False)
            eval "(not Nil)" `shouldReturn` Right (AtomBool True)
          it "should work for expressions" $ do
            eval "(not (= 5 2))" `shouldReturn` Right (AtomBool True)
          it "should be false for other atoms" $ do
            eval "(not 50)" `shouldReturn` Right (AtomBool False)
            eval "(not 0)" `shouldReturn` Right (AtomBool False)
            eval "(not \"hellow\")" `shouldReturn` Right (AtomBool False)

        describe "list#null?" $ do
          it "should return false for list with any number of items" $ do
            eval "(null? (quote (1 2 3 4 5)))" `shouldReturn` Right (AtomBool False)
          it "should return true for no items" $ do
            eval "(null? Nil)" `shouldReturn` Right (AtomBool True)
            eval "(null? (quote ()))" `shouldReturn` Right (AtomBool True)

----
