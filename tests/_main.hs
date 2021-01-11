import qualified CallStackTest
import qualified EvalTest
import qualified LParserTest
import qualified StdlibTest
import Test.Hspec

main :: IO ()
main = hspec $ do
  EvalTest.tests
  LParserTest.tests
  StdlibTest.tests
  CallStackTest.tests
