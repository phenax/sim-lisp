import EvalTest
import LParserTest
import StdlibTest
import Test.Hspec

main :: IO ()
main = hspec $ do
  valueParsers
  expressionParsers
  stdlibTests
  evalExpressionTests
