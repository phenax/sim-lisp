import LParserTest
import Test.Hspec

main :: IO ()
main = hspec $ do
  valueParsers
  expressionParsers
