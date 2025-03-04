import Test.Hspec

import QueryableSpec
import ParserSpec
import QuerySpec

main :: IO ()
main = hspec $ do
  QueryableSpec.spec
  ParserSpec.spec
  QuerySpec.spec
  
