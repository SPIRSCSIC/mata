import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Mata.Base64

import Set1

tests = Set1.tests

main :: IO ()
main = defaultMainWithOpts Main.tests mempty
       
