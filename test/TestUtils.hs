module TestUtils where
import Test.HUnit

testSuite :: [Assertion] -> Test
testSuite = TestList . map TestCase
