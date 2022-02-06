import Test.Hspec
import ParseBCG
import Data.Either
import BCG

validBCG = "1 green 2 green 1 0 \n\
           \1 blue 3 blue 1 0 \n\
           \1 red 4 green 0 1 \n\
           \1 red 6 red 1 0 \n\
           \2 red 3 red 1 0 \n\
           \2 blue 5 blue 1 0 \n\
           \3 green 4 green 1 0 \n\
           \3 green 6 red 0 1 \n\
           \4 red 5 red 1 0 \n\
           \4 red 6 green 0 1 \n\
           \4 blue 6 blue 1 0 \n\
           \5 green 6 green 1 0"

invalidComplexNumBCG = "1 green 2 green 1 0 \n\
                       \1 blue 3 blue 1 0 \n\
                       \1 red 4 green 0 e \n\
                       \1 red 6 red 1 0 \n\
                       \2 red 3 red 1 0 \n\
                       \2 blue 5 blue 1 0 \n\
                       \3 green 4 green 1 0 \n\
                       \3 green 6 red 0 1 \n\
                       \4 red 5 red 1 0 \n\
                       \4 red 6 green 0 1 \n\
                       \4 blue 6 blue 1 0 \n\
                       \5 green 6 green 1 0"

invalidTooManyParamBCG = "1 green 2 green 1 0 \n\
                         \1 blue 3 blue 1 0 \n\
                         \1 red 4 green 0 1 \n\
                         \1 red 6 red 1 0 0 \n\
                         \2 red 3 red 1 0 \n\
                         \2 blue 5 blue 1 0 \n\
                         \3 green 4 green 1 0 \n\
                         \3 green 6 red 0 1 \n\
                         \4 red 5 red 1 0 \n\
                         \4 red 6 green 0 1 \n\
                         \4 blue 6 blue 1 0 \n\
                         \5 green 6 green 1 0"

invalidTooFewParamBCG = "1 green 2 green 1 0 \n\
                        \1 blue 3 1 0 \n\
                        \1 red 4 green 0 1 \n\
                        \1 red 6 red 1 0 \n\
                        \2 red 3 red 1 0 \n\
                        \2 blue 5 blue 1 0 \n\
                        \3 green 4 green 1 0 \n\
                        \3 green 6 red 0 1 \n\
                        \4 red 5 red 1 0 \n\
                        \4 red 6 green 0 1 \n\
                        \4 blue 6 blue 1 0 \n\
                        \5 green 6 green 1 0"

main :: IO ()
main = hspec $ do
    describe "parser" $ do
        context "when provided with valid input" $ do
            it "parses Successfully" $ do
                parse validBCG `shouldSatisfy` isRight
        context "when provided with an input with invalid complex number" $ do
            it "returns a ComplexNumberParseFailure exception and the first line the error occurred" $ do
                show (parse invalidComplexNumBCG) `shouldBe` show (Left (ComplexNumberParseFailure, 3) :: Either (ParseException, Int) BCG)
        context "when provided with an input with more parameters than expected" $ do
            it "returns an IncorrectNumberOfParameters exception and the first line the error occurred" $ do
                show (parse invalidTooManyParamBCG) `shouldBe` show (Left (IncorrectNumberOfParameters, 4) :: Either (ParseException, Int) BCG)
        context "when provided with an input with fewer parameters than expected" $ do
            it "returns an IncorrectNumberOfParameters exception and the first line the error occurred" $ do
                show (parse invalidTooFewParamBCG) `shouldBe` show (Left (IncorrectNumberOfParameters, 2) :: Either (ParseException, Int) BCG)

