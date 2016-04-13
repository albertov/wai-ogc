module Network.Wai.OgcSpec (main, spec) where

import           Test.Hspec
import           Network.Wai.Ogc ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "compiles" $ do
    True `shouldBe` True
