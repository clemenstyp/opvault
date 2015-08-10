module EncryptionSpec where

import Test.Hspec

spec :: Spec
spec = undefined

derivedKeySpec :: Spec
derivedKeySpec = do
  context "when provided a valid salt (in a profile context) and password" $
    it "returns the vault's derived key" $ do
      undefined
