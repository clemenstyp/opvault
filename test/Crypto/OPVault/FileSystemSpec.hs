{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Crypto.OPVault.FileSystemSpec where

import Test.Hspec

import Data.Either (isLeft)

import Crypto.OPVault.Types
import Crypto.OPVault.FileSystem
import Paths_opvault

spec :: Spec
spec = do
  describe "getVault" getVaultSpec

getVaultSpec :: Spec
getVaultSpec = do
  context "when provided a valid vault" $
    it "returns the same vault and a parsed profile" $ do
      vault  <- VaultPath <$> getDataFileName "test-vault/default"
      result <- runResultT $ getVault vault
      result `shouldSatisfy`
        \case Right (_, Profile{pUuid="2B894A18997C4638BACC55F2D56A4890"}) -> True
              _ -> False

  context "when provided a vault that doesn't exist" $
    it "fails as it cannot find any needed files" $ do
      vaultPath <- getDataFileName "garbage_path"
      result <- runResultT $ getVault (VaultPath vaultPath)
      result `shouldSatisfy` isLeft
