{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Crypto.OPVault.FileSystemSpec where

import Control.Arrow (second)
import qualified Data.HashMap.Strict as HM (size)
import Test.Hspec

import Crypto.OPVault.Types
import Crypto.OPVault.FileSystem
import Paths_opvault

data TestContext = Ctx
  { vaultPath     :: String
  , fakeVaultPath :: String
  , badVaultPath  :: String
  , vault         :: Vault
  , fakeVault     :: Vault
  , badVault      :: Vault
  }

setupTest :: IO TestContext
setupTest = do
  vaultPath     <- getDataFileName "test-vault/default"
  badVaultPath  <- getDataFileName "test-vault/default-bad"
  fakeVaultPath <- getDataFileName "test-vault/not-there"
  let vault     = VaultPath vaultPath
  let badVault  = VaultPath badVaultPath
  let fakeVault = VaultPath fakeVaultPath
  return Ctx{..}

spec :: Spec
spec = do
  describe "getVault" getVaultSpec
  describe "getItems" getItemsSpec

getVaultSpec :: Spec
getVaultSpec = do
  context "when provided a valid vault" $
    it "returns the same vault and a parsed profile" $ do
      Ctx{..} <- setupTest
      result  <- runResultT $ getVault vault
      let result' = second pUuid <$> result
      result' `shouldBe` Right (vault, "2B894A18997C4638BACC55F2D56A4890")

  context "when provided a vault that doesn't exist" $
    it "fails as it cannot find any needed files" $ do
      Ctx{..} <- setupTest
      result  <- runResultT $ getVault fakeVault
      result `shouldBe` Left (concat [ "Unable to read file at "
                                     , fakeVaultPath
                                     , "/profile.js"
                                     ])

  context "when provided a unparseable vault" $
    it "fails as it cannot proceed any further" $ do
      Ctx{..} <- setupTest
      result  <- runResultT $ getVault badVault
      result `shouldBe` Left "Could not sanatize OPVault JSON file"

getItemsSpec :: Spec
getItemsSpec = do
  context "when provided a valid vault" $
    it "returns a map of UUIDs to items" $ do
      Ctx{..} <- setupTest
      result  <- runResultT $ getItems vault
      let result' = HM.size <$> result
      result' `shouldBe` Right 29

  context "when provided a vault with no band files" $
    it "returns an empty item map but does not fail" $ do
      Ctx{..} <- setupTest
      result  <- runResultT $ getItems badVault
      let result' = HM.size <$> result
      result' `shouldBe` Right 0
