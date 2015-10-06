{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Crypto.OPVault.FileSystemSpec where

import Control.Arrow (second)
import Test.Hspec

import qualified Data.HashMap.Strict as HM (size)

import Crypto.OPVault.FileSystem
import Crypto.OPVault.Types
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
      result <- runResultT $ getVault vault

      let expectedProfile = Profile
            { pUuid = "2B894A18997C4638BACC55F2D56A4890"
            , pCreatedAt = 1373753414
            , pUpdatedAt = 1370323483
            , pLastUpdatedBy = "Dropbox"
            , pProfileName = "default"
            , pIterations = 50000
            , pMasterKey = Base64 "opdata01\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL#|&\225;\235#z\133\184\234\204K\221\209\DC1\167\187{\238|\247\US\SOH\157\249&\140\179u\GSV=\ESC\235\240\&3\RS}\239L&\238\185\SOa\210\194\&3\155<-#\206u\233i\242P\161\190\130\&72\130\&6\135\149\v\225\151\"\242\220\146\240.aCR\192\130\208CX\196!\193\221\201\r\a\216\198\201\251F%XF\239\149\SI\DC4T~[r\179*\SOd\207=$dmA\183\253\213u4\161\221\128\141\NAK\232\223\228)\158\247\238\138>\146=\194\132\150PL\172\176\190dzF\NULyz\222l\180\SYN\148\194\235MA\182t\206v-f\233\136\149\253\233\141\218\134+\132r\bt\176\155\b\vP\239\149\DC4\180\234\SO:\EM\245\213\FS\203\136P\205&b>V\218\222\242\188\188bQ\148\221\DLE\DELf:uH\249\145\128\&0u\135N\204O\201\139x[L\213l<\233\188\178<\207p\241\144\143\200Z[\149 \205 \217\210j;\251)\172(\156\DC2b0,\130\246\176\135}Vci\185\143\181Q\251\157\EOTD4\196\203\FSP\220\181\187Z\a\173\ETX\NAK\253\151B\215\208\237\201\185\237h[\250v\151\142\"\143\218\162\&7\218\228\NAK'1"
            , pOverviewKey = Base64 "opdata01@\NUL\NUL\NUL\NUL\NUL\NUL\NUL\140\181\133\156\b\EM\232\139\159\137\139\DC3T|\148L\b;'\189\228a2}\151\SI*\\\151\146F\244 \DC4\223\ts>h<c\SI\184N\150+\147\&3\246\f\"\172J\249\248\171V\245\183s\151)\217\ETX \189\SOH\233\rm\128\176\EOT)\ACK\167 \ACK\EOTT\t#\254Ls\NAK\131^Yy\USe#\190\230\FS\SUBSC\189\ETX\249\183!\190D\166\195xh\DLEj-\237\135'@ \DC3n\139\161\&1r\f;\157\131"
            , pSalt = Base64 "?JN0\195z;\SOp \163\142J\198\146B"
            }
      result `shouldBe` Right (vault, expectedProfile)

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

      let resultSize = HM.size <$> result
      resultSize `shouldBe` Right 29

  context "when provided a vault with no band files" $
    it "returns an empty item map but does not fail" $ do
      Ctx{..} <- setupTest
      result  <- runResultT $ getItems badVault
      let result' = HM.size <$> result
      result' `shouldBe` Right 0
