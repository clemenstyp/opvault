{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Crypto.OPVault.EncryptionSpec where

import Control.Monad.IO.Class (liftIO)
import Paths_opvault
import Test.Hspec

import Crypto.OPVault

data TestContext = Ctx
  { profile     :: Profile
  , password    :: Password
  , badPassword :: Password
  }

forceEither :: Either a b -> b
forceEither (Right x) = x
forceEither _ = undefined

setupTest :: IO TestContext
setupTest = do
  (vault, profile) <-
    fmap forceEither . runResultT . getVault =<< 
    VaultPath <$> liftIO (getDataFileName "test-vault/default")
  let password = "freddy"
  let badPassword = "notcorrect"
  return Ctx{..}

spec :: Spec
spec = do
  describe "derivedKey"  derivedKeySpec
  describe "masterKey"   masterKeySpec
  describe "overviewKey" overviewKeySpec

derivedKeySpec :: Spec
derivedKeySpec =
  context "when provided **any** profile and password combination" $
    it "returns a (possibly invalid) derived key" $ do
      Ctx{..} <- setupTest
      let key = derivedKey profile password
      dKey key `shouldBe` "c\176u\222\133\137IU\157O\170\157\&4\139\241\v\218\
                          \\160\229g\173\148=x\ETX\242)\FS\147B\170\170"
      dMAC key `shouldBe` "\255:\180&\206U\191\t{%+?-\241\196\186C\DC2\166\150\
                          \\SOH\128\132Mzb[\192\171@\195^"

masterKeySpec :: Spec
masterKeySpec =
  context "when provided a profile and **any** derived key" $
    it "returns a (possibly invalid) master key" $ do
      Ctx{..} <- setupTest
      key     <- runResultT . masterKey profile $
                 derivedKey profile password
      fmap mKey key `shouldBe`
        Right ",ur\251P\249\194\167O\249pN2=\181\DC3{\203\NUL!\241\&4v\189Aky\
               \\GS{\206C\195"
      fmap mMAC key `shouldBe`
        Right "\172\234\218\249\NUL\225\n\216\&1\SOH[\254S\250\251bra:\GS:\
              \\164\235\157\245\205\199\ESC\233\131\176\222"

overviewKeySpec :: Spec
overviewKeySpec =
  context "when provided a profile and **any** derived key" $
    it "returns a (possibly invalid) overview key" $ do
      Ctx{..} <- setupTest
      key     <- runResultT . overviewKey profile $
                 derivedKey profile password
      fmap oKey key `shouldBe`
        Right "B!i\SUB\203\DEL\DC3\219\146\142\159\196\&2\130F\196\DC4\ETB:\242\
              \\168q\157g\DC3p\215\DC3\228\217\vP"
      fmap oMAC key `shouldBe`
        Right "\211 \238)\144\&6\208\151\223\161A\214\SOi\230\167\180\r.s)\141\
              \\175f\158\&5DAH\190>\174"
