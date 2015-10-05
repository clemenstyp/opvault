{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Crypto.OPVault.EncryptionSpec where

import Data.Either (isRight)
import qualified Data.HashMap.Strict as HM (fromList, lookup, toList)
import qualified Data.Vector as V (length, (!))
import Paths_opvault
import Test.Hspec

import Crypto.OPVault

data TestContext = Ctx
  { profile     :: Profile
  , vault       :: Vault
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
  describe "derivedKey"    derivedKeySpec
  describe "masterKey"     masterKeySpec
  describe "overviewKey"   overviewKeySpec
  describe "itemKey"       itemKeySpec
  describe "itemDetails"   itemDetailsSpec

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

chooseItem :: HashMap Text Item -> ResultT IO Item
chooseItem =
  liftMaybe "Could not find dummy item" .
  HM.lookup "EC0A40400ABB4B16926B7417E95C9669"

itemKeySpec :: Spec
itemKeySpec =
  context "When provided a vault item and a master key" $
    it "returns the decryption key for an item" $ do
      Ctx{..} <- setupTest

      key <- runResultT $ do
        item   <- chooseItem =<< getItems vault
        master <- masterKey profile (derivedKey profile password)
        itemKey item master

      fmap iKey key `shouldBe`
        Right "\r\ESC\tD\145\177\237\244\168-\144Z\183<\173\&3E[\CAN\236\195\226\
              \\DC35\230\ENQ\159?&vP\f"
      fmap iMAC key `shouldBe`
        Right "\147\CAN\233\236\228&\225\231\189\242}\237o6\199\203\206\\1\229\
              \\137s\249|\ENQ\140\203H\149\NAK\194\&4"

itemDetailsSpec :: Spec
itemDetailsSpec =
  context "When provided an item and an item key" $
    it "returns the decrypted item details for an item" $ do
      Ctx{..} <- setupTest

      eitherDetails <- runResultT $ do
        item   <- fmap (snd . (!! 3) . HM.toList) $ getItems vault
        master <- masterKey profile (derivedKey profile password)
        key    <- itemKey item master
        itemDetails item key

      eitherDetails `shouldSatisfy` isRight
      let details = forceEither eitherDetails

      -- TODO: Due to the loose typing situation, this needs some serious cleanup.
      let Just (Array vec) = HM.lookup ("fields" :: Text) details
      V.length vec `shouldBe` 2

      let field1 = vec V.! 0
      let field2 = vec V.! 1

      field1 `shouldBe` Object (HM.fromList [ ("designation", String "username")
                                              , ("value", String "WendyAppleseed")
                                                , ("name", String "username")
                                                  , ("type", String "T")
                                                    ])
      field2 `shouldBe` Object (HM.fromList [ ("designation", String "password")
                                              , ("value", String "reTDx8KHhW8eAc")
                                                , ("name", String "password")
                                                  , ("type", String "T")
                                                    ])
