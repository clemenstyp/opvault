{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Crypto.OPVault.Encryption
    ( derivedKey
    , masterKey
    , overviewKey
    , folderOverview
    , itemKey
    , itemOverview
    , itemDetails
    ) where

import Data.Aeson           (decode)
import Data.ByteArray       (ByteArrayAccess, convert, view)
import Data.ByteString      (drop, take)
import Data.ByteString.Lazy (fromStrict)
import Prelude              hiding (drop, length, take)

import Crypto.Cipher.AES   (AES256)
import Crypto.Cipher.Types (cbcDecrypt, cipherInit, makeIV)
import Crypto.Hash         (Digest, SHA512 (..), hash)
import Crypto.KDF.PBKDF2   (Parameters (..), generate, prfHMAC)

import Crypto.OPVault.Types

realize :: ByteArrayAccess b => b -> ByteString
realize = convert

derivedKey :: Profile -> Password -> DerivedKey
derivedKey Profile{..} (Password pass) =
    let bytes = generate (prfHMAC SHA512)
                         (Parameters pIterations 512)
                         pass
                         (rawBytes pSalt)
     in DerivedKey (take 32 bytes) (take 32 $ drop 32 bytes)

masterKey :: Monad m => Profile -> DerivedKey -> ResultT m MasterKey
masterKey Profile{pMasterKey=mk} DerivedKey{..} = do
    op    <- opdata mk
    bytes <- opDecrypt dKey op
    let hashed = hash bytes :: Digest SHA512
    return $ MasterKey (realize $ view hashed 0  32)
                       (realize $ view hashed 32 32)

overviewKey :: Monad m => Profile -> DerivedKey -> ResultT m OverviewKey
overviewKey Profile{pOverviewKey=ok} DerivedKey{..} = do
    op    <- opdata ok
    bytes <- opDecrypt dKey op
    let hashed = hash bytes :: Digest SHA512
    return $ OverviewKey (realize $ view hashed 0  32)
                         (realize $ view hashed 32 32)

folderOverview :: Monad m => Folder -> OverviewKey -> ResultT m ByteString
folderOverview Folder{..} OverviewKey{..} = opDecrypt oKey =<< opdata fOverview

itemKey :: MonadIO m => Item -> MasterKey -> ResultT m ItemKey
itemKey Item{..} MasterKey{..} = do
    let raw  = rawBytes iEncKey

    let iv   = view raw 0  16
    let dat  = view raw 16 64
    -- TODO: Actually do MAC checking
    -- let mac  = view raw 80 32

    ctx <- liftCrypto $ cipherInit mKey
    iv' <- liftMaybe "Could not create IV" $ makeIV iv
    let bytes = cbcDecrypt (ctx :: AES256) iv' (realize dat)
    return $ ItemKey (realize $ view bytes 0 32) (realize $ view bytes 32 32)

itemOverview :: Monad m => Item -> OverviewKey -> ResultT m Object
itemOverview Item{..} OverviewKey{..} = do
    op  <- opdata iOverview
    raw <- opDecrypt oKey op
    liftMaybe "Could not decode item overview" $ decode (fromStrict raw)

itemDetails :: Monad m => Item -> ItemKey -> ResultT m Object --ItemDetails
itemDetails Item{..} ItemKey{..} =
    liftMaybe "Could not decode encrypted details." . decode . fromStrict =<<
    opDecrypt iKey =<<
    opdata iDetails
