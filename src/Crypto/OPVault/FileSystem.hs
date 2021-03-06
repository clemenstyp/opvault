{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Crypto.OPVault.FileSystem
    ( getItems
    , getFolderFile
    , getVault
    ) where

import Prelude hiding (readFile)

import Control.Applicative              ((<|>))
import Control.Concurrent.Async         (Concurrently (..))
import Control.Exception                (IOException, catch)
import Control.Monad                    ((<=<))
import Data.Aeson                       (decode)
import Data.Attoparsec.ByteString.Char8 (Parser, char, endOfInput, many',
                                         many1', parseOnly, satisfy, string)
import Data.ByteString.Lazy             (fromStrict)
import Data.Foldable                    (foldl')
import Data.Maybe                       (catMaybes)

import qualified Data.ByteString.Char8 as B (readFile)
import qualified Data.HashMap.Strict   as HM (empty, union)

import Crypto.OPVault.Types

parseProfile :: Parser ByteString
parseProfile = string "var profile="                *>
               fmap pack (many1' $ satisfy (/=';')) <*
               char ';'

parseOthers :: Parser ByteString
parseOthers = many'  (satisfy (/='('))             *>
              char '('                             *>
              fmap pack (many1' $ satisfy(/=')'))  <*
              string ");"

parseVaultData :: Parser ByteString
parseVaultData = (parseOthers <|> parseProfile) <* endOfInput

fromJSON :: (FromJSON a, Monad m) => ByteString -> ResultT m a
fromJSON bytes =
    case parseOnly parseVaultData bytes of
        Left _      -> failure "Could not sanatize OPVault JSON file"
        Right clean -> liftMaybe "Could not parse OPVault JSON file" .
                       decode $ fromStrict clean

readFile' :: FilePath -> IO (Either String ByteString)
readFile' path = catch
    (Right <$> B.readFile path)
    (\(_::IOException) -> return . Left $ "Unable to read file at " ++ path)

readFile :: MonadIO m => FilePath -> ResultT m ByteString
readFile = liftEither <=< io . readFile'

dropLeft :: Either a b -> Maybe b
dropLeft (Left _)  = Nothing
dropLeft (Right x) = Just x

readFiles' :: [FilePath] -> Concurrently [ByteString]
readFiles' = fmap catMaybes . foldr fn (pure [])
    where fn p acc = (:) <$> Concurrently (dropLeft <$> readFile' p) <*> acc

getVault :: MonadIO m => Vault -> ResultT m (Vault, Profile)
getVault (VaultPath p) = do
    file <- fromJSON =<< readFile (p ++ "/profile.js")
    return (VaultPath p, file)

getFolderFile :: MonadIO m => Vault -> ResultT m FolderMap
getFolderFile (VaultPath p) =
    fromJSON =<< readFile (p ++ "/folders.js")

bandFiles :: Vault -> [FilePath]
bandFiles (VaultPath p) = fmap structure "0123456789ABCDEF"
    where structure x = concat [p, "/band_", [x], ".js"]

getItems :: MonadIO m => Vault -> ResultT m ItemMap
getItems vault =
    let files  = readFiles' $ bandFiles vault
        reader = sequence . fmap fromJSON
        runner = join . io . runConcurrently
    in fmap (foldl' HM.union HM.empty) . runner $ reader <$> files
