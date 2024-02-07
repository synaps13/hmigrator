{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Lib
  ( getFileNames,
    getSqlFiles,
    connect,
    runMigrations,
    withConnection,
  )
where

import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BS
import Data.Digest.Murmur32 (hash32)
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import Data.Text (Text, pack, splitOn, takeWhile, tail)
import Hasql.Connection (Connection, Settings, acquire, release)
import Hasql.Session (run, sql)
import System.Directory (doesDirectoryExist, listDirectory, makeAbsolute)
import System.Exit (exitWith, ExitCode (ExitFailure))

data Migration = Migration
  { version :: Text,
    name :: Text,
    hash :: Text,
    file :: FilePath
  }
  deriving (Show)

-- | Run the migrations
runMigrations :: FilePath -> Connection -> IO ()
runMigrations dir connection = do
  files <- getSqlFiles dir
  putStrLn $ "Files that are detected to be migrations:" ++ show files
  let adjustedFileNames = map ((dir <> "/") <>) files
  migrations <- mapM getMigration adjustedFileNames
  putStrLn $ "Migrations to be run: " ++ show migrations
  mapM_ runMigration migrations
  where
    runMigration migration = do
      migrationContent <- BS.readFile $ file migration
      let session = sql (BS.pack "BEGIN; " <> migrationContent <> BS.pack " COMMIT;")
      execResult <- run session connection
      case execResult of
        Left err -> putStrLn ("Migration failed: " ++ show err) >> exitWith (ExitFailure 1)
        Right _ -> putStrLn $ "Migration successful: " ++ file migration

-- | Establish a connection to the database and run the migrmigrations
withConnection :: Settings -> (Connection -> IO a) -> IO a
withConnection settings = bracket (connect settings) release

-- TODO: read variables from ENV
connect :: Settings -> IO Connection
connect settings =
  acquire settings >>= \case
    Left err -> fail $ "connection failed: " ++ show err
    Right con -> putStrLn "Connection successfully established" >> return con

getMigration :: FilePath -> IO Migration
getMigration file = do
  hash <- getHash
  return $ Migration ver description (pack $ show hash) file
  where
    filename = last $ splitOn "/" $ pack file
    splitFilename = case splitOn "__" filename of
      [version, description] -> (version, Data.Text.takeWhile (/= '.') description)
      _InvalidFileName -> ("", "")
    ver =  Data.Text.tail $ fst splitFilename
    description = snd splitFilename
    getHash = hash32 <$> BS.readFile file

-- | Filter out all the files that match naming strategy and will be used for migrations.
getSqlFiles :: FilePath -> IO [FilePath]
getSqlFiles dir = getFileNames dir <&> filter endsWithSqlExtension . filter beginsWithAllowedKeys
  where
    endsWithSqlExtension filename = ".sql" `isSuffixOf` filename
    beginsWithAllowedKeys (f : _) = f `elem` ['V', 'U', 'R']
    beginsWithAllowedKeys [] = False

-- | Get files available in the directory
getFileNames :: FilePath -> IO [FilePath]
getFileNames dir = absolutePath >>= doesDirectoryExist >>= getFileNamesInDirectory
  where
    getFileNamesInDirectory False = return [] :: IO [FilePath]
    getFileNamesInDirectory True = absolutePath >>= listDirectory
    absolutePath = makeAbsolute dir

