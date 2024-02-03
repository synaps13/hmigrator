{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( getFileNames,
    getSqlFiles,
  )
where

import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import System.Directory (doesDirectoryExist, listDirectory)

getSqlFiles :: FilePath -> IO [FilePath]
getSqlFiles dir = getFileNames dir <&> filter endsWithSqlExtension . filter beginsWithAllowedKeys
  where
    endsWithSqlExtension filename = ".sql" `isSuffixOf` filename
    beginsWithAllowedKeys (f : _) = f `elem` ['V', 'U', 'R']
    beginsWithAllowedKeys [] = False

-- | Get files available in the directory
getFileNames :: FilePath -> IO [FilePath]
getFileNames dir = doesDirectoryExist dir >>= getFileNamesInDirectory
  where
    getFileNamesInDirectory False = return [] :: IO [FilePath]
    getFileNamesInDirectory True = listDirectory dir
