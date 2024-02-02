{-# LANGUAGE OverloadedStrings #-}

module Lib
  (
    getFileNames,
    getSqlFiles
  ) where

import System.Directory (doesDirectoryExist, listDirectory)
import Data.List (isSuffixOf)
import Data.Functor ((<&>))

getSqlFiles :: FilePath -> IO [FilePath]
getSqlFiles dir = getFileNames dir <&> filter endsWithSqlExtension
  where
    endsWithSqlExtension filename = ".sql" `isSuffixOf` filename


-- | Get files available in the directory
getFileNames :: FilePath -> IO [FilePath]
getFileNames dir = doesDirectoryExist dir >>= getFileNamesInDirectory
  where
    getFileNamesInDirectory False = return [] :: IO [FilePath]
    getFileNamesInDirectory True = listDirectory dir

