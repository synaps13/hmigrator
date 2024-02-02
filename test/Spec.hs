{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec ( hspec, describe, it, shouldBe, around_ )
import Test.QuickCheck
    ( generate, vectorOf, Gen )
import System.Directory (createDirectory, removeDirectoryRecursive)
import System.FilePath()

import Lib (getFileNames, getSqlFiles)
import Control.Monad (replicateM)
import Test.QuickCheck.Gen (elements)
import GHC.IO (bracket)

main :: IO ()
main = hspec $ do
  describe "Lib" $ do
    around_ withTempDirectory $ do
      it "should return a list of file names" $ do
        fileNames <- generate $ vectorOf 5 $ arbitraryRandomFilename 5 False
        mapM_ (\filename -> writeFile ("testdir/" <> filename) "test\n") fileNames

        names <- getFileNames "testdir"

        length names `shouldBe` 5

    around_ withTempDirectory $ do
      it "should return only files with .sql extension" $ do
        fileNamesSql <- generate $ vectorOf 5 $ arbitraryRandomFilename 5 True
        fileNames <- generate $ vectorOf 5 $ arbitraryRandomFilename 5 False
        mapM_ (\filename -> writeFile ("testdir/" <> filename) "test\n") $ fileNames ++ fileNamesSql

        names <- getSqlFiles "testdir"

        length names `shouldBe` 5

withTempDirectory :: IO () -> IO ()
withTempDirectory action = bracket (createDirectory "testdir") (\_ -> removeDirectoryRecursive "testdir") (const action)

arbitraryRandomFilename :: Int -> Bool -> Gen String
arbitraryRandomFilename n sqlExt = do
    let validChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['-', '_']
    name <- replicateM n (elements validChars)
    extension <- (if sqlExt then return "sql" else replicateM 3 (elements validChars))
    return (name ++ "." ++ extension)