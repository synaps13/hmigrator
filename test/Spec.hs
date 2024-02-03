{-# LANGUAGE OverloadedStrings #-}

import Data.List (intercalate)
import GHC.IO (bracket)
import Lib (getFileNames, getSqlFiles)
import System.Directory (createDirectory, removeDirectoryRecursive)
import System.FilePath ()
import Test.Hspec (around_, describe, hspec, it, shouldBe)
import Test.QuickCheck
  ( Gen,
    choose,
    generate,
    listOf1,
    vectorOf,
  )
import Test.QuickCheck.Gen (elements)

main :: IO ()
main = hspec $ do
  describe "Lib" $ do
    around_ withTempDirectory $ do
      it "should return a list of file names" $ do
        fileNames <- generate $ vectorOf 5 genFlywayFilename
        mapM_ (\filename -> writeFile ("testdir/" <> filename) "test\n") fileNames

        names <- getFileNames "testdir"

        length names `shouldBe` 5

    around_ withTempDirectory $ do
      it "should return only files that pass all filters [startsWith V/U/R, endsWith .sql]" $ do
        fileNamesSql <- generate $ vectorOf 5 genFlywayFilename
        fileNames <- generate $ vectorOf 5 genRandomFilename
        mapM_ (\filename -> writeFile ("testdir/" <> filename) "test\n") $ fileNames ++ fileNamesSql

        names <- getSqlFiles "testdir"

        length names `shouldBe` 5

withTempDirectory :: IO () -> IO ()
withTempDirectory action = bracket (createDirectory "testdir") (\_ -> removeDirectoryRecursive "testdir") (const action)

-- Generate a valid Flyway migration filename
genFlywayFilename :: Gen String
genFlywayFilename = do
  version <- genVersion
  description <- listOf1 $ elements ['a' .. 'z']
  let filename = "V" ++ version ++ "__" ++ description ++ ".sql"
  return filename
  where
    genVersion = do
      parts <- listOf1 $ choose (1, 9) :: Gen [Int]
      return $ intercalate "." (map show parts)

-- Generate a random filename
genRandomFilename :: Gen String
genRandomFilename = do
    name <- listOf1 $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
    let filename = name ++ ".sql"
    return filename