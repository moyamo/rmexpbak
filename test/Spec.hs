-- We have orphan BakName instances to make testing easier
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple
import qualified Dhall
import Lib
import RIO
import RIO.Directory as Directory
import RIO.FilePath ((</>))
import System.Environment as Environment
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()

instance Arbitrary BakName where
  arbitrary = BakName <$> arbitrary

instance IsString BakName where
  fromString = BakName

main :: IO ()
main = hspec $ do
  describe "serializeDhall" $ do
    let serializeThenParse :: (Dhall.FromDhall a, Dhall.ToDhall a) => a -> IO a
        serializeThenParse =
          Dhall.input Dhall.auto
            . textDisplay
            . serializeDhall
    prop "is right inverse of (Dhall.input . Dhall.auto) " $
      \v -> serializeThenParse (v :: [(String, Int)]) >>= flip shouldBe v

  describe "listBaksInLoft" $ do
    it "returns all files and directories that are not hidden" $ do
      withSystemTempDirectory "rmexpbak_test" $ \tmpDir -> do
        traverse_ (\n -> Directory.createDirectory (tmpDir </> n)) $
          [ "2020-01-03",
            ".hidden_dir"
          ]
        traverse_ (\n -> writeFileUtf8Builder (tmpDir </> n) "") $
          [ "2021-01-06.zip",
            ".rmexpbak_state.dhall",
            "someFile",
            ".rmexpbak.dhall"
          ]
        baks <- listBaksInLoft (LoftDir tmpDir)
        baks `shouldSatisfy` elemsUnique
        Set.fromList baks `shouldBe` Set.fromList ["2020-01-03", "2021-01-06.zip", "someFile"]

  describe "updateBackupNumbers" $ do
    prop "is identity when bns == Map.keys bo" $
      \v -> updateBackupNumbers (Map.keys v) v `shouldBe` v
    it "is empty when empty" $
      updateBackupNumbers [] Map.empty `shouldBe` Map.empty
    it "starts with one" $
      updateBackupNumbers ["2021-01-01"] Map.empty `shouldBe` Map.fromList [("2021-01-01", 1)]
    it "keeps date order" $
      shouldBe
        ( updateBackupNumbers
            ["2021-03-01", "2021-01-01", "2021-01-03"]
            Map.empty
        )
        ( Map.fromList
            [("2021-01-01", 1), ("2021-01-03", 2), ("2021-03-01", 3)]
        )
    it "adds new dates at end" $
      shouldBe
        ( updateBackupNumbers
            ["2021-05-05", "2021-03-01", "2021-01-01"]
            (Map.fromList [("2021-01-01", 1), ("2020-01-03", 2), ("2021-03-01", 3)])
        )
        ( Map.fromList
            [("2021-01-01", 1), ("2020-01-03", 2), ("2021-03-01", 3), ("2021-05-05", 4)]
        )
    it "adds new past dates at end" $
      shouldBe
        ( updateBackupNumbers
            ["2020-05-05", "2021-03-01", "2021-01-01"]
            (Map.fromList [("2021-01-01", 1), ("2020-01-03", 2), ("2021-03-01", 3)])
        )
        ( Map.fromList
            [("2021-01-01", 1), ("2020-01-03", 2), ("2021-03-01", 3), ("2020-05-05", 4)]
        )
    prop "return value is a submap of bo" $
      \(v, w) -> v `Map.isSubmapOf` updateBackupNumbers w v `shouldBe` True
    prop "has unique integers" $
      \(v, w) ->
        elemsUnique (v & Map.elems)
          ==> elemsUnique (updateBackupNumbers w v & Map.elems) `shouldBe` True

  describe "numbersToKeep" $ do
    prop "never returns numbers larger than given" $
      \n -> numbersToKeep n `shouldSatisfy` all (<= n)
    it "returns [0] when given 0" $
      numbersToKeep 0 `shouldBe` [0]
    it "returns [0, 4, 6, 7] when given 7" $
      numbersToKeep 7 `shouldBe` [0, 4, 6, 7]
    it "returns [0, 8, 12, 14, 16] when given 16" $
      numbersToKeep 16 `shouldBe` [0, 8, 12, 14, 16]
    it "returns [0, 8, 12, 16, 17] when given 17" $
      numbersToKeep 17 `shouldBe` [0, 8, 12, 16, 17]
    it "returns [0, 2] when given 2" $
      numbersToKeep 2 `shouldBe` [0, 2]
    prop "always returns n as last value" $
      \n -> (numbersToKeep n & List.last) `shouldBe` n
    prop "always returns 0 as the first value" $
      \n -> (numbersToKeep n & List.head) `shouldBe` 0

  describe "baksToDelete" $ do
    prop "never deletes the latest backup" $ do
      \v ->
        (v & Map.elems & elemsUnique) -- Integers are unique
          ==> baksToDelete v `shouldSatisfy` ((v & Map.toList & fmap Tuple.swap & List.maximum & snd) `notElem`)
    prop "never deletes the 0th backup" $ do
      \v ->
        (v & Map.elems & elemsUnique) -- Integers are unique
          ==> baksToDelete v `shouldSatisfy` ((v & Map.toList & fmap Tuple.swap & Map.fromList & Map.lookup 0 & fromMaybe "trivial case") `notElem`)
    it "deletes for adding 3 correctly" $
      baksToDelete (Map.fromList [("2020-01-03", 2), ("2021-03-01", 3)]) `shouldBe` []
    it "deletes for adding 4 correctly" $
      baksToDelete (Map.fromList [("2020-01-03", 2), ("2021-03-01", 3), ("2022-01-01", 4)]) `shouldBe` ["2021-03-01"]

  describe "rmexpbakMain" $ do
    let withTestDir go = withSystemTempDirectory "rmexpbak_test" $ \tmpDir -> do
          traverse_ (\n -> Directory.createDirectory (tmpDir </> n)) $
            [ "2",
              "4",
              "5",
              "6",
              "7",
              ".hidden_dir"
            ]
          traverse_ (\n -> writeFileUtf8Builder (tmpDir </> n) "") $
            [ "1.zip",
              "3.zip",
              "8.zip",
              "9.zip"
            ]
          writeFileUtf8Builder (tmpDir </> ".rmexpbak.dhall") "{ rmCmd = [\"rm\",\"-r\"] }"
          go tmpDir
    it "doesn't delete anything when --delete not specified" $ do
      withTestDir $ \tmpDir -> do
        Environment.withArgs [tmpDir] $ do
          rmexpbakMain
          remainingBaks <- Directory.listDirectory tmpDir
          remainingBaks `shouldSatisfy` elemsUnique
          Set.fromList remainingBaks
            `shouldBe` Set.fromList
              [ ".rmexpbak.dhall",
                ".rmexpbak_state.dhall",
                ".hidden_dir",
                "1.zip",
                "2",
                "3.zip",
                "4",
                "5",
                "6",
                "7",
                "8.zip",
                "9.zip"
              ]
    it "deletes an exponential number of backups when --delete is specified" $ do
      withTestDir $ \tmpDir -> do
        Environment.withArgs [tmpDir, "--delete"] $ do
          rmexpbakMain
          remainingBaks <- Directory.listDirectory tmpDir
          remainingBaks `shouldSatisfy` elemsUnique
          Set.fromList remainingBaks
            `shouldBe` Set.fromList
              [ ".rmexpbak.dhall",
                ".rmexpbak_state.dhall",
                ".hidden_dir",
                "4",
                "8.zip",
                "9.zip"
              ]
    it "remembers the numbering when run consecutively" $ do
      withTestDir $ \tmpDir -> do
        Environment.withArgs [tmpDir, "--delete"] $ do
          rmexpbakMain
          remainingBaks <- Directory.listDirectory tmpDir
          remainingBaks `shouldSatisfy` elemsUnique
          Set.fromList remainingBaks
            `shouldBe` Set.fromList
              [ ".rmexpbak.dhall",
                ".rmexpbak_state.dhall",
                ".hidden_dir",
                "4",
                "8.zip",
                "9.zip"
              ]
          Directory.createDirectory (tmpDir </> "0.d")
          rmexpbakMain
          remainingBaks2 <- Directory.listDirectory tmpDir
          remainingBaks2 `shouldSatisfy` elemsUnique
          Set.fromList remainingBaks2
            `shouldBe` Set.fromList
              [ ".rmexpbak.dhall",
                ".rmexpbak_state.dhall",
                ".hidden_dir",
                "4",
                "8.zip",
                "0.d"
              ]

elemsUnique :: Ord a => [a] -> Bool
elemsUnique l = (l & length) == (l & Set.fromList & Set.size)
