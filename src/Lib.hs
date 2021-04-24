{-# LANGUAGE TemplateHaskell #-}

module Lib where

import qualified Data.List.Index as List (indexed)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Tuple (swap)
import Dhall (FromDhall, ToDhall)
import qualified Dhall
import Dhall.Deriving (type (<<<))
import qualified Dhall.Deriving as Dhall
import qualified Dhall.Pretty as Dhall
import Options.Applicative.Simple
  ( ReadM,
    argument,
    empty,
    help,
    long,
    metavar,
    short,
    simpleOptions,
    simpleVersion,
    switch,
  )
import qualified Options.Applicative.Simple as Options.Applicative
import qualified Paths_rmexpbak
import RIO
import qualified RIO.Directory as Directory
import RIO.FilePath ((</>))
import RIO.Lens
import qualified RIO.List as List
import RIO.Process
import qualified RIO.Process as Process
import qualified RIO.Text as Text
import qualified System.IO.Error

-- | 'rmexpbakMain' is the main function called in Main. It is the entry-point
-- of this program.
rmexpbakMain :: IO ()
rmexpbakMain = do
  cwd <- Directory.getCurrentDirectory
  cla <- parseCmdLineArgs cwd
  lo <-
    logOptionsHandle stderr $
      False -- Disable verbose output
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appCmdLineArgs = cla
            }
     in runRIO app run

-- * RIO Boilerplate

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appCmdLineArgs :: !CmdLineArgs
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

-- * Parse the command-line arguments

data CmdLineArgs = CmdLineArgs
  { cmdLineArgsDelete :: Bool,
    cmdLineArgsPath :: LoftDir
  }
  deriving (Show)

class HasCmdLineArgs env where
  cmdLineArgsL :: Lens' env CmdLineArgs

instance HasCmdLineArgs App where
  cmdLineArgsL = lens appCmdLineArgs (\x y -> x {appCmdLineArgs = y})

parseCmdLineArgs :: FilePath -> IO CmdLineArgs
parseCmdLineArgs workingDirectory = do
  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_rmexpbak.version)
      empty -- Text before "Usage:" line
      "Delete an exponential number of backups"
      ( CmdLineArgs
          <$> switch
            ( long "delete"
                <> short 'd'
                <> help "Actually delete backups. Without this flag we just print the files that would be deleted."
            )
            <*> argument (readLoftDir workingDirectory) (metavar "DIR" <> help "The directory containing the backups")
      )
      empty
  return options
  where
    readLoftDir :: FilePath -> ReadM LoftDir
    readLoftDir cwd = do
      path <- Options.Applicative.str
      return $ LoftDir (cwd </> path)

-- * State

-- A Loft is the folder that contains the backups.

-- | Keeps the state of the Loft across invocations of this program.
data LoftState = LoftState
  { -- | If @x@ is the @n@th backup then @@Map.lookup x loftStateBakOrder ==
    -- Just n@@
    loftStateBakOrder :: Map BakName Word
  }
  deriving (Generic)
  deriving
    (ToDhall, FromDhall)
    via Dhall.Codec
          (Dhall.Field (Dhall.CamelCase <<< Dhall.DropPrefix "loftState"))
          LoftState

{- HLINT ignore LoftState "Use newtype instead of data" -}

-- | The absolute path to the directory that contains the backups
newtype LoftDir = LoftDir {unLoftDir :: FilePath}
  deriving (Show, Eq, Ord, ToDhall, FromDhall) via FilePath

-- | The filename or dirname of the backup. eg. The BakName of
-- @\/var\/backups\/2021-01-01.zip@ is just @2021-01-01.zip@
newtype BakName = BakName {unBakName :: FilePath}
  deriving (Eq, Ord, Show, ToDhall, FromDhall) via FilePath

-- | Takes a LoftDir and converts it into a list of BakName ignoring hidden
-- files
listBaksInLoft :: LoftDir -> IO [BakName]
listBaksInLoft ld = do
  baks <- Directory.listDirectory $ unLoftDir ld
  return $ [BakName b | b <- baks, not ("." `List.isPrefixOf` b)]

bakToPath :: LoftDir -> BakName -> FilePath
bakToPath ld bn = unLoftDir ld </> unBakName bn

-- | Updates the bakOrder with the new Baks found in the path
updateBackupNumbersIO :: LoftDir -> Map BakName Word -> IO (Map BakName Word)
updateBackupNumbersIO ld bo = do
  baks <- listBaksInLoft ld
  -- ignore hidden files
  return $ updateBackupNumbers baks bo

updateBackupNumbers :: [BakName] -> Map BakName Word -> Map BakName Word
updateBackupNumbers bns bo =
  Map.union
    bo
    ( newBaks
        & List.sort
        & indexedStarting nextNumber
        & fmap swap
        & Map.fromList
    )
  where
    newBaks = filter (`Map.notMember` bo) bns
    nextNumber = (bo & Map.elems & foldl' max 0) + 1
    indexedStarting n = (each . _1 %~ (+ n) . fromIntegral) . List.indexed

readDhallIfExists :: FromDhall a => FilePath -> RIO env (Maybe a)
readDhallIfExists path =
  catch
    ( do
        utf8 <- readFileUtf8 path
        Just <$> liftIO (Dhall.input Dhall.auto utf8)
    )
    ( \case
        e
          | System.IO.Error.isDoesNotExistError e ->
            return Nothing
          | otherwise -> throwIO e
    )

-- | Save the new LoftState to disk
updateLoftStateRIO :: HasCmdLineArgs env => RIO env LoftState
updateLoftStateRIO = do
  loftDir <- view (cmdLineArgsL . to cmdLineArgsPath)
  let loftStateName = ".rmexpbak_state.dhall"
  let loftStatePath = unLoftDir loftDir </> loftStateName
  loftState <-
    readDhallIfExists loftStatePath >>= \case
      Nothing -> return $ LoftState {loftStateBakOrder = Map.empty}
      Just ls -> return ls
  let bo = loftStateBakOrder loftState
  newBo <- liftIO $ updateBackupNumbersIO loftDir bo
  let newLoftState = loftState {loftStateBakOrder = newBo}
  writeFileUtf8Builder loftStatePath (serializeDhall newLoftState)
  return newLoftState

serializeDhall :: ToDhall a => a -> Utf8Builder
serializeDhall =
  (<> "\n") . displayShow . Dhall.prettyExpr
    . Dhall.embed Dhall.inject

-- | We need to decide what Baks we want to delete. As is this algorithm named
-- we want to delete an exponential number of backups. We do this by keeping the
-- largest number divisible by 2^n for every n.
numbersToKeep :: Word -> [Word]
numbersToKeep lastNumber = go (Just lastNumber) 1 []
  where
    go Nothing _ acc = acc
    go (Just n) pow2 acc =
      let numbers = [n, n - 1 .. 0]
          r =
            take (fromIntegral pow2) numbers
              & filter (`divisible` pow2)
              & List.headMaybe
              -- A sequence of d consecutive numbers must have at least one
              -- number divisible by d
              & Maybe.fromJust
          -- Suppose we didn't update n, if lastNumber is, say, 16 then 16 is
          -- the largest number divisible by 1, 2, 4, 8 and 16, thus the
          -- algorithm would say keep [16]. However if we subtract pow2 from n
          -- then the algorithm says keep [0, 8, 12, 14, 16], which is much
          -- more useful.
          newN
            | n >= pow2 = Just $ n - pow2
            | otherwise = Nothing
       in go newN (2 * pow2) (r : acc)
    divisible n d = n `rem` d == 0

baksToDelete :: Map BakName Word -> [BakName]
baksToDelete bo = bo & Map.filter (`notElem` keep) & Map.keys
  where
    keep = numbersToKeep maxNumber & Set.fromList
    maxNumber = bo & Map.elems & foldl' max 0

-- * Read a configuration file that tells us how to delete the file

data LoftConfig = LoftConfig
  { loftConfigRmCmd :: [Text]
  }
  deriving (Generic)
  deriving
    (FromDhall)
    via Dhall.Codec
          (Dhall.Field (Dhall.CamelCase <<< Dhall.DropPrefix "loftConfig"))
          LoftConfig

{- HLINT ignore LoftConfig "Use newtype instead of data" -}

readLoftConfig :: HasCmdLineArgs env => RIO env LoftConfig
readLoftConfig = do
  loftDir <- view (cmdLineArgsL . to cmdLineArgsPath)
  let loftConfigName = ".rmexpbak.dhall"
  let loftConfigPath = unLoftDir loftDir </> loftConfigName
  readDhallIfExists loftConfigPath >>= \case
    Just lc -> return lc
    Nothing ->
      liftIO $
        fail $
          "Could not find .rmexpbak.dhall configuration file in "
            <> unLoftDir loftDir

deleteBaks :: LoftConfig -> LoftState -> RIO App ()
deleteBaks loftConfig loftState = do
  let toDelete = baksToDelete $ loftStateBakOrder loftState
  let rmCmd = Text.unpack <$> loftConfigRmCmd loftConfig
  shouldDelete <- view (cmdLineArgsL . to cmdLineArgsDelete)
  loftDir <- view (cmdLineArgsL . to cmdLineArgsPath)
  toDelete
    & traverse_
      ( \bak -> do
          let bakPath = loftDir `bakToPath` bak
          exists <- liftIO $ Directory.doesPathExist bakPath
          when exists $ do
            logInfo $ displayShow (rmCmd ++ [bakPath])
            when shouldDelete $ do
              let cmd : args = rmCmd ++ [bakPath]
              Process.proc cmd args Process.runProcess_
      )

-- * Delete an Exponential amount of Backups

run :: RIO App ()
run = do
  loftState <- updateLoftStateRIO
  loftConfig <- readLoftConfig
  willDelete <- view (cmdLineArgsL . to cmdLineArgsDelete)
  unless willDelete $ do
    logInfo "Dry-run. Specify --delete to actually delete files"
  deleteBaks loftConfig loftState
  unless willDelete $ do
    logInfo "Dry-run. Specify --delete to actually delete files"
