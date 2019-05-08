{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module DAML
    ( Update
    , runUpdate
    , create
    , fetch
    , archive
    , Choice (..)
    , exercise
    , assert

    , Scenario
    , runScenario
    , submit
    , submitMany
    , reject
    , assertActiveWith
    , assertArchived

    , Template (..)
    , ContractId
    , Party

    , Set
    , (\/)
    , (\\)

    , Typeable
    ) where

import Control.Monad.Except
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import Data.Typeable
import System.Exit
import qualified Data.Set as Set

import Ledger (Party, Template (..), ContractId, Ledger)
import qualified Ledger

newtype Update a = Update {unUpdate :: RWST (Set.Set Party) () Ledger (Except String) a}
  deriving (Functor, Applicative, Monad, MonadError String, MonadReader (Set.Set Party), MonadState Ledger)

runUpdate :: Update a -> Set.Set Party -> Ledger -> Either String (a, Ledger)
runUpdate tx parties ledger = do
  (res, ledger', ()) <- runExcept (runRWST (unUpdate tx) parties ledger)
  pure (res, ledger')

errorUnknownContractId :: ContractId t -> String
errorUnknownContractId cid = unlines
    [ "Unknown contract id " ++ show cid
    ]

errorMissingAuthorizationCreate :: Template t => t -> Set.Set Party -> String
errorMissingAuthorizationCreate inst parties = unlines
    [ "Missing authorization from " ++ show (Set.toList parties)
    , "when creating contract instance"
    , "  " ++ show inst
    ]

errorMissingAuthorizationArchive :: ContractId t -> Set.Set Party -> String
errorMissingAuthorizationArchive cid parties = unlines
    [ "Missing authorization from " ++ show (Set.toList parties)
    , "when archiving contract id"
    , "  " ++ show cid
    ]

errorMissingAuthorizationExercise :: Show c => ContractId t -> c -> Set.Set Party -> String
errorMissingAuthorizationExercise cid arg parties = unlines
    [ "Missing authorization from " ++ show (Set.toList parties)
    , "when exercising choice on contract id"
    , "  " ++ show cid
    , "with argument"
    , "  " ++ show arg
    ]

create :: Template t => t -> Update (ContractId t)
create inst = do
    authorized <- ask
    let unauthorized = signatories inst Set.\\ authorized
    unless (null unauthorized) $ throwError $ errorMissingAuthorizationCreate inst unauthorized
    ledger <- get
    let (cid, ledger') = Ledger.insert inst ledger
    put ledger'
    pure cid

fetch :: Template t => ContractId t -> Update t
fetch i = do
    l <- get
    case Ledger.lookup i l of
        Nothing -> throwError $ errorUnknownContractId i
        Just x -> pure x

archive :: Template t => ContractId t -> Update ()
archive i = do
    l <- get
    case Ledger.delete i l of
        Nothing -> throwError $ errorUnknownContractId i
        Just (x, l') -> do
            authorized <- ask
            let unauthorized = signatories x Set.\\ authorized
            unless (null unauthorized) $
                throwError $ errorMissingAuthorizationArchive i unauthorized
            put l'

class (Template t, Show c) => Choice t c r | t c -> r where
    controllers :: t -> c -> Set.Set Party
    action :: ContractId t -> t -> c -> Update r

exercise :: Choice t c r => ContractId t -> c -> Update r
exercise cid arg = do
    inst <- fetch cid
    authorized <- ask
    let unauthorized = controllers inst arg Set.\\ authorized
    unless (null unauthorized) $ throwError $ errorMissingAuthorizationExercise cid arg unauthorized
    local (const $ signatories inst `Set.union` controllers inst arg) $
        action cid inst arg

assert :: MonadError String m => String -> Bool -> m ()
assert msg b = if b then pure () else throwError $ "Assertion failed: " ++ msg


newtype Scenario a = Scenario {unScenario :: StateT Ledger (Except String) a}
  deriving (Functor, Applicative, Monad, MonadError String, MonadState Ledger)

runScenario :: Scenario a -> IO ()
runScenario scenario =
  case runExcept (runStateT (unScenario scenario) Ledger.empty) of
    Left err -> do
      putStrLn err
      putStrLn "FAILURE"
      exitFailure
    Right (_, ledger') -> do
      putStrLn $ Ledger.render ledger'
      putStrLn "SUCCESS"

submitMany :: Set.Set Party -> Update a -> Scenario a
submitMany parties tx = do
  ledger <- get
  (res, ledger') <- liftEither $ runUpdate tx parties ledger
  put ledger'
  pure res

submit :: Party -> Update a -> Scenario a
submit = submitMany . Set.singleton

reject :: String -> Party -> Update a -> Scenario ()
reject msg party tx = do
    ledger <- get
    case runUpdate tx (Set.singleton party) ledger of
        Left _ -> pure ()
        Right _ -> throwError ("Update succeeded unexpectedly: " ++ msg)

assertActiveWith :: Template t => ContractId t -> (t -> Bool) -> Scenario ()
assertActiveWith cid prop = do
  ledger <- get
  case Ledger.lookup cid ledger of
    Nothing -> throwError $ errorUnknownContractId cid
    Just inst
      | prop inst -> pure ()
      | otherwise -> throwError $ unlines
          [ "Fact does not have expected property"
          , "  " ++ show cid
          ]

assertArchived :: Template t => ContractId t -> Scenario ()
assertArchived cid = do
  ledger <- get
  case Ledger.lookup cid ledger of
    Nothing -> pure ()
    Just{} -> throwError $ unlines
      [ "Fact is unexpectedly active"
      , "  " ++ show cid
      ]

type Set = Set.Set

(\/) :: Set Party -> Set Party -> Set Party
(\/) = Set.union

(\\) :: Set Party -> Set Party -> Set Party
(\\) = Set.difference
