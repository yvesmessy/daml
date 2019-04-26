{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Monad.Except
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import Data.String
import Data.Typeable
import System.Exit
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

newtype Party = Party {partyName :: String}
  deriving (Eq, Ord, Show, IsString)

class (Eq a, Show a, Typeable a) => Fact a where
  signatories :: a -> Set.Set Party

data SomeFact where
  SomeFact :: Fact a => a -> SomeFact

newtype FactId a = FactId Int
  deriving (Eq, Show)

data Ledger = Ledger
  { facts :: Map.Map Int SomeFact
  , nextFactId :: Int
  }

emptyLedger :: Ledger
emptyLedger = Ledger
  { facts = Map.empty
  , nextFactId = 1
  }

insertFact :: Fact a => a -> Ledger -> (FactId a, Ledger)
insertFact x l =
  let
    n = nextFactId l
    l' = Ledger
      { facts = Map.insert n (SomeFact x) (facts l)
      , nextFactId = n+1
      }
  in
    (FactId n, l')

fetchFact :: Fact a => FactId a -> Ledger -> Maybe a
fetchFact (FactId n) l = do
  SomeFact x <- Map.lookup n (facts l)
  cast x

consumeFact :: Fact a => FactId a -> Ledger -> Maybe (a, Ledger)
consumeFact i@(FactId n) l = do
  x <- fetchFact i l
  pure (x, l{facts = Map.delete n (facts l)})

showLedger :: Ledger -> String
showLedger l =
  unlines [ show n ++ " -> " ++ show x | (n, SomeFact x) <- Map.toList (facts l) ]

errorMissingAuthorization :: Fact a => a -> Set.Set Party -> String
errorMissingAuthorization fact parties = unlines
    [ "Missing authorization from " ++ show (Set.toList parties)
    , "when creating fact"
    , "  " ++ show fact
    ]

errorUnknownFactId :: FactId a -> String
errorUnknownFactId factId = unlines
  [ "Unknown fact id " ++ show factId
  ]

newtype Transaction a = Transaction {unTransaction :: RWST (Set.Set Party) () Ledger (Except String) a}
  deriving (Functor, Applicative, Monad, MonadError String, MonadReader (Set.Set Party), MonadState Ledger)

create :: Fact a => a -> Transaction (FactId a)
create fact = do
  authorizers <- ask
  let unauthorized = signatories fact Set.\\ authorizers
  unless (null unauthorized) $ throwError $ errorMissingAuthorization fact unauthorized
  ledger <- get
  let (factId, ledger') = insertFact fact ledger
  put ledger'
  pure factId

consume :: Fact a => FactId a -> (a -> Transaction b) -> Transaction b
consume i cont = do
  l <- get
  case consumeFact i l of
    Nothing -> throwError $ errorUnknownFactId i
    Just (x, l') -> do
      put l'
      local (`Set.union` signatories x) $ cont x

assert :: MonadError String m => String -> Bool -> m ()
assert msg b = if b then pure () else throwError $ "Assertion failed: " ++ msg

runTransaction :: Transaction a -> Set.Set Party -> Ledger -> Either String (a, Ledger)
runTransaction tx parties ledger = do
  (res, ledger', ()) <- runExcept (runRWST (unTransaction tx) parties ledger)
  pure (res, ledger')


newtype Example a = Example {unExample :: StateT Ledger (Except String) a}
  deriving (Functor, Applicative, Monad, MonadError String, MonadState Ledger)

submit :: Party -> Transaction a -> Example a
submit party tx = do
  ledger <- get
  (res, ledger') <- liftEither $ runTransaction tx (Set.singleton party) ledger
  put ledger'
  pure res

assertActiveWith :: Fact a => FactId a -> (a -> Bool) -> Example ()
assertActiveWith factId prop = do
  ledger <- get
  case fetchFact factId ledger of
    Nothing -> throwError $ errorUnknownFactId factId
    Just fact
      | prop fact -> pure ()
      | otherwise -> throwError $ unlines
          [ "Fact does not have expected property"
          , "  " ++ show factId
          ]

assertConsumed :: Fact a => FactId a -> Example ()
assertConsumed factId = do
  ledger <- get
  case fetchFact factId ledger of
    Nothing -> pure ()
    Just{} -> throwError $ unlines
      [ "Fact is unexpectedly active"
      , "  " ++ show factId
      ]

runExample :: Example a -> IO ()
runExample example =
  case runExcept (runStateT (unExample example) emptyLedger) of
    Left err -> do
      putStrLn err
      putStrLn "FAILURE"
      exitFailure
    Right (_, ledger') -> do
      putStrLn $ showLedger ledger'
      putStrLn "SUCCESS"


data Rule a b = Rule
  { ruleName :: String
  , ruleBody :: a -> Transaction b
  }

-- GENERIC PROPOSAL WORKFLOW

class Fact a => Proposable a where
  proposers :: a -> Set.Set Party

newtype Propose a = Propose {proposeFact :: a}
  deriving (Show, Eq, Typeable)

instance Proposable a => Fact (Propose a) where
  signatories (Propose x) = proposers x

newtype Accept a = Accept {acceptFact :: a}
  deriving (Show, Eq, Typeable)

instance Proposable a => Fact (Accept a) where
  signatories (Accept x) = signatories x Set.\\ proposers x

ruleAcceptProposal :: Proposable a => FactId (Propose a) -> FactId (Accept a) -> Transaction (FactId a)
ruleAcceptProposal propId acptId =
  consume propId $ \(Propose propFact) ->
  consume acptId $ \(Accept acptFact) -> do
    assert "Propose and accept match" $ propFact == acptFact
    create propFact


-- GENERIC TRANSFER WORKFLOW

class Fact a => Transferrable a where
  getOwner :: a -> Party
  setOwner :: Party -> a -> a

data Transfer a = Transfer
  { transferFactId :: FactId a
  , transferOldOwner :: Party
  , transferNewOwner :: Party
  }
  deriving (Show, Eq, Typeable)

instance Transferrable a => Fact (Transfer a) where
  signatories transfer = Set.fromList [transferOldOwner transfer, transferNewOwner transfer]

instance Transferrable a => Proposable (Transfer a) where
  proposers transfer = Set.singleton (transferOldOwner transfer)

ruleTransfer :: Transferrable a => FactId a -> FactId (Transfer a) -> Transaction (FactId a)
ruleTransfer factId transferId =
  consume factId $ \fact ->
  consume transferId $ \transfer -> do
    assert "Fact and transfer match" $
      factId == transferFactId transfer && getOwner fact == transferOldOwner transfer
    create (setOwner (transferNewOwner transfer) fact)


-- GENERIC SPLIT/MERGE WORKFLOW

class Fact a => Quantified a where
  owners :: a -> Set.Set Party
  getQuantity :: a -> Int
  setQuantity :: Int -> a -> a

data Split a = Split
  { splitFactId :: FactId a
  , splitOwners :: Set.Set Party
  , splitQuantity :: Int
  }
  deriving (Show, Eq, Typeable)

instance Quantified a => Fact (Split a) where
  signatories = splitOwners

ruleSplit :: Quantified a => FactId a -> FactId (Split a) -> Transaction (FactId a, FactId a)
ruleSplit factId splitId =
  consume factId $ \fact ->
  consume splitId $ \split -> do
    assert "Fact and split match" $ factId == splitFactId split && owners fact == splitOwners split
    let factQuant = getQuantity fact
    let splitQuant = splitQuantity split
    assert "Split quantity in range" $ 0 < splitQuant && splitQuant < factQuant
    factId1 <- create (setQuantity splitQuant fact)
    factId2 <- create (setQuantity (factQuant - splitQuant) fact)
    pure (factId1, factId2)

data Merge a = Merge
  { mergeFactId1 :: FactId a
  , mergeFactId2 :: FactId a
  , mergeOwners :: Set.Set Party
  }
  deriving (Show, Eq, Typeable)

instance Quantified a => Fact (Merge a) where
  signatories = mergeOwners

ruleMerge :: Quantified a => FactId a -> FactId a -> FactId (Merge a) -> Transaction (FactId a)
ruleMerge factId1 factId2 mergeId =
  consume factId1 $ \fact1 ->
  consume factId2 $ \fact2 ->
  consume mergeId $ \merge -> do
    assert "Fact 1 and fact 2 match" $
      setQuantity 0 fact1 == setQuantity 0 fact2
    assert "Fact 1 and merge match" $
      factId1 == mergeFactId1 merge && owners fact1 == mergeOwners merge
    assert "Fact 2 and merge match" $
      factId2 == mergeFactId2 merge && owners fact2 == mergeOwners merge
    let mergeQuant = getQuantity fact1 + getQuantity fact2
    create (setQuantity mergeQuant fact1)


-- GENERIC DVP WORKFLOW

data Dvp a b = Dvp
  { dvpDeliveryId :: FactId a
  , dvpDeliverer :: Party
  , dvpPaymentId :: FactId b
  , dvpPayer :: Party
  }
  deriving (Eq, Show, Typeable)

instance (Transferrable a, Transferrable b) => Fact (Dvp a b) where
  signatories dvp = Set.fromList [dvpDeliverer dvp, dvpPayer dvp]

instance (Transferrable a, Transferrable b) => Proposable (Dvp a b) where
  proposers dvp = Set.singleton (dvpPayer dvp)

ruleDvpSettle ::
  (Transferrable a, Transferrable b) =>
  FactId (Dvp a b) ->
  Transaction (FactId a, FactId b)
ruleDvpSettle dvpId =
  consume dvpId $ \dvp -> do
    let deliveryId = dvpDeliveryId dvp
    transferDeliveryId <- create Transfer
        { transferFactId = deliveryId
        , transferOldOwner = dvpDeliverer dvp
        , transferNewOwner = dvpPayer dvp
        }
    deliveredId <- ruleTransfer deliveryId transferDeliveryId
    let paymentId = dvpPaymentId dvp
    transferPaymentId <- create Transfer
        { transferFactId = paymentId
        , transferOldOwner = dvpPayer dvp
        , transferNewOwner = dvpDeliverer dvp
        }
    payedId <- ruleTransfer paymentId transferPaymentId
    pure (deliveredId, payedId)
