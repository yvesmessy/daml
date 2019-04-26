{-# LANGUAGE OverloadedStrings #-}
import Data.Typeable
import qualified Data.Set as Set

import Lib

main :: IO ()
main = do
  putStrLn ""
  putStrLn (replicate 80 '=')
  runExample exampleCashTransfer
  runExample exampleDvp


-- CASH TRANSFER EXAMPLE

data Cash = Cash
  { cashIssuer :: Party
  , cashOwner :: Party
  , cashAmount :: Int
  }
  deriving (Eq, Show, Typeable)

instance Fact Cash where
  signatories cash = Set.fromList [cashIssuer cash, cashOwner cash]

instance Proposable Cash where
  proposers cash = Set.singleton (cashIssuer cash)

instance Transferrable Cash where
  getOwner = cashOwner
  setOwner owner cash = cash{cashOwner = owner}

instance Quantified Cash where
  owners cash = Set.singleton (cashOwner cash)
  getQuantity = cashAmount
  setQuantity amount cash = cash{cashAmount = amount}

createCash :: Cash -> Example (FactId Cash)
createCash cash = do
  proposeCash <- submit (cashIssuer cash) $ create Propose {proposeFact = cash}
  submit (cashOwner cash) $ do
    acceptCash <- create Accept {acceptFact = cash}
    ruleAcceptProposal proposeCash acceptCash

exampleCashTransfer :: Example ()
exampleCashTransfer = do
  -- The bank issues $10 to Alice
  let aliceCash10 = Cash
        { cashIssuer = "Bank"
        , cashOwner = "Alice"
        , cashAmount = 10
        }
  aliceCash10Id <- createCash aliceCash10

  -- Alice splits her $10 into $4 Cash and $6
  (aliceCash4Id, aliceCash6Id) <- submit "Alice" $ do
    splitAliceCash10Id <- create Split
      { splitFactId = aliceCash10Id
      , splitOwners = Set.singleton "Alice"
      , splitQuantity = 4
      }
    ruleSplit aliceCash10Id splitAliceCash10Id

  -- Check that Alice' $6 are correct
  assertActiveWith aliceCash6Id (== aliceCash10 {cashAmount = 6})

  -- The bank issues $20 to Bob
  let bobCash20 = Cash
        { cashOwner = "Bob"
        , cashIssuer = "Bank"
        , cashAmount = 20
        }
  bobCash20Id <- createCash bobCash20

  -- Alice transfers her $4 to Bob
  let transferAliceCash4 = Transfer
        { transferFactId = aliceCash4Id
        , transferOldOwner = "Alice"
        , transferNewOwner = "Bob"
        }
  acceptTranferAliceCash4Id <- submit "Bob" $
    create Accept {acceptFact = transferAliceCash4}
  bobCash4Id <- submit "Alice" $ do
    proposeTransferAliceCash4Id <- create Propose {proposeFact = transferAliceCash4}
    transferAliceCash4Id <- ruleAcceptProposal proposeTransferAliceCash4Id acceptTranferAliceCash4Id
    ruleTransfer aliceCash4Id transferAliceCash4Id

  -- Bob merges his $20 and $4 into $24
  bobCash24Id <- submit "Bob" $ do
    mergeId <- create Merge
      { mergeFactId1 = bobCash20Id
      , mergeFactId2 = bobCash4Id
      , mergeOwners = Set.singleton "Bob"
      }
    ruleMerge bobCash20Id bobCash4Id mergeId

  -- Check that Bob's $24 are correct
  assertActiveWith bobCash24Id (== bobCash20 {cashAmount = 24})

  pure ()



-- DVP EXAMPLE

data Asset = Asset
  { assetOwner :: Party
  , assetDescription :: String
  }
  deriving (Show, Eq, Typeable)

instance Fact Asset where
  signatories asset = Set.singleton (assetOwner asset)

instance Transferrable Asset where
  getOwner = assetOwner
  setOwner owner asset = asset{assetOwner = owner}

exampleDvp :: Example ()
exampleDvp = do
  let aliceAsset = Asset {assetOwner = "Alice", assetDescription = "Painting by Alice"}
  aliceAssetId <- submit "Alice" $
    create aliceAsset
  let bobCash100 = Cash {cashIssuer = "Bank", cashOwner = "Bob", cashAmount = 100}
  bobCash100Id <- createCash bobCash100

  let dvpFact = Dvp
        { dvpDeliveryId = aliceAssetId
        , dvpDeliverer = "Alice"
        , dvpPaymentId = bobCash100Id
        , dvpPayer = "Bob"
        }
  proposeDvpId <- submit "Bob" $
    create Propose {proposeFact = dvpFact}
  (bobAssetId, aliceCash100Id) <- submit "Alice" $ do
    acceptDvpId <- create Accept {acceptFact = dvpFact}
    dvpId <- ruleAcceptProposal proposeDvpId acceptDvpId
    ruleDvpSettle dvpId

  assertConsumed aliceAssetId
  assertActiveWith bobAssetId (== aliceAsset {assetOwner = "Bob"})
  assertConsumed bobCash100Id
  assertActiveWith aliceCash100Id (== bobCash100 {cashOwner = "Alice"})

  pure ()
