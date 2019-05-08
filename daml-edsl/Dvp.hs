{-# LANGUAGE OverloadedStrings #-}
module Dvp where

import qualified Data.Set as Set

import DAML
import Cash
import Proposable
import Transferrable

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
    preconsume dvpId $ \dvp -> do
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

    assertArchived aliceAssetId
    assertActiveWith bobAssetId (== aliceAsset {assetOwner = "Bob"})
    assertArchived bobCash100Id
    assertActiveWith aliceCash100Id (== bobCash100 {cashOwner = "Alice"})

    pure ()
