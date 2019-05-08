{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Cash where

import Data.Coerce

import DAML
-- import Fungible
import Option
import Proposal
-- import Transferrable

data Cash = Cash
    { cashIssuer :: Party
    , cashOwner :: Party
    , cashAmount :: Int
    }
    deriving (Eq, Show, Typeable)

instance Template Cash where
    signatories cash = [cashIssuer cash, cashOwner cash]

createCash :: Cash -> Example (ContractId Cash)
createCash cash = do
    proposalCid <- submit (cashIssuer cash) do
        create Proposal
            { proposalAsset = cash
            , proposalReceivers = [cashOwner cash]
            }
    submit (cashOwner cash) do
        exercise proposalCid Accept

example :: Example ()
example = do
    let bank = "Bank"
    let alice = "Alice"
    let cash = Cash
            { cashIssuer = bank
            , cashOwner = alice
            , cashAmount = 10
            }
    propCid <- submit bank do
        create Proposal
            { proposalAsset = Option
                { optionAsset = cash
                , optionReceivers = [alice]
                }
            , proposalReceivers = [alice]
            }
    optCid <- submit alice do
        exercise propCid Accept
    reject "Bank cannot archive option" bank do
        archive optCid
    cashCid <- submit alice do
        exercise optCid Call

    pure ()

-- instance Transferrable Cash where
--     getOwner = cashOwner
--     setOwner owner cash = cash{cashOwner = owner}

-- instance Fungible Cash where
--     quantityControllers cash = Set.singleton (cashOwner cash)
--     getQuantity = cashAmount
--     setQuantity amount cash = cash{cashAmount = amount}

-- exampleCashTransfer :: Example ()
-- exampleCashTransfer = do
--     -- The bank issues $10 to Alice
--     let aliceCash10 = Cash
--                 { cashIssuer = "Bank"
--                 , cashOwner = "Alice"
--                 , cashAmount = 10
--                 }
--     aliceCash10Id <- createCash aliceCash10

--     -- Alice splits her $10 into $4 Cash and $6
--     (aliceCash4Id, aliceCash6Id) <- submit "Alice" $ do
--         splitAliceCash10Id <- create Split
--             { splitFactId = aliceCash10Id
--             , splitControllers = Set.singleton "Alice"
--             , splitQuantity = 4
--             }
--         ruleSplit aliceCash10Id splitAliceCash10Id

--     -- Check that Alice' $6 are correct
--     assertActiveWith aliceCash6Id (== aliceCash10 {cashAmount = 6})

--     -- The bank issues $20 to Bob
--     let bobCash20 = Cash
--                 { cashOwner = "Bob"
--                 , cashIssuer = "Bank"
--                 , cashAmount = 20
--                 }
--     bobCash20Id <- createCash bobCash20

--     -- Alice transfers her $4 to Bob
--     let transferAliceCash4 = Transfer
--                 { transferFactId = aliceCash4Id
--                 , transferOldOwner = "Alice"
--                 , transferNewOwner = "Bob"
--                 }
--     acceptTranferAliceCash4Id <- submit "Bob" $
--         create Accept {acceptFact = transferAliceCash4}
--     bobCash4Id <- submit "Alice" $ do
--         proposeTransferAliceCash4Id <- create Propose {proposeFact = transferAliceCash4}
--         transferAliceCash4Id <- ruleAcceptProposal proposeTransferAliceCash4Id acceptTranferAliceCash4Id
--         ruleTransfer aliceCash4Id transferAliceCash4Id

--     -- Bob merges his $20 and $4 into $24
--     bobCash24Id <- submit "Bob" $ do
--         mergeId <- create Merge
--             { mergeFactId1 = bobCash20Id
--             , mergeFactId2 = bobCash4Id
--             , mergeControllers = Set.singleton "Bob"
--             }
--         ruleMerge bobCash20Id bobCash4Id mergeId

--     -- Check that Bob's $24 are correct
--     assertActiveWith bobCash24Id (== bobCash20 {cashAmount = 24})

--     pure ()
