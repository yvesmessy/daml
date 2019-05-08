{-# LANGUAGE MultiParamTypeClasses #-}
module Proposal where

import DAML

data Proposal t = Proposal
    { proposalAsset :: t
    , proposalReceivers :: Set Party
    }
    deriving (Show, Eq, Typeable)

instance Template t => Template (Proposal t) where
    signatories (Proposal x ps) = signatories x \\ ps

data Accept = Accept
    deriving (Show)

instance Template t => Choice (Proposal t) Accept (ContractId t) where
    controllers (Proposal _ ps) Accept = ps
    action cid (Proposal x _) Accept = do
        archive cid
        create x
