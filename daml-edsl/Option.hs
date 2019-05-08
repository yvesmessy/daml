{-# LANGUAGE MultiParamTypeClasses #-}
module Option where

import DAML

data Option t = Option
    { optionAsset :: t
    , optionReceivers :: Set Party
    }
    deriving (Eq, Show, Typeable)

instance Template t => Template (Option t) where
    signatories (Option x _) = signatories x

data Call = Call
    deriving (Show)

instance Template t => Choice (Option t) Call (ContractId t) where
    controllers (Option _ ps) Call = ps
    action cid (Option x _) Call = do
        archive cid
        create x

data Kill = Kill
    deriving (Show)

instance Template t => Choice (Option t) Kill () where
    controllers (Option _ ps) Kill = ps
    action cid _ _ = archive cid
