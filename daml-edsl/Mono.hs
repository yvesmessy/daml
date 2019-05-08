{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Mono where

import DAML

data Pizza = Pizza
    { pizzaMakers :: Set Party
    , pizzaEaters :: Set Party
    }
    deriving (Eq, Show, Typeable)

instance Template Pizza where
    signatories Pizza{..} = pizzaMakers \/ pizzaEaters


data Option_Pizza = Option_Pizza
    { optionAsset :: Pizza
    , optionReceivers :: Set Party
    }
    deriving (Eq, Show, Typeable)

instance Template Option_Pizza where
    signatories Option_Pizza{..} = signatories optionAsset

data Call = Call
    deriving (Show)

instance Choice Option_Pizza Call (ContractId Pizza) where
    controllers Option_Pizza{..} Call = optionReceivers
    action cid Option_Pizza{..} Call = do
        archive cid
        create optionAsset

data Kill = Kill
    deriving (Show)

instance Choice Option_Pizza Kill () where
    controllers Option_Pizza{..} Kill = optionReceivers
    action cid Option_Pizza{..} Kill = do
        archive cid


data Offer_Pizza = Offer_Pizza
    { offerAsset :: Pizza
    , offerReceivers :: Set Party
    }
    deriving (Eq, Show, Typeable)

instance Template Offer_Pizza where
    signatories Offer_Pizza{..} = signatories offerAsset \\ offerReceivers

data Accept = Accept
    deriving (Show)

instance Choice Offer_Pizza Accept (ContractId Pizza) where
    controllers Offer_Pizza{..} Accept = offerReceivers
    action cid Offer_Pizza{..} Accept = do
        archive cid
        create offerAsset

data Optionize = Optionize
    deriving (Show)

instance Choice Offer_Pizza Optionize (ContractId Option_Pizza) where
    controllers Offer_Pizza{..} Optionize = offerReceivers
    action cid Offer_Pizza{..} Optionize = do
        archive cid
        create Option_Pizza
            { optionAsset = offerAsset
            , optionReceivers = offerReceivers
            }

test :: Scenario ()
test = do
    let luigi = "Luigi"
    let mario = "Mario"
    let pizza = Pizza
            { pizzaMakers = [luigi]
            , pizzaEaters = [mario]
            }
    offerId <- submit luigi do
        create Offer_Pizza
            { offerAsset = pizza
            , offerReceivers = [mario]
            }
    optionId <- submit mario do
        exercise offerId Optionize
    pizzaId <- submit mario do
        exercise optionId Call

    pure ()
