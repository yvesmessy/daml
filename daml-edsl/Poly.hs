{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Poly where

import DAML

data Pizza a = Pizza
    { pizzaMakers :: Set Party
    , pizzaEaters :: Set Party
    , pizzaSlices :: a
    }
    deriving (Eq, Show, Typeable)

instance (Num a, Ord a, Show a, Typeable a) => Template (Pizza a) where
    signatories Pizza{..} = pizzaMakers \/ pizzaEaters

data Eat a = Eat
    { eatSlices :: a
    }
    deriving (Show)

instance (Num a, Ord a, Show a, Typeable a) => Choice (Pizza a) (Eat a) (Maybe (ContractId (Pizza a))) where
    controllers Pizza{..} Eat{..} = pizzaEaters
    action cid Pizza{..} Eat{..} = do
        assert "Eating at least something" $ eatSlices > 0
        let newPizzaSlices = pizzaSlices - eatSlices
        assert "Eating no more than available" $ newPizzaSlices >= 0
        archive cid
        if newPizzaSlices > 0
            then Just <$> create Pizza{pizzaSlices = newPizzaSlices, ..}
            else pure Nothing


data Option t = Option
    { optionAsset :: t
    , optionReceivers :: Set Party
    }
    deriving (Eq, Show, Typeable)

instance Template t => Template (Option t) where
    signatories Option{..} = signatories optionAsset

data Call = Call
    deriving (Show)

instance Template t => Choice (Option t) Call (ContractId t) where
    controllers Option{..} Call = optionReceivers
    action cid Option{..} Call = do
        archive cid
        create optionAsset

data Kill = Kill
    deriving (Show)

instance Template t => Choice (Option t) Kill () where
    controllers Option{..} Kill = optionReceivers
    action cid Option{..} Kill = do
        archive cid


data Offer t = Offer
    { offerAsset :: t
    , offerReceivers :: Set Party
    }
    deriving (Eq, Show, Typeable)

instance Template t => Template (Offer t) where
    signatories Offer{..} = signatories offerAsset \\ offerReceivers

data Accept = Accept
    deriving (Show)

instance Template t => Choice (Offer t) Accept (ContractId t) where
    controllers Offer{..} Accept = offerReceivers
    action cid Offer{..} Accept = do
        archive cid
        create offerAsset

data Optionize = Optionize
    deriving (Show)

instance Template t => Choice (Offer t) Optionize (ContractId (Option t)) where
    controllers Offer{..} Optionize = offerReceivers
    action cid Offer{..} Optionize = do
        archive cid
        create Option
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
            , pizzaSlices = 10 :: Int
            }
    offerId <- submit luigi do
        create Offer
            { offerAsset = pizza
            , offerReceivers = [mario]
            }
    optionId <- submit mario do
        exercise offerId Optionize
    pizzaId <- submit mario do
        exercise optionId Call
    mbPizzaId <- submit mario do
        exercise pizzaId Eat{eatSlices = 2 :: Int}

    pure ()
