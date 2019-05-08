{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
module FunDeps where

import DAML

class GenericTemplate g where
    genSignatories :: g -> Set Party

class Monomorphize g t | g -> t, t -> g where
    pack :: g -> t
    unpack :: t -> g

genCreate :: (Monomorphize g t, Template t) => g -> Update (ContractId t)
genCreate = create . pack

class GenericChoice g c r | g c -> r where
    genControllers :: g -> c -> Set Party
    genAction :: (Monomorphize g t, Template t) => ContractId t -> g -> c -> Update r


-- A pizza generic in the possible sizes of slices.
data Pizza a = Pizza
    { pizzaMakers :: Set Party
    , pizzaEaters :: Set Party
    , pizzaSlices :: a
    }
    deriving (Eq, Show, Typeable)

instance (Num a, Ord a, Show a, Typeable a) => GenericTemplate (Pizza a) where
    genSignatories Pizza{..} = pizzaMakers \/ pizzaEaters

data Eat a = Eat
    { eatSlices :: a
    }
    deriving (Show)

instance (Num a, Ord a, Show a, Typeable a, Monomorphize (Pizza a) t, Template t) => GenericChoice (Pizza a) (Eat a) (Maybe (ContractId t)) where
    genControllers Pizza{..} Eat{..} = pizzaEaters
    genAction cid Pizza{..} Eat{..} = do
        assert "Eating at least something" $ eatSlices > 0
        let newPizzaSlices = pizzaSlices - eatSlices
        assert "Eating no more than available" $ newPizzaSlices >= 0
        archive cid
        if newPizzaSlices > 0
            then Just <$> genCreate Pizza{pizzaSlices = newPizzaSlices, ..}
            else pure Nothing

-- A pizza with integer sized slices.
newtype Pizza_Int = Pizza_Int{unPizza_Int :: Pizza Int}
    deriving (Eq, Show, Typeable)

instance Monomorphize (Pizza Int) Pizza_Int where
    pack = Pizza_Int
    unpack = unPizza_Int

instance Template Pizza_Int where
    signatories = genSignatories . unpack

instance Choice Pizza_Int (Eat Int) (Maybe (ContractId Pizza_Int)) where
    controllers = genControllers . unpack
    action cid = genAction cid . unpack


-- A generic option.
data Option t = Option
    { optionAsset :: t
    , optionReceivers :: Set Party
    }
    deriving (Eq, Show, Typeable)

instance GenericTemplate t => GenericTemplate (Option t) where
    genSignatories Option{..} = genSignatories optionAsset

data Call = Call
    deriving (Show)

instance (Monomorphize g t, Template t) => GenericChoice (Option g) Call (ContractId t) where
    genControllers Option{..} Call = optionReceivers
    genAction cid Option{..} Call = do
        archive cid
        genCreate optionAsset

data Kill = Kill
    deriving (Show)

instance GenericChoice (Option g) Kill () where
    genControllers Option{..} Kill = optionReceivers
    genAction cid Option{..} Kill = do
        archive cid

-- An option for `Pizza Int`.
newtype Option_Pizza_Int = Option_Pizza_Int{unOption_Pizza_Int :: Option (Pizza Int)}
    deriving (Eq, Show, Typeable)

instance Monomorphize (Option (Pizza Int)) Option_Pizza_Int where
    pack = Option_Pizza_Int
    unpack = unOption_Pizza_Int

instance Template Option_Pizza_Int where
    signatories = genSignatories . unpack

instance Choice Option_Pizza_Int Call (ContractId Pizza_Int) where
    controllers = genControllers . unpack
    action cid = genAction cid . unpack

instance Choice Option_Pizza_Int Kill () where
    controllers = genControllers . unpack
    action cid = genAction cid . unpack


-- A generic offer.
data Offer t = Offer
    { offerAsset :: t
    , offerReceivers :: Set Party
    }
    deriving (Eq, Show, Typeable)

instance GenericTemplate t => GenericTemplate (Offer t) where
    genSignatories Offer{..} = genSignatories offerAsset \\ offerReceivers

data Accept = Accept
    deriving (Show)

instance (Monomorphize g t, Template t) => GenericChoice (Offer g) Accept (ContractId t) where
    genControllers Offer{..} Accept = offerReceivers
    genAction cid Offer{..} Accept = do
        archive cid
        genCreate offerAsset

data Optionize = Optionize
    deriving (Show)

instance (Monomorphize (Option g) t, Template t) => GenericChoice (Offer g) Optionize (ContractId t) where
    genControllers Offer{..} Optionize = offerReceivers
    genAction cid Offer{..} Optionize = do
        archive cid
        genCreate Option
            { optionAsset = offerAsset
            , optionReceivers = offerReceivers
            }

-- An offer for `Pizza Int`.
newtype Offer_Pizza_Int = Offer_Pizza_Int{unOffer_Pizza_Int :: Offer (Pizza Int)}
    deriving (Eq, Show, Typeable)

instance Monomorphize (Offer (Pizza Int)) Offer_Pizza_Int where
    pack = Offer_Pizza_Int
    unpack = unOffer_Pizza_Int

instance Template Offer_Pizza_Int where
    signatories = genSignatories . unpack

instance Choice Offer_Pizza_Int Accept (ContractId Pizza_Int) where
    controllers = genControllers . unpack
    action cid = genAction cid . unpack

instance Choice Offer_Pizza_Int Optionize (ContractId Option_Pizza_Int) where
    controllers = genControllers . unpack
    action cid = genAction cid . unpack

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
        genCreate Offer
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
