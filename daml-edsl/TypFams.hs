{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module TypFams where

import DAML hiding (GenContractId)
import qualified DAML

class GenericTemplate g where
    genSignatories :: g -> Set Party

class Template (Monomorphic g) => Monomorphize g where
    type Monomorphic g :: *
    pack :: g -> Monomorphic g
    unpack :: Monomorphic g -> g

type GenContractId a = DAML.ContractId (Monomorphic a)

genCreate :: Monomorphize g => g -> Update (GenContractId g)
genCreate = create . pack

class GenericChoice g c where
    type Result g c :: *
    genControllers :: g -> c -> Set Party
    genAction :: Monomorphize g => GenContractId g -> g -> c -> Update (Result g c)


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

instance (Num a, Ord a, Show a, Typeable a, Monomorphize (Pizza a)) => GenericChoice (Pizza a) (Eat a) where
    type Result (Pizza a) (Eat a) = Maybe (GenContractId (Pizza a))
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

instance Monomorphize (Pizza Int) where
    type Monomorphic (Pizza Int) = Pizza_Int
    pack = Pizza_Int
    unpack = unPizza_Int

instance Template Pizza_Int where
    signatories = genSignatories . unpack @(Pizza Int)

instance Choice Pizza_Int (Eat Int) (Maybe (DAML.ContractId Pizza_Int)) where
    controllers = genControllers . unpack @(Pizza Int)
    action cid = genAction cid . unpack @(Pizza Int)


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

instance Monomorphize g => GenericChoice (Option g) Call where
    type Result (Option g) Call = GenContractId g
    genControllers Option{..} Call = optionReceivers
    genAction cid Option{..} Call = do
        archive cid
        genCreate optionAsset

data Kill = Kill
    deriving (Show)

instance GenericChoice (Option g) Kill where
    type Result (Option g) Kill = ()
    genControllers Option{..} Kill = optionReceivers
    genAction cid Option{..} Kill = do
        archive cid

-- An option for `Pizza Int`.
newtype Option_Pizza_Int = Option_Pizza_Int{unOption_Pizza_Int :: Option (Pizza Int)}
    deriving (Eq, Show, Typeable)

instance Monomorphize (Option (Pizza Int)) where
    type Monomorphic (Option (Pizza Int)) = Option_Pizza_Int
    pack = Option_Pizza_Int
    unpack = unOption_Pizza_Int

instance Template Option_Pizza_Int where
    signatories = genSignatories . unpack @(Option (Pizza Int))

instance Choice Option_Pizza_Int Call (DAML.ContractId Pizza_Int) where
    controllers = genControllers . unpack @(Option (Pizza Int))
    action cid = genAction cid . unpack @(Option (Pizza Int))

instance Choice Option_Pizza_Int Kill () where
    controllers = genControllers . unpack @(Option (Pizza Int))
    action cid = genAction cid . unpack @(Option (Pizza Int))


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

instance Monomorphize g => GenericChoice (Offer g) Accept where
    type Result (Offer g) Accept = GenContractId g
    genControllers Offer{..} Accept = offerReceivers
    genAction cid Offer{..} Accept = do
        archive cid
        genCreate offerAsset

data Optionize = Optionize
    deriving (Show)

instance Monomorphize (Option g) => GenericChoice (Offer g) Optionize where
    type Result (Offer g) Optionize = GenContractId (Option g)
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

instance Monomorphize (Offer (Pizza Int)) where
    type Monomorphic (Offer (Pizza Int)) = Offer_Pizza_Int
    pack = Offer_Pizza_Int
    unpack = unOffer_Pizza_Int

instance Template Offer_Pizza_Int where
    signatories = genSignatories . unpack @(Offer (Pizza Int))

instance Choice Offer_Pizza_Int Accept (DAML.ContractId Pizza_Int) where
    controllers = genControllers . unpack @(Offer (Pizza Int))
    action cid = genAction cid . unpack @(Offer (Pizza Int))

instance Choice Offer_Pizza_Int Optionize (DAML.ContractId Option_Pizza_Int) where
    controllers = genControllers . unpack @(Offer (Pizza Int))
    action cid = genAction cid . unpack @(Offer (Pizza Int))

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
