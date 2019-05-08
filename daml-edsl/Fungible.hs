module Fungible where

import qualified Data.Set as Set

import DAML

class Fact a => Fungible a where
    quantityControllers :: a -> Set.Set Party
    getQuantity :: a -> Int
    setQuantity :: Int -> a -> a

data Split a = Split
    { splitFactId :: FactId a
    , splitControllers :: Set.Set Party
    , splitQuantity :: Int
    }
    deriving (Show, Eq, Typeable)

instance Fungible a => Fact (Split a) where
    signatories = splitControllers

ruleSplit :: Fungible a => FactId a -> FactId (Split a) -> Transaction (FactId a, FactId a)
ruleSplit factId splitId =
    preconsume factId $ \fact ->
    preconsume splitId $ \split -> do
        assert "Fact and split match" $ factId == splitFactId split && quantityControllers fact == splitControllers split
        let factQuant = getQuantity fact
        let splitQuant = splitQuantity split
        assert "Split quantity in range" $ 0 < splitQuant && splitQuant < factQuant
        factId1 <- create (setQuantity splitQuant fact)
        factId2 <- create (setQuantity (factQuant - splitQuant) fact)
        pure (factId1, factId2)

data Merge a = Merge
    { mergeFactId1 :: FactId a
    , mergeFactId2 :: FactId a
    , mergeControllers :: Set.Set Party
    }
    deriving (Show, Eq, Typeable)

instance Fungible a => Fact (Merge a) where
    signatories = mergeControllers

ruleMerge :: Fungible a => FactId a -> FactId a -> FactId (Merge a) -> Transaction (FactId a)
ruleMerge factId1 factId2 mergeId =
    preconsume factId1 $ \fact1 ->
    preconsume factId2 $ \fact2 ->
    preconsume mergeId $ \merge -> do
        assert "Fact 1 and fact 2 match" $
            setQuantity 0 fact1 == setQuantity 0 fact2
        assert "Fact 1 and merge match" $
            factId1 == mergeFactId1 merge && quantityControllers fact1 == mergeControllers merge
        assert "Fact 2 and merge match" $
            factId2 == mergeFactId2 merge && quantityControllers fact2 == mergeControllers merge
        let mergeQuant = getQuantity fact1 + getQuantity fact2
        create (setQuantity mergeQuant fact1)
