{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
module Ledger
    ( Party
    , Template (..)
    , ContractId
    , Ledger
    , empty
    , insert
    , lookup
    , delete
    , render
    ) where

import Prelude hiding (lookup)
import Data.String
import Data.Typeable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

newtype Party = Party {partyName :: String}
    deriving (Eq, Ord, Show, IsString)

class (Eq t, Show t, Typeable t) => Template t where
    signatories :: t -> Set.Set Party

data Contract where
    Contract :: Template t => t -> Contract

type role ContractId nominal
newtype ContractId t = ContractId Int
    deriving (Eq, Show)

data Ledger = Ledger
    { facts :: Map.Map Int Contract
    , nextContractId :: Int
    }

empty :: Ledger
empty = Ledger
    { facts = Map.empty
    , nextContractId = 1
    }

allowPolymorphicContracts :: Bool
allowPolymorphicContracts = False

insert :: Template t => t -> Ledger -> (ContractId t, Ledger)
insert x l
    | allowPolymorphicContracts || null (typeRepArgs t) =
        let n = nextContractId l
            l' = Ledger
                { facts = Map.insert n (Contract x) (facts l)
                , nextContractId = n+1
                }
        in  (ContractId n, l')
    | otherwise = error ("Trying to create contract of polymorphic type " ++ show t)
    where
        t = typeOf x

lookup :: Template t => ContractId t -> Ledger -> Maybe t
lookup (ContractId n) l = do
    Contract x <- Map.lookup n (facts l)
    cast x

delete :: Template t => ContractId t -> Ledger -> Maybe (t, Ledger)
delete i@(ContractId n) l = do
    x <- lookup i l
    pure (x, l{facts = Map.delete n (facts l)})

render :: Ledger -> String
render l =
    unlines [ show n ++ " -> " ++ show x | (n, Contract x) <- Map.toList (facts l) ]
