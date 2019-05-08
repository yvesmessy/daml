module Transferrable where

import qualified Data.Set as Set

import DAML
import Proposable

class Template t => Transferrable t where
    getOwners :: t -> Set.Set Party
    setOwners :: Set.Set Party -> t -> t

data Transfer a = Transfer
    { transferFactId :: FactId a
    , transferOldOwner :: Party
    , transferNewOwner :: Party
    }
    deriving (Show, Eq, Typeable)

instance Transferrable a => Fact (Transfer a) where
    signatories transfer = Set.fromList [transferOldOwner transfer, transferNewOwner transfer]

instance Transferrable a => Proposable (Transfer a) where
    proposers transfer = Set.singleton (transferOldOwner transfer)

ruleTransfer :: Transferrable a => FactId a -> FactId (Transfer a) -> Transaction (FactId a)
ruleTransfer factId transferId =
    preconsume factId $ \fact ->
    preconsume transferId $ \transfer -> do
        assert "Fact and transfer match" $
            factId == transferFactId transfer && getOwner fact == transferOldOwner transfer
        create (setOwner (transferNewOwner transfer) fact)
