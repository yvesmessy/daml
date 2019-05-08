{-# LANGUAGE OverloadedStrings #-}
module Referendum where

import qualified Data.Set as Set
import DAML

data Registration = Registration
    { regMember :: Party
    }
    deriving (Eq, Show, Typeable)

instance Fact Registration where
    signatories reg = Set.singleton (regMember reg)

data Electorate = Electorate
    { eleMembers :: Set.Set Party
    }
    deriving (Eq, Show, Typeable)

instance Fact Electorate where
    signatories = eleMembers

ruleConstituteElectorate :: [FactId Registration] -> Transaction (FactId Electorate)
ruleConstituteElectorate regIds = do
    many preconsume regIds $ \regs ->
        create Electorate {eleMembers = Set.fromList (map regMember regs)}

data Initiative = Initiative
    { iniInitiator :: Party
    , iniElectorate :: FactId Electorate
    , iniDescription :: String
    }
    deriving (Eq, Show, Typeable)

instance Fact Initiative where
    signatories ini = Set.singleton (iniInitiator ini)

data Vote = Vote
    { voteVoter :: Party
    , voteInitiative :: FactId Initiative
    }
    deriving (Eq, Show, Typeable)

instance Fact Vote where
    signatories vote = Set.singleton (voteVoter vote)

data Law = Law
    { lawElectorate :: Set.Set Party
    , lawDescription :: String
    }
    deriving (Eq, Show, Typeable)

instance Fact Law where
    signatories = lawElectorate

ruleReferendum :: FactId Initiative -> [FactId Vote] -> Transaction (FactId Law)
ruleReferendum iniId voteIds = do
    ini <- fetch iniId
    archive iniId
    -- withAuthority iniId $ archive iniId
    votes <- mapM fetch voteIds
    mapM_ (\voteId -> withAuthority voteId $ archive voteId) voteIds
    let eleId = iniElectorate ini
    ele <- fetch eleId
    withAuthority eleId $ do
        assert "All votes for initiative" $ all (\vote -> voteInitiative vote == iniId) votes
        let voters = Set.fromList (map voteVoter votes)
        let electorate = eleMembers ele
        assert "All voters in electorate" $ voters `Set.isSubsetOf` electorate
        assert "Votes have majority" $ 2 * Set.size voters > Set.size electorate
        create Law {lawElectorate = electorate, lawDescription = iniDescription ini}

exampleReferendum :: Example ()
exampleReferendum = do
    let electorate = ["Alice", "Bob", "Carl"]
    regIds <- mapM (\member -> submit member $ create Registration {regMember = member}) electorate
    eleId <- submit "Alice" $ ruleConstituteElectorate regIds
    iniId <- submit "Alice" $ create Initiative
        { iniInitiator = "Alice"
        , iniElectorate = eleId
        , iniDescription = "Make it more like wonderland"
        }
    aliceVoteId <- submit "Alice" $ create (Vote "Alice" iniId)
    bobVoteId <- submit "Bob" $ create (Vote "Bob" iniId)
    lawId <- submit "Alice" $ ruleReferendum iniId [aliceVoteId, bobVoteId]

    pure ()
