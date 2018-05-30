{-# LANGUAGE ScopedTypeVariables, PatternSynonyms #-}
module Synchronization (checkSynchronizedForTransaction, checkSynchronizedForChoice) where

import Control.Monad.State as State
import Control.Monad.Except as Except
import Control.Arrow (first, second)

import Data.Fix

import LocalType (LocalType, Location, Participant, Identifier, Choice(..))
import qualified LocalType
import TypeContext (TypeContext, LocalTypeState, Synchronizable(..), pattern Selected, pattern Offered)
import qualified TypeContext
import Session
import Program
import Zipper


{-| Synchronize the local type for a participant. This is UNSAFE: no checking is done -}
synchronizeLocalType :: Participant -> Session Value ()
synchronizeLocalType participant = do 
    monitor <- getMonitor participant
    let localType = _localType monitor
        newLocalType = 
            case localType of 
                TypeContext.Unsynchronized (history, future) -> 
                    TypeContext.Synchronized (history, future)

                TypeContext.Synchronized {} -> 
                    localType

    putMonitor participant (monitor { _localType = newLocalType })


{-| Type to distinguish between the two synchronizations; used to give correct error messages -}
data SynchronizationType 
    = ForTransaction 
    | ForChoice 


participantNames :: SynchronizationType -> (String, String)
participantNames context = 
    case context of 
        ForTransaction -> ("sender", "receiver") 
        ForChoice -> ("offerer", "selector") 


{-| Check whether two types are already synchronized, and synchronize them if not -}
checkSynchronized :: (TypeContext String -> TypeContext String -> Session Value ()) -> Participant -> Participant -> Session Value ()
checkSynchronized synchronizer partyA partyB = do
    wrappedA <- _localType <$> getMonitor partyA
    wrappedB <- _localType <$> getMonitor partyB
    case ( wrappedA, wrappedB) of 
        (TypeContext.Synchronized {}, TypeContext.Synchronized {}) -> 
            -- already synchronized
            return ()
        _ -> do
            synchronizer (fst $ TypeContext.unwrapState wrappedA) (fst $ TypeContext.unwrapState wrappedB)   
            -- if the above did not error, the types are synchronized; lets put that into the model
            synchronizeLocalType partyA  
            synchronizeLocalType partyB


checkSynchronizedForTransaction :: Participant -> Participant -> Session Value ()
checkSynchronizedForTransaction sender receiver = 
    let 
        synchronizer :: TypeContext String 
                     -> TypeContext String 
                     -> Session Value ()
        synchronizer senderType receiverType =
            case (senderType, receiverType) of 
                (TypeContext.Sent sOwner expectedReceiver sType _, TypeContext.Received rOwner expectedSender rType _) -> do
                    -- owners have to be the expected owners  
                    checkOwners ForTransaction (sOwner, sender) (rOwner, receiver)

                    -- both parties must agree on the sender and receiver
                    checkParties ForTransaction (sender, expectedSender) (receiver, expectedReceiver)

                    -- both parties must send a value of the same type
                    ensure (sType == rType) (SynchronizationError $ "type mismatch: sender sends " ++ sType ++ " but the receiver expects " ++ rType)

                (TypeContext.Sent {}, other) -> 
                    Except.throwError $ SynchronizationError $ "the receiver's previous instruction is not a Receive, but " ++ show other

                (other, TypeContext.Received {}) -> 
                    Except.throwError $ SynchronizationError $ "the sender's previous instruction is not a Send, but " ++ show other
        
                (s, r) -> 
                    Except.throwError $ SynchronizationError $ "neither sender nor receiver can roll a transaction: their types are " ++ show (s, r)
    in 
        checkSynchronized synchronizer sender receiver


checkSynchronizedForChoice :: Participant -> Participant -> Session Value ()
checkSynchronizedForChoice offerer selector = checkSynchronized synchronizer offerer selector
  where
        synchronizer :: TypeContext String
                     -> TypeContext String
                     -> Session Value ()
        synchronizer offererType selectorType =
            case (offererType, selectorType) of 
                (TypeContext.Offered oOwner expectedSelector picked _, TypeContext.Selected sOwner expectedOfferer selection _) -> do
                    -- owners have to be the expected owners
                    checkOwners ForTransaction (oOwner, offerer) (sOwner, selector)

                    -- both parties must agree on the offerer and selector
                    checkParties ForTransaction (expectedOfferer, offerer) (expectedSelector, selector)

                    -- both parties must have the same options
                    let strippedSelection = selection 
                        strippedOptions = picked

                    if strippedOptions == strippedSelection then
                        return ()
                    else 
                        Except.throwError $ SynchronizationError $ 
                            "label/type mismatch: the offerer offers `" 
                                ++ show strippedOptions  
                                ++ "` but the selector implements `" 
                                ++ show strippedSelection ++ "`"


                (other, TypeContext.Selected {}) ->
                    Except.throwError $ SynchronizationError $ "the offerer's previous instruction is not a Offer, but " ++ show other

                (TypeContext.Offered {}, other) -> 
                    Except.throwError $ SynchronizationError $ "the selector's previous instruction is not a Select, but " ++ show other

                (s, r) -> 
                    Except.throwError $ SynchronizationError $ "offerer and selector are out of sync" ++ show (s, r)


checkOwners :: SynchronizationType -> (Participant, Participant) -> (Participant, Participant) -> Session Value ()
checkOwners context (expectedA, actualA) (expectedB, actualB) = do 
    let (a, b) = participantNames context

    ensure (expectedA == actualA) $ 
        SynchronizationError $ "owner error in " ++ a ++ ": " ++ expectedButGot expectedA actualA 

    ensure (expectedB == actualB) $ 
        SynchronizationError $ "owner error in " ++ b ++ ": " ++ expectedButGot expectedB actualB


checkParties :: SynchronizationType -> (Participant, Participant) -> (Participant, Participant) -> Session Value ()
checkParties context (expectedA, actualA) (expectedB, actualB) = do 
    let (a, b) = participantNames context

    ensure (expectedA == actualA) $ 
        SynchronizationError $ a ++ " mismatch: the " ++ b ++ "'s type expects `" ++ expectedA ++ "` but got `" ++ actualA ++ "`"

    ensure (expectedB == actualB) $ 
        SynchronizationError $ b ++ " mismatch: the " ++ a ++ "'s type expects `" ++ expectedB ++ "` but got `" ++ actualB ++ "`"


expectedButGot expected got = 
    "got `" ++ show got ++ "` but expected`" ++ show expected ++ "`"
