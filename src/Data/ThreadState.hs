{-# LANGUAGE ScopedTypeVariables, UndecidableInstances, NamedFieldPuns, FlexibleContexts #-}   
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Data.ThreadState 
    (OtherThreads
    , ThreadState(..)
    , Threads
    , Progress(..)
    , Data.ThreadState.empty
    , singleton
    , mapOther
    , toOther
    , mapActive
    , add
    , addInactive
    , scheduleThread
    , scheduleThreadBackward
    , reschedule
    , rescheduleBackward
    , addBlocked
    , addUninitialized
    , removeUninitialized 
    , canBeScheduledForward
    , canBeScheduledBackward
    , getThread
    , scheduleParticipant
    )  where 

import qualified Utils

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Types
import Data.Thread as Thread (Thread(..), pid)
import Data.Maybe (fromMaybe)
import Control.Applicative (Alternative, (<|>), empty)
import Data.Monoid ((<>))
import Data.PID as PID (PID, nonsense)
import Data.Actor as Actor (Actor, Participant, currentParticipant)

import Control.Monad.Except as Except

import GHC.Generics
import Elm
import Data.Aeson

type Threads history a = Map PID (Thread history a)

data OtherThreads history a = 
    OtherThreads
        { active :: Threads history a
        , inactive :: Threads history a
        , blocked :: Threads history a
        , filtered :: Threads history a
        , uninitialized :: Threads history a
        } deriving (Generic, ElmType, ToJSON, FromJSON)



empty :: ThreadState h a 
empty = 
    Stuck  
        OtherThreads
            { active = Map.empty   
            , inactive = Map.empty 
            , blocked = Map.empty 
            , filtered = Map.empty 
            , uninitialized = Map.empty 
            } 

singleton :: Thread h a -> ThreadState h a 
singleton = flip add Data.ThreadState.empty 

canBeScheduledForward :: ThreadState h a -> List PID
canBeScheduledForward state = 
    case toOther state of
        OtherThreads{active, blocked, uninitialized} -> 
            Set.toList $ Map.keysSet active <> Map.keysSet blocked <> Map.keysSet uninitialized 


canBeScheduledBackward :: ThreadState h a -> List PID
canBeScheduledBackward state = 
    case toOther state of
        OtherThreads{active, blocked, inactive } -> 
            Set.toList $ Map.keysSet active <> Map.keysSet blocked <> Map.keysSet inactive 


instance (Show h, Show a) => Show (OtherThreads h a) where
    show (OtherThreads active inactive blocked filtered uninitialized) = 
        "Other Threads:"
            <> "\n"
            <> "\n"
            <> Utils.showMap "active" active
            <> Utils.showMap "done  " inactive
            <> Utils.showMap "blocked" blocked
            <> Utils.showMap "filtered" filtered
            <> Utils.showMap "uninitialized" uninitialized

data ThreadState history a 
    = Running (Thread history a) (OtherThreads history a) 
    | Stuck (OtherThreads history a)
    deriving (Generic, ElmType, ToJSON, FromJSON)

instance (Show h, Show a) => Show (ThreadState h a) where
    show (Running current other) = 
        "Running:"
            <> "\n"
            <> "\n"
            <> show current
            <> "\n"
            <> "\n"
            <> show other

    show (Stuck other) = 
        "Stuck:"
            <> "\n"
            <> show other

asum :: (Foldable t, Alternative f) => t (f a) -> f a
asum = foldl (<|>) Control.Applicative.empty 

pidForParticipant :: Participant -> ThreadState h a -> Maybe PID
pidForParticipant participant state = helper (toOther state)
  where helper OtherThreads{ active, inactive , blocked , filtered , uninitialized } = 
          let 
              threads = Map.elems active ++ Map.elems inactive ++ Map.elems blocked ++ Map.elems filtered ++ Map.elems uninitialized 
              predicate Thread{actor, pid } = 
                  case Actor.currentParticipant actor of 
                      Just v | v == participant -> Just pid
                      _ -> Nothing
          in
                asum $ map predicate threads


mapActive :: (Thread h a -> Thread h a) -> ThreadState h a -> ThreadState h a
mapActive tagger state = 
    case state of
        Running active rest -> 
            Running (tagger active) rest

        Stuck rest ->
            Stuck rest

add :: Thread h a -> ThreadState h a -> ThreadState h a
add newThread state = 
    case state of 
        Running current other@OtherThreads{ active } -> 
            Running current (other { active = Map.insert (Thread.pid newThread) newThread active }) 

        Stuck other ->
            Running newThread other

addInactive :: Thread h a -> ThreadState h a -> ThreadState h a
addInactive newThread state = 
    case state of 
        Running current other@OtherThreads{ inactive } -> 
            Running current (other { inactive = Map.insert (Thread.pid newThread) newThread inactive }) 

        Stuck other@OtherThreads{inactive} ->
            Stuck (other { inactive = Map.insert (Thread.pid newThread) newThread inactive }) 


addBlocked :: Thread h a -> ThreadState h a -> ThreadState h a
addBlocked newThread state = 
    case state of 
        Running current other@OtherThreads{ blocked } -> 
            Running current (other { blocked = Map.insert (Thread.pid newThread) newThread blocked }) 

        Stuck other@OtherThreads{blocked} ->
            Stuck (other { blocked = Map.insert (Thread.pid newThread) newThread blocked }) 

addUninitialized :: Thread h a -> ThreadState h a -> ThreadState h a
addUninitialized newThread state = 
    case state of 
        Running current other@OtherThreads{ uninitialized } -> 
            Running current (other { uninitialized = Map.insert (Thread.pid newThread) newThread uninitialized }) 

        Stuck other@OtherThreads{uninitialized} ->
            Stuck (other { uninitialized = Map.insert (Thread.pid newThread) newThread uninitialized }) 


removeUninitialized :: PID  -> ThreadState h a -> ThreadState h a
removeUninitialized pid = 
    mapOther $ \other@OtherThreads{ uninitialized } -> 
        other { uninitialized = Map.delete pid uninitialized } 

toOther :: ThreadState h a -> OtherThreads h a 
toOther state = 
    case state of
        Running current other@OtherThreads{active} -> 
            other { active = Map.insert (Thread.pid current) current active } 

        Stuck other -> 
            other 

mapOther :: (OtherThreads h a -> OtherThreads h a) -> ThreadState h a -> ThreadState h a
mapOther tagger state = 
    case state of 
        Running current other ->
            Running current (tagger other) 

        Stuck other -> 
            Stuck (tagger other) 


getThread :: PID -> ThreadState h a -> Maybe (Thread h a)
getThread pid state = 
    case toOther state of
        OtherThreads { active, inactive , blocked , filtered , uninitialized  } ->
            let asum :: Alternative f => List (f a) -> f a
                asum = foldl (<|>) Control.Applicative.empty 

                threads = [ active, inactive, blocked, filtered, uninitialized ]
            in
                asum $ map (Map.lookup pid) threads




-- type Interpreter value program = State.StateT (Context value) (Either Error) program




{-| Type to keep track of the progress that a thread makes 

Based on [a monad for deterministic parallelism](https://simonmar.github.io/bib/papers/monad-par.pdf)
-} 
data Progress work 
    = Done 
    | Step work 
    deriving (Show, Eq)

scheduleThreadBackward:: PID -> ThreadState h a -> Either ThreadScheduleError (ThreadState h a) 
scheduleThreadBackward pid threads = 
    let work = 
            case toOther threads of 
                state@OtherThreads{ active, inactive, blocked, filtered, uninitialized } -> 
                    let 
                        isActive = 
                            Map.lookup pid active
                                |> fmap (\v -> (v, state { active = Map.delete pid active }))

                        -- wake up if blocked
                        isBlocked = 
                            Map.lookup pid blocked
                                |> fmap (\v -> (v, state { blocked = Map.delete pid blocked }))

                        -- spawn if not initialized 
                        isInactive = 
                            Map.lookup pid inactive
                                |> fmap (\v -> (v, state { inactive = Map.delete pid inactive }))

                        errors :: ThreadScheduleError 
                        errors 
                            | Map.member pid uninitialized = ThreadScheduleError pid ThreadIsUninitialized
                            | Map.member pid filtered = ThreadScheduleError pid ThreadIsFiltered
                            | otherwise = ThreadScheduleError pid ThreadDoesNotExist
                    in 
                        case fromMaybe (Left errors) (Right <$> (isActive <|> isBlocked <|> isInactive)) of
                            Left e -> 
                                Left e

                            Right ( newActive, rest ) -> 
                                Right $ Running newActive rest
    in
        case threads of 
            Running (Thread currentPID _ _ _) _ | currentPID == pid -> 
                    Right threads

            _ -> 
                work 

scheduleParticipant :: Participant -> ThreadState h a -> Either ThreadScheduleError (ThreadState h a) 
scheduleParticipant participant threads =  
    case pidForParticipant participant threads of 
        Nothing -> 
            Left $ ThreadScheduleError PID.nonsense (ParticipantHasNoThread participant)

        Just pid -> 
            scheduleThread pid threads
            

scheduleThread :: PID -> ThreadState h a -> Either ThreadScheduleError (ThreadState h a) 
scheduleThread pid threads = 
    let work = 
            case toOther threads of 
                state@OtherThreads{ active, inactive, blocked, filtered, uninitialized } -> 
                    let 
                        isActive = 
                            Map.lookup pid active
                                |> fmap (\v -> (v, state { active = Map.delete pid active }))

                        -- wake up if blocked
                        isBlocked = 
                            Map.lookup pid blocked
                                |> fmap (\v -> (v, state { blocked = Map.delete pid blocked }))

                        -- spawn if not initialized 
                        isUninitialized = 
                            Map.lookup pid uninitialized
                                |> fmap (\v -> (v, state { uninitialized = Map.delete pid uninitialized }))

                        errors :: ThreadScheduleError 
                        errors 
                            | Map.member pid inactive = ThreadScheduleError pid ThreadIsFinished
                            | Map.member pid filtered = ThreadScheduleError pid ThreadIsFiltered
                            | otherwise = ThreadScheduleError pid ThreadDoesNotExist
                    in 
                        case fromMaybe (Left errors) (Right <$> (isActive <|> isBlocked <|> isUninitialized)) of
                            Left e -> 
                                Left e

                            Right ( newActive, rest ) -> 
                                Right $ Running newActive rest
    in
        case threads of 
            Running (Thread currentPID _ _ _) _ | currentPID == pid -> 
                    Right threads

            _ -> 
                work 


reschedule :: ThreadState h a -> Maybe (ThreadState h a)
reschedule state = 
    case state of 
        Running current rest -> 
            return $ Running current rest

        Stuck rest -> 
            case rescheduleInternal rest of 
                Left _ -> 
                    -- was stuck, is still stuck
                    Nothing

                Right (current, other) -> 
                    Just $ Running current other


rescheduleBackward :: ThreadState h a -> Maybe (ThreadState h a)
rescheduleBackward state = 
    case state of 
        Running current rest -> 
            return $ Running current rest

        Stuck rest -> 
            case rescheduleInternal rest of 
                Left other@OtherThreads{ inactive } -> 
                    -- no active threads to switch to, so bring back an inactive thread
                    case Map.minView inactive of
                        Just ( first, rest ) ->
                            return $ Running first (other { inactive = rest }) 

                        Nothing -> 
                            Nothing 


                Right (current, other) -> 
                    Just $ Running current other

rescheduleInternal :: OtherThreads h program -> Either (OtherThreads h program) (Thread h program, OtherThreads h program)
rescheduleInternal state@OtherThreads{active, inactive, blocked, filtered, uninitialized} =     
    case Map.minView active of
        Just (first, rest) -> 
            -- try to make progress on the minimal (most senior) thread 
            Right (first, state { active = rest }) 

        Nothing ->
            if Map.null blocked then
                -- finished all threads, give back the final state
                if Map.null uninitialized then
                    Left state
                else
                    rescheduleInternal (state { active = uninitialized, uninitialized = Map.empty })
            else
                -- try to schedule the blocked threads again
                -- in the how that they are now unblocked
                rescheduleInternal (state { active = blocked, blocked = Map.empty })
        

