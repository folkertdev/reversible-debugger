{-# LANGUAGE ScopedTypeVariables, UndecidableInstances, NamedFieldPuns, FlexibleContexts #-}   

module Data.ThreadState where 

import Queue
import qualified Utils
import Data.Map (Map)
import Data.Map as Map
import Types
import Data.Thread as Thread (Thread(..), pid)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import Data.Monoid ((<>))

import Control.Monad.Except as Except

type Threads history a = Map PID (Thread history a)

data OtherThreads history a = 
    OtherThreads
        { active :: Threads history a
        , inactive :: Threads history a
        , blocked :: Threads history a
        , filtered :: Threads history a
        } 


instance (Show h, Show a) => Show (OtherThreads h a) where
    show (OtherThreads active inactive blocked filtered) = 
        "Other Threads:"
            <> "\n"
            <> Utils.showMap "active" active
            <> Utils.showMap "done" inactive
            <> Utils.showMap "blocked" blocked
            <> Utils.showMap "filtered" filtered

data ThreadState history a 
    = Running (Thread history a) (OtherThreads history a) 
    | Stuck (OtherThreads history a)

instance (Show h, Show a) => Show (ThreadState h a) where
    show (Running current other) = 
        "Running:"
            <> "\n"
            <> show current
            <> "\n"
            <> show other

    show (Stuck other) = 
        "Stuck:"
            <> "\n"
            <> show other

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



-- type Interpreter value program = State.StateT (Context value) (Either Error) program




{-| Type to keep track of the progress that a thread makes 

Based on [a monad for deterministic parallelism](https://simonmar.github.io/bib/papers/monad-par.pdf)
-} 
data Progress work 
    = Done 
    | Step work 
    | Blocked work
    deriving (Show, Eq)


scheduleThread :: PID -> ThreadState h a -> Either ThreadScheduleError (ThreadState h a) 
scheduleThread pid threads = 
    case toOther threads of 
        state@OtherThreads{ active, inactive, blocked, filtered } -> 
            let 
                isActive = 
                    Map.lookup pid active
                        |> fmap (\v -> (v, state { active = Map.delete pid active }))

                -- wake up if blocked
                isBlocked = 
                    Map.lookup pid blocked
                        |> fmap (\v -> (v, state { blocked = Map.delete pid blocked }))

                errors :: ThreadScheduleError 
                errors 
                    | Map.member pid inactive = ThreadScheduleError pid ThreadIsFinished
                    | Map.member pid filtered = ThreadScheduleError pid ThreadIsFiltered
                    | otherwise = ThreadScheduleError pid ThreadDoesNotExist
            in 
                case fromMaybe (Left errors) (Right <$> (isActive <|> isBlocked)) of
                    Left e -> 
                        Left e

                    Right ( newActive, rest ) -> 
                        Right $ Running newActive rest


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


rescheduleInternal :: OtherThreads h program -> Either (OtherThreads h program) (Thread h program, OtherThreads h program)
rescheduleInternal state@OtherThreads{active, inactive, blocked, filtered} =     
    case Map.minView active of
        Just (first, rest) -> 
            -- try to make progress on the minimal (most senior) thread 
            Right (first, state { active = rest }) 

        Nothing ->
            if Map.null blocked then
                -- finished all threads, give back the final state
                Left state
            else
                -- try to schedule the blocked threads again
                -- in the how that they are now unblocked
                rescheduleInternal (state { active = blocked, blocked = Map.empty })
        

schedule :: (Monad m, Monoid msg) => (Thread h a -> m (Progress (Thread h a), msg)) -> (Thread h a -> Bool) -> ThreadState h a -> m (ThreadState h a, msg)
schedule step predicate state = 
    case state of 
        Stuck rest -> 
            return (Stuck rest, mempty)

        Running active rest -> do
            (result, message) <- scheduleInternal step predicate active rest
            case result of 
                Left remaining -> 
                    return (Stuck remaining, message)

                Right (t, ts) -> 
                    return (Running t ts, message)





scheduleInternal :: (Monad m, Monoid msg) => (Thread h a -> m (Progress (Thread h a), msg)) -> (Thread h a -> Bool) 
                 -> Thread h a -> OtherThreads h a -> m (Either (OtherThreads h a) (Thread h a, OtherThreads h a) , msg)
scheduleInternal step predicate thread@(Thread pid _ _) state@OtherThreads{active, inactive, blocked, filtered} = 
    if predicate thread then do
        ( result, msg ) <- step thread 
        case result of
            Done ->
                return 
                    ( rescheduleInternal (state { inactive = Map.insert pid thread inactive }) 
                    , msg 
                    )
        

            Step newThread -> 
                return (Right ( newThread, state ) , msg ) 

            Blocked newThread -> 
                return 
                    ( rescheduleInternal (state { blocked = Map.insert pid newThread blocked })
                    , msg 
                    )

    
    else
        return (rescheduleInternal state, mempty)  

data HistoryInspection history = 
    HistoryInspection 
        { spawned :: history -> Maybe PID
        , createdVariable :: history -> Maybe Identifier
            , sent :: history -> Maybe Identifier
            , received :: history -> Maybe Identifier
        }

{-
unschedule :: Monad m 
           => HistoryInspection h  
       -> (Thread h program -> m (Progress (Thread h program)))
       -> (Thread h program -> Bool)
       -> Thread h program 
       -> OtherThreads h program
       -> m (Either (OtherThreads h program) (Thread h program, OtherThreads h program))
unschedule HistoryInspection{spawned} step predicate thread@(Thread pid history program) state@OtherThreads{active, inactive, blocked, filtered} = 
    if predicate thread then 
        case history of
            [] -> 
                return $ rescheduleInternal (state { inactive = Map.insert pid thread inactive }) 

            (mostRecent:rest) -> 
                case spawned mostRecent of
                    Nothing -> do
                        -- reverse the parent thread, keeping the children constant
                        newParent <- step thread 
                        case newParent of
                            Done ->
                                return $ rescheduleInternal (state { inactive = Map.insert pid thread inactive }) 

                            Step newThread -> 
                                return $ Right ( newThread, state ) 

                            Blocked _ -> 
                                error "backwardThread blocked"

                            Branched _ _ -> 
                                error "backwardThread branched"


                    Just spawnedName -> 
                       case Map.lookup spawnedName active of
                            Nothing -> 
                               case Map.lookup spawnedName inactive of
                                   Just (Thread _ _ [childProgram]) -> do
                                       -- child is already completely rolled 
                                       {-
                                       State.modify $ \context -> 
                                            let 
                                                newThreads = Map.adjust (\v -> v - 1 :: Int) pid $ Map.delete spawnedName (_threads context)
                                            in
                                                context { _threads = newThreads } 
                                        -}

                                       return $ Right ( Thread pid rest (spawn childProgram : program), state ) 
                    

                                   other -> 
                                       error $ "child is invalid" ++ show other

                            Just childThread -> 
                                schedule predicate childThread (state { active = Map.insert pid thread active })
                                 

    else
        return $ reschedule state 

{-

data MicroOz

data History 

data Value 

forwardT :: Map Identifier Value -> Thread History MicroOz -> Thread History MicroOz 

forward :: Map Identifier Value -> ThreadState History MicroOz -> ThreadState History MicroOz





backwardT :: Map Identifier Value -> Thread History MicroOz -> (Map Identifier Value, Thread History MicroOz)

backward :: Map Identifier Value -> ThreadState History MicroOz -> (Map Identifier Value, ThreadState History MicroOz)

data FSE = Spawn 

data BSE = RollReceive PID Identifier | RollThread PID


-}
-}
