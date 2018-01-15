{-# LANGUAGE ScopedTypeVariables, NamedFieldPuns, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, DuplicateRecordFields #-}
module Interpreter where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Control.Monad.State as State
import Control.Monad.Except as Except
import Control.Applicative (liftA2)
import qualified Data.Foldable as Foldable

import qualified Queue 
import Data.Thread as Thread
import Data.Context as Context 
import Data.PID as PID (PID, create, parent, child)
import Data.ThreadState (Progress(..) , ThreadState(..) , OtherThreads)
import qualified Data.ThreadState as ThreadState

import Types
import qualified Cmd
import Debug.Trace as Debug

import MicroOz (Program(..), History(CreatedVariable), Value(Receive), ForwardMsg(..), BackwardMsg(..), advanceP, backwardP, renameCreator) 
import qualified MicroOz


{-| The main type keeping the context of the execution

It's a `StateT` that has read and write access to a tuple of two values

* `Context Value` stores variable bindings, channels and handles fresh variable name generation
* `ThreadState History Program` contains the threads 

Additionally, any computation `Either` succeed, or fail producing an `Error`.

Finally, a value of type `a` is produced.
-}
type Execution a = StateT (Context Value, ThreadState History Program) (Either Error) a


rollSends :: Int -> ChannelName -> Execution () 
rollSends n channelName = do
    histories <- liftContext $ Context.withChannel channelName (return . Queue.lastNSends n) 
    handleBackwardEffects RollSend { caller = PID.create [], histories = histories, channelName = channelName }


rollReceives :: Int -> ChannelName -> Execution () 
rollReceives n channelName = do
    histories <- liftContext $ Context.withChannel channelName (return . Queue.lastNReceives n)
    handleBackwardEffects RollReceive { caller = PID.create [], histories = histories, channelName = channelName }



rollThread :: PID -> Execution Program
rollThread pid = 
    let
        proceedOnThreadIsUninitialized :: Error -> Execution Program
        proceedOnThreadIsUninitialized error = 
                    case error of
                        SchedulingError (ThreadScheduleError pid ThreadIsUninitialized) -> do
                            match <- State.gets (ThreadState.getThread pid . extract)
                            maybe undefined finalize match

                        _ -> 
                            Except.throwError error

        finalize :: Thread History Program -> Execution Program 
        finalize Thread{program} = 
            case program of
                [ instruction ] -> 
                    return instruction 

                _ -> 
                    error $ "invalid initial program: " ++ show program

        recurseTillDone = do
            scheduleThreadBackward pid 
            state <- extract <$> State.get
            case state of
                Running current@Thread{program} rest -> do
                    ( progress, cmd ) <- liftContext $ rollback current 

                    let messages = Cmd.unpack cmd 
                        processMessages = Foldable.traverse_ handleBackwardEffects messages


                    case progress of
                        Done -> do
                            -- thread is completely unrolled, try scheduling the parent
                            setThreadState $ ThreadState.addUninitialized current $ Stuck rest
                            processMessages
                            finalize current


                        Step newCurrent -> do
                            setThreadState $ Running newCurrent rest
                            processMessages
                            rollThread pid 

                Stuck _ -> 
                    -- scheduleThread will have errored
                    undefined

    in
        recurseTillDone `Except.catchError` proceedOnThreadIsUninitialized 
                        

rollVariable :: Identifier -> Execution () 
rollVariable identifier = do
    pid <- liftContext $ Context.lookupCreator identifier 
    scheduleThread pid
    withRunning $ \current@Thread{history} rest -> do
        let toUndo = 1 + length (takeWhile (/= CreatedVariable identifier) history)
        rollN pid toUndo 

-- ROLLING HELPERS 


{-| Roll the (assumed) most recent action on a channel -}
rollChannel :: ChannelName -> Queue.QueueHistory -> Execution () 
rollChannel ourChannel history = 
    let pid = 
            case Debug.traceShowId history of 
                Queue.Added v -> v
                Queue.Removed v -> v

        predicate h = 
            case h of 
                MicroOz.Received { channelName } | channelName == ourChannel -> False
                MicroOz.Sent { channelName } | channelName == ourChannel -> False
                _ -> True
    in 
        rollWhile predicate pid


{-| roll the thread with the given PID (at most) n times -}
rollN :: PID -> Int -> Execution ()
rollN pid n =
    if n <= 0 then 
          return () 
    else do
        scheduleThread pid
        withRunning $ \current rest -> do
            ( progress, cmd ) <- liftContext $ rollback current 

            let messages = Cmd.unpack cmd 

            case progress of
                Done -> do
                    setThreadState $ ThreadState.addUninitialized current $ Stuck rest
                    Foldable.traverse_ handleBackwardEffects messages

                Step newCurrent -> do
                    setThreadState $ Running newCurrent rest
                    Foldable.traverse_ handleBackwardEffects messages
                    rollN pid (n - 1)

{-| roll while a predicate holds. 
This function is inclusive and will also roll the first match
-}
rollWhile :: (History -> Bool) -> PID -> Execution ()
rollWhile predicate pid = do
    scheduleThreadBackward pid
    withRunning $ \current@Thread{history} rest -> 
        case history of 
            [] -> return ()
            (h:hs) -> 
                if predicate h then do
                    backward
                    rollWhile predicate pid

                else
                    -- recursion unrolls till match
                    -- so also roll the match
                    backward




skipLets_ :: Execution ()  
skipLets_ = 
    let predicate Thread{program} = 
            case program of 
                (MicroOz.Let {} : _) -> True
                _ -> False
    in
        skipHelper predicate []


skipLets :: Execution ()  
skipLets = 
    let predicate Thread{program} = 
            case program of 
                (MicroOz.Send {} : _) -> False
                (MicroOz.Let  _ Receive {} _ : _) -> False
                _ -> True
    in
        skipHelper predicate []



skipHelper :: (Thread History Program -> Bool) -> List (Thread History Program) -> Execution ()
skipHelper predicate filtered = do
    state <- extract <$> State.get
    case ThreadState.reschedule state of
        Nothing -> 
            -- no thread can be scheduled, we're done
            setThreadState $ foldr ThreadState.add state filtered

        Just (Stuck rest) -> 
            error "reschedule cannot return a stuck"

        Just (Running current rest) -> 
            if predicate current then do
                forward
                skipHelper predicate filtered

            else do
                setThreadState (Stuck rest)
                skipHelper predicate (current : filtered)  


-- HANDLE EFFECTS

{- 
Threads have to sometimes influence other threads, either to spawn
when moving forward, or roll other threads (e.g. roll a child thread when 
the parent wants to roll a spawn).

These things are called `Effects`, and the functions below process them in-order
giving a new state.
-}


handleBackwardEffects :: BackwardMsg -> Execution () 
handleBackwardEffects action = 
    case action of
        RollChild { caller, toRoll, actor } -> do
            -- a parent wants to roll its child, then undo the spawning completely
            threadProgram <- rollThread toRoll 

            scheduleThreadBackward caller 
            liftContext $ Context.removeThread toRoll

            let addHistory :: Thread History Program -> Thread History Program 
                addHistory thread@Thread{pid, program} = 
                    thread { program = SpawnThread actor (renameCreator pid threadProgram) : program }

            State.modify (change $ ThreadState.mapActive addHistory . ThreadState.removeUninitialized toRoll) 

        RollSend { caller, channelName,  histories } -> 
            -- rolling a send requires rolling all subsequent actions on the channel
            Foldable.traverse_ (rollChannel channelName) histories

        RollReceive { caller, channelName, histories } -> 
            -- rolling a receive requires rolling all subsequent actions on the channel
            Foldable.traverse_ (rollChannel channelName) histories


handleForwardEffects :: (MonadError Error m, Has (ThreadState History Program) c) => ForwardMsg -> StateT c m () 
handleForwardEffects action = 
    case action of
        Spawn thread@Thread { pid, program } ->
            -- add the thread to the state (it is in the context already)
            State.modify $ change $ ThreadState.add $ thread { program = map (MicroOz.renameCreator pid) program }


-- ATOMIC MOVING FUNCTIONS


backward :: Execution () 
backward = do
    state <- extract <$> State.get

    case state of 
        Running current@Thread{ pid } rest -> do
            ( progress, commands ) <- liftContext $ rollback current

            let messages = Cmd.unpack commands

            case progress of
                Done -> do
                    let uninitializeThread :: ThreadState History Program -> ThreadState History Program 
                        uninitializeThread state = 
                            case state of 
                                Running current other -> 
                                    ThreadState.addUninitialized current (Stuck other)

                                Stuck other -> 
                                    Stuck other 

                    State.modify (change uninitializeThread)
                    Foldable.traverse_ handleBackwardEffects messages

                Step newCurrent -> do
                    setThreadState $ Running newCurrent rest
                    Foldable.traverse_ handleBackwardEffects messages


        Stuck rest ->
            case ThreadState.rescheduleBackward state  of 
                Just rescheduled -> do
                    setThreadState rescheduled
                    backward 

                Nothing ->  
                    Except.throwError $ SchedulingError $ ThreadScheduleError (PID.create []) DeadLock


handleBlockedThread :: Error -> Execution () 
handleBlockedThread error =  
    case error of 
        BlockedOnReceive pid -> 
            withRunning $ \current rest -> do
                    newState <- 
                        Stuck rest
                            -- reschedule (makes sure the 'current' thread is not immediately rescheduled  
                            |> ThreadState.reschedule
                            -- insert the 'current' thread into the blocked other threads
                            |> fmap (return . ThreadState.addBlocked current)
                            -- if rescheduling failed, propagate the error 
                            |> Maybe.fromMaybe (Except.throwError (BlockedOnReceive pid))
                            |> liftContext

                    setThreadState newState

        _ ->
            -- propagate any other errors
            Except.throwError error 


forward :: Execution () 
forward = 
    flip Except.catchError handleBlockedThread $ do
        state <- extract <$> State.get
        case state of 
            Running current@Thread{ pid } rest -> do
                ( progress, commands ) <- liftContext $ advance current 

                let messages = Cmd.unpack commands

                case progress of
                    Done -> do
                        setThreadState $ ThreadState.addInactive current $ Stuck rest
                        Foldable.traverse_ handleForwardEffects messages

                    Step newCurrent -> do
                        setThreadState $ Running newCurrent rest
                        Foldable.traverse_ handleForwardEffects messages


            Stuck rest ->
                case ThreadState.reschedule state  of 
                    Just rescheduled -> do
                        setThreadState rescheduled 
                        forward

                    Nothing ->  
                        Except.throwError $ SchedulingError $ ThreadScheduleError (PID.create []) DeadLock


advance :: (MonadState (Context Value) m, MonadError Error m) => Thread History Program -> m ( Progress (Thread History Program), Cmd.Cmd ForwardMsg) 
advance thread@(Thread pid currentActor histories program) = 
    case program of
        [] -> 
            return ( Done, Cmd.none )

        (p:ps) -> do
            (h, newActor, newProgram, cmd ) <- advanceP pid currentActor p ps
            return ( Step $ Thread pid newActor (h:histories) newProgram, cmd )


rollback :: (MonadState (Context Value) m, MonadError Error m) => Thread History Program -> m ( Progress (Thread History Program), Cmd.Cmd BackwardMsg)
rollback thread@(Thread pid currentActor histories program) = 
    case histories of
        [] -> 
            return ( Done , Cmd.none ) 


        (h:hs) -> do
            ( consumed, newActor, newProgram, cmd ) <- backwardP pid currentActor h program 
            --return ( Step $ Thread pid hs newProgram, cmd )
            if consumed then
                return ( Step $ Thread pid newActor hs newProgram, cmd )
            else
                return ( Step $ Thread pid newActor (h:hs) newProgram, cmd )

-- HELPERS 


setThreadState :: (Has (ThreadState History Program) c, MonadError Error m) => ThreadState History Program -> StateT c m () 
setThreadState = State.modify . change . const 


withRunning :: (Thread History Program -> OtherThreads History Program -> Execution ()) -> Execution ()
withRunning tagger = do
    state <- extract <$> State.get 
    case state of 
        Running current rest ->
            tagger current rest

        Stuck _ -> 
            return ()


-- Row Polymorphism

{-| An attempt at row polymorphism in haskell, which allows us to combine functions defined on a subsection of the state
(so either the Context, or the ThreadState) and embed them in `Execution`. 
-}
class Has smaller larger where
    extract :: larger -> smaller

    change :: (smaller -> smaller) -> (larger -> larger)

instance Has a (a, b) where
    extract = fst 

    change f ( a, b) = (f a, b)

instance Has b (a, b) where
    extract = snd 

    change f ( a, b) = (a, f b)


liftContext :: StateT (Context Value) (Either Error) a -> Execution a
liftContext smallerComputation = do
    smallerState <- extract <$> State.get

    (value, newSmallerState) <- lift $ State.runStateT smallerComputation smallerState

    State.modify $ change $ const newSmallerState

    return value


-- THREAD SCHEDULING

{-| lifts thread scheduling errors into our error type, preserving the context -} 
embedThreadScheduleError :: MonadError Error m =>  Either ThreadScheduleError a -> m a
embedThreadScheduleError value = 
    case value of 
        Left e ->
            Except.throwError $ SchedulingError e

        Right v ->
            return v


{-| Schedule a thread for a forwards move: the thread must be active, uninitialized or blocked -}
scheduleThread :: (Has (ThreadState History Program) c, MonadError Error m) => PID -> StateT c m () 
scheduleThread pid = do
    state <- extract <$> State.get 
    newState <- lift $ embedThreadScheduleError $ ThreadState.scheduleThread pid state
    setThreadState newState


{-| Schedule a thread for a backwards move: the thread must be active, inactive or blocked -}
scheduleThreadBackward :: (Has (ThreadState History Program) c, MonadError Error m) => PID -> StateT c m () 
scheduleThreadBackward pid = do 
    state <- extract <$> State.get 
    newState <- lift $ embedThreadScheduleError $ ThreadState.scheduleThreadBackward pid state
    setThreadState newState
