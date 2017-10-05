import qualified Data.Map as Map
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Data.IORef

type Map = Map.Map
type Name = String
type Environment = IORef (Map Name Value)

getVar :: Environment -> Name -> IO Value 
getVar envRef name = do
    env <- readIORef envRef
    case Map.lookup name env of 
        Nothing -> 
            error $ "Unbound variable: " ++ name

        Just v -> 
            return v
defineVar :: Environment -> String -> Value -> IO ()
defineVar envRef name value = do
    env <- readIORef envRef
    writeIORef envRef $ Map.insert name value env


eval :: Environment -> S -> IO ()
eval env program = 
    case program of 
        Skip -> 
            return () 

        Assignment name value continuation -> do
            defineVar env name (Term value)
            eval env continuation
            
        SpawnThread work -> do
            threadID <- forkIO $ eval env work
            return () 
    
        Sequence a b -> do 
            eval env a
            eval env b


concurrently env a b = do
            var <- newEmptyMVar 

            _ <- forkProcess env var a 
            _ <- forkProcess env var b
    
            res <- takeMVar var
            return () 

forkProcess env var expression = 
    forkIO $ do 
        res <- eval env expression
        _ <- tryPutMVar var res
        return () 
    

data Value 
    = Term Term 
    | Chan Channel 
    deriving (Show)

data IntExpression 
    = Value IntValue
    | Addition IntExpression IntExpression
    | Subtraction IntExpression IntExpression
    | Multiplication IntExpression IntExpression
    | IntegerDivision IntExpression IntExpression


newtype IntValue = IntValue Int 

newtype BoolValue = BoolValue Bool


type Program = S


program = 
    Assignment "z" true $ 
        Assignment "x" port $ 
            Sequence (SpawnThread skip) 
                ( receive "x" ) 
        {-
            Sequence
                ( CreateThread skip  (send "x" "z")   ) 
                skip -- ( Assignment "y" (receive "x") skip )
            -} 



data S 
    = Assignment String Term S
    | Skip
    | SpawnThread S
    | Sequence S S
    | Receive String
    deriving (Show)

skip = Skip
receive = receive

data Term 
    = TBool Bool 
    | TPort 
    | TVar Name (Maybe EffectType)
    deriving (Show)

data EffectType = HttpRequest | HttpResponse deriving (Eq, Show)


true = TBool True
false = TBool False
port = TPort

main = do
    env <- newIORef Map.empty
    eval env program
    
    finalEnv <- readIORef env
    print finalEnv


data Channel = Channel {
               _send         :: String -> IO ()
             , _receive      :: IO String
             , _extra        :: [String]
             }

instance Show Channel where show _ = "Channel"

evalChan env t = do
    chan <- evalTerm env t
    case chan of
        Chan c -> return c
        _ -> error "not a channel"


evalTerm :: Environment -> Term -> IO Value
evalTerm env term =
    case term of
        TBool _ -> return $ Term term
        TPort -> return $ Term term

