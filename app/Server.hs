{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import System.Environment
import qualified Data.Map as Map

import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors, simpleCors)

import qualified MicroOz
import qualified MicroOz.Parser
import qualified Data.ThreadState as ThreadState
import Data.ReplState
import Data.Context as Context (Context, localTypeStates)
import Data.Identifier (Identifier)

import qualified Repl 
import DebuggerParser (Instruction(SkipLets))

{-| the API described as a type -}
type UserAPI = 
    "initialize" :> ReqBody '[JSON] String :> Post '[JSON] ReplState
        :<|> "step" :> ReqBody '[JSON] (Instruction, Bool, ReplState) :> Post '[JSON] ReplState
        :<|> Raw
        
 
server :: Server UserAPI
server = initialize :<|> step :<|> serveDirectoryFileServer "frontend/" 
  where initialize :: String -> Handler ReplState
        initialize input = 
            case MicroOz.Parser.programWithTypes input of
                Left e -> 
                    error (show e)
                Right (globalType, types, program) -> 
                    let ( context, thread ) = MicroOz.init globalType types program 
                    in return $ ReplState context (ThreadState.singleton thread) 

        step :: (Instruction, Bool, ReplState) -> Handler ReplState
        step (instruction, fastForward, replState) =
            case Repl.interpretInstruction instruction replState of
                Left e -> error (show e)
                Right newState -> 
                    if fastForward then
                        case Repl.interpretInstruction SkipLets newState of 
                            Left e -> error (show e)
                            Right newer -> return newer
                    else
                        return newState


userAPI :: Proxy UserAPI
userAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve userAPI server

main :: IO ()
main = do 
    -- read the port from an environment variable (important for heroku)
    port <- getEnv "PORT"

    -- sethost "*" allows incoming requests from all hosts: also important for heroku
    let settings = setPort (read port) $ setHost "*" defaultSettings

    putStrLn $ "running on port " ++ port
    runSettings settings $ corsified app

-- | CORS middleware configured with 'appCorsResourcePolicy'.
-- | allows elm requests CORS requests (OPTIONS)
corsified :: Middleware
corsified = cors (const $ Just appCorsResourcePolicy)

-- | Cors resource policy to be used with 'corsified' middleware.
--
-- This policy will set the following:
--
-- * RequestHeaders: @Content-Type@
-- * MethodsAllowed: @OPTIONS, GET, PUT, POST@
appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy = CorsResourcePolicy {
    corsOrigins        = Nothing
  , corsMethods        = ["OPTIONS", "GET", "PUT", "POST"]
  , corsRequestHeaders = ["Authorization", "Content-Type"]
  , corsExposedHeaders = Nothing
  , corsMaxAge         = Nothing
  , corsVaryOrigin     = False
  , corsRequireOrigin  = False
  , corsIgnoreFailures = False
                                           }
