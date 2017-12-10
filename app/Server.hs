{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson as Aeson (FromJSON, ToJSON)
import Data.Text
import qualified Data.Map as Map
import Servant

import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Cors       (CorsResourcePolicy(..), cors, simpleCors)


import Data.ReplState
import Data.Context as Context (Context, localTypeStates)
import qualified Data.ThreadState as ThreadState
import MicroOz (Value)
import qualified MicroOz 
import qualified MicroOz.Parser
import Types (Identifier(..))
import SessionType 

import qualified Repl 

import DebuggerParser (Instruction)

type Current = ReplState 

type UserAPI = 
    "initialize" :> ReqBody '[JSON] String :> Post '[JSON] Current
        :<|> "step" :> ReqBody '[JSON] StepInfo :> Post '[JSON] String
 
type User = String

data StepInfo = StepInfo { instruction :: Instruction } deriving (Generic, FromJSON, ToJSON)

users1 = ["test"]

server1 :: Server UserAPI
server1 = initialize :<|> step  
  where initialize :: String -> Handler Current
        initialize input = 
            case MicroOz.Parser.programWithTypes input of
                Left e -> 
                    error (show e)
                Right (types, program) -> 
                    let ( context, thread ) = MicroOz.init types program 
                        in case Repl.run $ ReplState context (ThreadState.singleton thread) of 
                            Left e -> error e
                            Right new -> return new 

        step :: StepInfo -> Handler String
        step _ = return "step" 

userAPI :: Proxy UserAPI
userAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = serve userAPI server1

main :: IO ()
main = run 8081 $ corsified app1

-- | CORS middleware configured with 'appCorsResourcePolicy'.
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
