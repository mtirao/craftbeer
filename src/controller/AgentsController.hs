{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Controller.AgentsController where

import Domain
import Views
import Db.Agents
import Db.Db

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)


import Control.Monad.IO.Class
import Database.PostgreSQL.Simple
import Data.Pool(Pool, createPool, withResource)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

import GHC.Int
import GHC.Generics (Generic)

import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.HTTP.Types.Status

import Data.Aeson


---CREATE
createAgent pool bodyI = do
                                b <- bodyI
                                agent <- return $ (decode b :: Maybe Agent)
                                case agent of
                                    Nothing -> status status400
                                    Just _ -> agentResponse pool agent

agentResponse pool agent = do 
                                dbAgent <- liftIO $ insert pool agent
                                case dbAgent of
                                        Nothing -> status status400
                                        Just a -> dbAgentResponse 
                                                where dbAgentResponse = do
                                                                        jsonResponse a
                                                                        status status201

----UPDATE
updateAgentResponse pool urcpe  idd = do 
                                        dbAgent <- liftIO $ update pool urcpe idd
                                        case dbAgent of
                                            Nothing -> status status400
                                            Just a -> dbAgent 
                                                    where dbAgent = do
                                                                            jsonResponse a
                                                                            status status201  
updateAgent pool ubody idd = do
                                b <- ubody
                                rcpe <- return $ (decode b :: Maybe Agent)
                                case rcpe of
                                    Nothing -> status status400
                                    Just _ -> updateAgentResponse pool rcpe idd 

---GET & LIST
listAgents pool =  do
                        agents <- liftIO $ (list pool :: IO [Agent])
                        jsonResponse agents 

getAgent pool idd = do 
                        maybeAgent <- liftIO $ (find pool idd :: IO (Maybe Agent))
                        case maybeAgent  of
                            Nothing -> status status400
                            Just a -> jsonResponse a 

---DELETE
deleteAgentId pool idd = do
                            deleteAgent pool idd
                            status status204