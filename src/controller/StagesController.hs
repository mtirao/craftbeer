{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Controller.StagesController where

import Domain
import Views
import Db.Stages
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

-----CREATE
createStage pool cbody  = do
                            b <- cbody
                            rcpe <- return $ (decode b :: Maybe Stage)
                            case rcpe of
                                Nothing -> status status400
                                Just _ -> createStageResponse pool rcpe
                                         

createStageResponse pool rcpe = do 
                                dbStage <- liftIO $ insert pool rcpe
                                case dbStage of
                                    Nothing -> status status400
                                    Just a -> dbStageResponse 
                                            where dbStageResponse = do
                                                                    jsonResponse a
                                                                    status status201 
----UPDATE
updateStage pool ubody idd  = do
                            b <- ubody
                            rcpe <- return $ (decode b :: Maybe Stage)
                            case rcpe of
                                Nothing -> status status400
                                Just _ -> updateStageResponse pool rcpe idd 

updateStageResponse pool urcpe  idd = do 
                                        dbStage <- liftIO $ update pool urcpe idd
                                        case dbStage of
                                            Nothing -> status status400
                                            Just a -> dbStageResponse 
                                                    where dbStageResponse = do
                                                                            jsonResponse a
                                                                            status status201  

---GET & LIST
listStages pool =  do
                        stages <- liftIO $ (list pool :: IO [Stage])
                        jsonResponse stages 

getStage pool idd = do 
                        maybeStage <- liftIO $ (find pool idd :: IO (Maybe Stage))
                        case maybeStage of
                            Nothing -> status status400
                            Just a -> jsonResponse a 

---DELETE
deleteStageId pool idd = do
                            deleteStage pool idd
                            status status204