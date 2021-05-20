{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Controller.SensorsController where

import Domain
import Views
import Db.Sensors
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
createSensor pool cbody  = do
                            b <- cbody
                            rcpe <- return $ (decode b :: Maybe Sensor)
                            case rcpe of
                                Nothing -> status status400
                                Just _ -> createSensorResponse pool rcpe
                                         

createSensorResponse pool rcpe = do 
                                dbSensor <- liftIO $ insert pool rcpe
                                case dbSensor of
                                    Nothing -> status status400
                                    Just a -> dbSensorResponse 
                                            where dbSensorResponse = do
                                                                    jsonResponse a
                                                                    status status201 
----UPDATE
updateSensor pool ubody idd  = do
                            b <- ubody
                            rcpe <- return $ (decode b :: Maybe Sensor)
                            case rcpe of
                                Nothing -> status status400
                                Just _ -> updateSensorResponse pool rcpe idd 

updateSensorResponse pool urcpe  idd = do 
                                        dbSensor <- liftIO $ update pool urcpe idd
                                        case dbSensor of
                                            Nothing -> status status400
                                            Just a -> dbSensorResponse 
                                                    where dbSensorResponse = do
                                                                            jsonResponse a
                                                                            status status201  

---GET & LIST
listSensors pool =  do
                        sensors <- liftIO $ (list pool :: IO [Sensor])
                        jsonResponse sensors 

getSensor pool idd = do 
                        maybeSensor <- liftIO $ (find pool idd :: IO (Maybe Sensor))
                        case maybeSensor of
                            Nothing -> status status400
                            Just a -> jsonResponse a 

---DELETE
deleteSensorId pool idd = do
                            deleteSensor pool idd
                            status status204