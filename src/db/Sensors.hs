{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db.Sensors where

import Db.Db
import Domain

import Web.Scotty.Internal.Types (ActionT)
import GHC.Generics (Generic)
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple
import Data.Pool(Pool, createPool, withResource)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import GHC.Int


instance DbOperation Sensor where
    insert pool (Just (Sensor _ sensortype name file)) = do
        res <- fetch pool (sensortype, name, file)
                            "INSERT INTO sensors(type, name, file) VALUES(?,?,?) RETURNING  id, type, name, file" :: IO [(Maybe Integer, TL.Text, TL.Text, TL.Text )]
        return $ oneSensor res
            where oneSensor ((id, sensortype, name, file) : _) = Just $ Sensor id sensortype name file
                  oneSensor _ = Nothing

    update pool (Just (Sensor id sensortype name file)) = do
        res <- fetch pool (sensortype, name, file, id)
                            "UPDATE sensors SET type=?, name=?, file=? WHERE id=? RETURNING  id, type, name, file" :: IO [(Maybe Integer, TL.Text, TL.Text, TL.Text )]
        return $ oneSensor res
            where oneSensor ((id, sensortype, name, file) : _) = Just $ Sensor id sensortype name file
                  oneSensor _ = Nothing