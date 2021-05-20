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

    update pool (Just (Sensor _ sensortype name file)) id = do
        res <- fetch pool (sensortype, name, file, id)
                            "UPDATE sensors SET type=?, name=?, file=? WHERE id=? RETURNING  id, type, name, file" :: IO [(Maybe Integer, TL.Text, TL.Text, TL.Text )]
        return $ oneSensor res
            where oneSensor ((id, sensortype, name, file) : _) = Just $ Sensor id sensortype name file
                  oneSensor _ = Nothing

    find  pool id = do 
                        res <- fetch pool (Only id) "SELECT id, type, name, file FROM sensors WHERE id=?" :: IO [(Maybe Integer, TL.Text, TL.Text, TL.Text )]
                        return $ oneSensor res
                        where oneSensor ((id, sensortype, name, file) : _) = Just $ Sensor id sensortype name file
                              oneSensor _ = Nothing

    list  pool = do
                    res <- fetchSimple pool "SELECT id, type, name, file FROM sensors" :: IO [(Maybe Integer, TL.Text, TL.Text, TL.Text )]
                    return $ map (\(id, sensortype, name, file) -> Sensor id sensortype name file) res


deleteSensor :: Pool Connection -> TL.Text -> ActionT TL.Text IO ()
deleteSensor pool id = do 
                        _ <- liftIO $ execSqlT pool [id] "DELETE FROM sensors WHERE id=?"
                        return ()
