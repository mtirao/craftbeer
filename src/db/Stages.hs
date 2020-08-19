{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db.Stages where

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


instance DbOperation Stage where
    insert pool (Just (Stage _ recipeid recipe_type temp time)) = do
        res <- fetch pool (recipeid, recipe_type, temp, time)
                            "INSERT INTO stages(recipeid, type, temp, time) VALUES(?,?,?,?) RETURNING  id, recipeid, type, temp, time" :: IO [(Maybe Integer, Integer, Integer, Integer, Integer )]
        return $ oneStage res
            where oneStage ((id, recipeid, recipe_type, temp, time) : _) = Just $ Stage id recipeid recipe_type temp time
                  oneStage _ = Nothing
    
    update pool (Just (Stage _ recipeid recipe_type temp time)) id = do
        res <- fetch pool (recipeid, recipe_type, temp, time, id)
                            "UPDATE stages SET recipeid=?, type=?, temp=?, time=? WHERE id=? RETURNING  id, recipeid, type, temp, time" :: IO [(Maybe Integer, Integer, Integer, Integer, Integer )]
        return $ oneStage res
            where oneStage ((id, recipeid, recipe_type, temp, time) : _) = Just $ Stage id recipeid recipe_type temp time
                  oneStage _ = Nothing
