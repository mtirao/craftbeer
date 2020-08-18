{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db.Users where

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




instance DbOperation User where
    insert pool (Just (User user password name lastname role)) = do
        res <- fetch pool (role, password, user, name, lastname)
                            "INSERT INTO users(role, password, username, name, lastname) VALUES(?,?,?,?,?) RETURNING id, role, password, user, name, lastname" :: IO [(Integer, TL.Text, TL.Text, TL.Text, TL.Text, TL.Text )]
        return $ oneUser res
            where oneUser ((id, role, password, user, name, lastname) : _) = Just $ User user password name lastname role
                  oneUser _ = Nothing
    
    update pool (Just (User user password name lastname role)) = do
        res <- fetch pool (role, password, user, name, lastname)
                            "INSERT INTO users(role, password, username, name, lastname) VALUES(?,?,?,?,?) RETURNING id, role, password, user, name, lastname" :: IO [(Integer, TL.Text, TL.Text, TL.Text, TL.Text, TL.Text )]
        return $ oneUser res
            where oneUser ((id, role, password, user, name, lastname) : _) = Just $ User user password name lastname role
                  oneUser _ = Nothing
