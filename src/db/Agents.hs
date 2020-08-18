{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db.Agents where

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



instance DbOperation Agent where
    insert pool (Just (Agent _ agenttype ip)) = do
        res <- fetch pool (agenttype, ip)
                            "INSERT INTO agents(agenttype, ip) VALUES(?,?) RETURNING  id, agenttype, ip" :: IO [(Maybe Integer, TL.Text, TL.Text)]
        return $ oneAgent res
            where oneAgent ((id, agenttype, ip) : _) = Just $ Agent id agenttype ip
                  oneAgent _ = Nothing
    
    update pool (Just (Agent id agenttype ip)) = do
        res <- fetch pool (agenttype, ip, id)
                            "UPDATE agents SET agenttype=?, ip=?  WHERE id=?" :: IO [(Maybe Integer, TL.Text, TL.Text)]
        return $ oneAgent res
            where oneAgent ((id, agenttype, ip) : _) = Just $ Agent id agenttype ip
                  oneAgent _ = Nothing

 