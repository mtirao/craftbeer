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
    
    update pool (Just (Agent _ agenttype ip)) id= do
        res <- fetch pool (agenttype, ip, id)
                            "UPDATE agents SET agenttype=?, ip=?  WHERE id=?" :: IO [(Maybe Integer, TL.Text, TL.Text)]
        return $ oneAgent res
            where oneAgent ((id, agenttype, ip) : _) = Just $ Agent id agenttype ip
                  oneAgent _ = Nothing

    find  pool id = do 
                        res <- fetch pool (Only id) "SELECT id, agenttype, ip FROM agents WHERE id=?" :: IO [(Maybe Integer, TL.Text, TL.Text)]
                        return $ oneAgent res
                        where oneAgent ((id, agenttype, ip) : _) = Just $ Agent id agenttype ip
                              oneAgent _ = Nothing

    list  pool = do
                    res <- fetchSimple pool "SELECT id, agenttype, ip FROM agents" :: IO [(Maybe Integer, TL.Text, TL.Text)]
                    return $ map (\(id, agenttype, ip) -> Agent id agenttype ip) res

deleteAgent :: Pool Connection -> TL.Text -> ActionT TL.Text IO ()
deleteAgent pool id = do 
                        _ <- liftIO $ execSqlT pool [id] "DELETE FROM agents WHERE id=?"
                        return ()
