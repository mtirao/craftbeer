{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db where
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

-- DbConfig contains info needed to connect to Postgresql server
data DbConfig = DbConfig {
     dbName :: String,
     dbUser :: String,
     dbPassword :: String
     }
     deriving (Show, Generic)

-- The function knows how to create new DB connection
-- It is needed to use with resource pool
newConn :: DbConfig -> IO Connection
newConn conf = connect defaultConnectInfo
                       { connectUser = dbUser conf
                       , connectPassword = dbPassword conf
                       , connectDatabase = dbName conf
                       }

--------------------------------------------------------------------------------


-- No arguments -- just pure sql
-- fetchSimple :: QueryResults r => Pool Connection -> Query -> IO [r]
fetchSimple pool sql = withResource pool retrieve
       where retrieve conn = query_ conn sql


--------------------------------------------------------------------------------

listArticles :: Pool Connection -> IO [Article]
listArticles pool = do
     res <- fetchSimple pool "SELECT * FROM article ORDER BY id DESC" :: IO [(Integer, T.Text, T.Text)]
     return $ map (\(id, title, bodyText) -> Article id title bodyText) res
