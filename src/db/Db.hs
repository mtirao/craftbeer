{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db.Db where
    
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

-- DbConfig contains info needed to connect to MySQL server
data DbConfig = DbConfig {
        dbName :: String,
        dbUser :: String,
        dbPassword :: String
    } deriving (Show, Generic)

-- The function knows how to create new DB connection
-- It is needed to use with resource pool
newConn :: DbConfig -> IO Connection
newConn conf = connect defaultConnectInfo
                       { connectUser = dbUser conf
                       , connectPassword = dbPassword conf
                       , connectDatabase = dbName conf
                       }

--------------------------------------------------------------------------------
-- Utilities for interacting with the DB.
-- No transactions.
--
-- Accepts arguments
fetch :: (FromRow r, ToRow q) => Pool Connection -> q -> Query -> IO [r]
fetch pool args sql = withResource pool retrieve
      where retrieve conn = query conn sql args

-- No arguments -- just pure sql
fetchSimple :: FromRow r => Pool Connection -> Query -> IO [r]
fetchSimple pool sql = withResource pool retrieve
       where retrieve conn = query_ conn sql

-- Update database
execSql :: ToRow q => Pool Connection -> q -> Query -> IO Int64
execSql pool args sql = withResource pool ins
       where ins conn = execute conn sql args

-------------------------------------------------------------------------------
-- Utilities for interacting with the DB.
-- Transactions.
--
-- Accepts arguments
fetchT :: (FromRow r, ToRow q) => Pool Connection -> q -> Query -> IO [r]
fetchT pool args sql = withResource pool retrieve
      where retrieve conn = withTransaction conn $ query conn sql args

-- No arguments -- just pure sql
fetchSimpleT :: FromRow r => Pool Connection -> Query -> IO [r]
fetchSimpleT pool sql = withResource pool retrieve
       where retrieve conn = withTransaction conn $ query_ conn sql

-- Update database
execSqlT :: ToRow q => Pool Connection -> q -> Query -> IO Int64
execSqlT pool args sql = withResource pool ins
       where ins conn = withTransaction conn $ execute conn sql args

--------------------------------------------------------------------------------

findUserByLogin :: Pool Connection -> String -> IO (Maybe User)
findUserByLogin pool login = do
         res <- liftIO $ fetch pool (Only login) "SELECT role, password, username, name, lastname FROM users WHERE username=?" :: IO [(String, String, String, String, String)]
         return $ userResponse res
         where userResponse [(role, pwd, usrname, name, lastname)] = Just (User (TL.pack pwd) (TL.pack usrname) (TL.pack name) (TL.pack lastname) (TL.pack role))
               userResponse _ = Nothing

--------------------------------------------------------------------------------

listArticles :: Pool Connection -> IO [Article]
listArticles pool = do
     res <- fetchSimple pool "SELECT * FROM article ORDER BY id DESC" :: IO [(Integer, TL.Text, TL.Text)]
     return $ map (\(id, title, bodyText) -> Article id title bodyText) res
   
findArticle :: Pool Connection -> TL.Text -> IO (Maybe Article)
findArticle pool id = do
     res <- fetch pool (Only id) "SELECT * FROM article WHERE id=?" :: IO [(Integer, TL.Text, TL.Text)]
     return $ oneArticle res
     where oneArticle ((id, title, bodyText) : _) = Just $ Article id title bodyText
           oneArticle _ = Nothing


updateArticle :: Pool Connection -> Maybe Article -> ActionT TL.Text IO ()
updateArticle pool Nothing = return ()
updateArticle pool (Just (Article id title bodyText)) = do
     liftIO $ execSqlT pool [title, bodyText, (TL.decodeUtf8 $ BL.pack $ show id)]
                            "UPDATE article SET title=?, bodyText=? WHERE id=?"
     return ()



--------------------------------------------------------------------------------
class DbOperation a where 
    insert :: Pool Connection -> Maybe a -> IO (Maybe a)  --Pool Connection -> Maybe a -> ActionT TL.Text IO ()
    update :: Pool Connection -> Maybe a -> IO (Maybe a)

--------------------------------------------------------------------------------