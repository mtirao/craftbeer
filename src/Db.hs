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

findUserByLogin :: Pool Connection -> String -> IO (Maybe String)
findUserByLogin pool login = do
         res <- liftIO $ fetch pool (Only login) "SELECT role, password, username, name, lastname FROM users WHERE username=?" :: IO [(String, String, String, String, String)]
         return $ password res
         where password [(_, pwd, _, _, _)] = Just pwd
               password _ = Nothing

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


insertArticle :: Pool Connection -> Maybe Article -> ActionT TL.Text IO ()
insertArticle pool Nothing = return ()
insertArticle pool (Just (Article id title bodyText)) = do
     liftIO $ execSqlT pool [title, bodyText]
                            "INSERT INTO article(title, bodyText) VALUES(?,?)"
     return ()

updateArticle :: Pool Connection -> Maybe Article -> ActionT TL.Text IO ()
updateArticle pool Nothing = return ()
updateArticle pool (Just (Article id title bodyText)) = do
     liftIO $ execSqlT pool [title, bodyText, (TL.decodeUtf8 $ BL.pack $ show id)]
                            "UPDATE article SET title=?, bodyText=? WHERE id=?"
     return ()

deleteArticle :: Pool Connection -> TL.Text -> ActionT TL.Text IO ()
deleteArticle pool id = do
     liftIO $ execSqlT pool [id] "DELETE FROM article WHERE id=?"
     return ()


--------------------------------------------------------------------------------
class DbOperation a where 
    insert :: Pool Connection -> Maybe a -> ActionT TL.Text IO ()


instance DbOperation User where
    insert pool Nothing = return ()
    insert pool (Just (User user password name lastname role)) = do
        liftIO $ execSqlT pool [role, password, user, name, lastname]
                            "INSERT INTO users(role, password, username, name, lastname) VALUES(?,?,?,?,?)"
        return () 

instance DbOperation Stage where
    insert pool Nothing = return ()
    insert pool (Just (Stage recipeid recipe_type temp time)) = do
        liftIO $ execSqlT pool [recipeid, recipe_type, temp, time]
                            "INSERT INTO recipe(recipeid, type, temp, time) VALUES(?,?,?,?)"
        return () 

instance DbOperation Sensor where
    insert pool Nothing = return ()
    insert pool (Just (Sensor sensortype name file)) = do
        liftIO $ execSqlT pool [sensortype, name, file]
                            "INSERT INTO sensors(type, name, file) VALUES(?,?,?)"
        return () 

instance DbOperation Recipe where
    insert pool Nothing = return ()
    insert pool (Just (Recipe style name ibu abv color)) = do
        liftIO $ execSqlT pool [style, name, (TL.decodeUtf8 $ BL.pack $ show ibu), (TL.decodeUtf8 $ BL.pack $ show abv), (TL.decodeUtf8 $ BL.pack $ show color)]
                            "INSERT INTO recipes(style, name, ibu, abv, color) VALUES(?,?,?,?,?)"
        return () 

instance DbOperation Ingredient where
    insert pool Nothing = return ()
    insert pool (Just (Ingredient recipe name ingredienttype unit)) = do
        liftIO $ execSqlT pool [(TL.decodeUtf8 $ BL.pack $ show recipe), name, ingredienttype, (TL.decodeUtf8 $ BL.pack $ show unit)]
                            "INSERT INTO ingredient(recipe, name, type, unit) VALUES(?,?,?,?)"
        return () 

instance DbOperation Agent where
    insert pool Nothing = return ()
    insert pool (Just (Agent agenttype ip)) = do
        liftIO $ execSqlT pool [agenttype, ip]
                            "INSERT INTO agents(type, ip, type, unit) VALUES(?,?)"
        return () 



--------------------------------------------------------------------------------