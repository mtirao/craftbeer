{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Craftbeer (runApp) where


import Db
import Views
import Domain

import           Data.Aeson (Value(..), object, (.=))
import           Network.Wai (Application)
import qualified Web.Scotty as S

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)
import Network.Wai
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai.Middleware.HttpAuth
import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Database.PostgreSQL.Simple
import Data.Pool(Pool, createPool, withResource)
import qualified Data.Text.Lazy as TL
import Data.Aeson

-- Parse file "application.conf" and get the DB connection info
makeDbConfig :: C.Config -> IO (Maybe Db.DbConfig)
makeDbConfig conf = do
  name <- C.lookup conf "database.name" :: IO (Maybe String)
  user <- C.lookup conf "database.user" :: IO (Maybe String)
  password <- C.lookup conf "database.password" :: IO (Maybe String)
  return $ DbConfig <$> name
                    <*> user
                    <*> password

--app' :: S.ScottyM ()
--app' = do
--  S.get "/" $ do
--    S.text "hello"

--  S.get "/some-json" $ do
--    S.json $ object ["foo" .= Number 23, "bar" .= Number 42]

--app :: IO Application
--app = S.scottyApp app'


runApp :: IO ()
runApp = do 
    loadedConf <- C.load [C.Required "application.conf"]
    dbConf <- makeDbConfig loadedConf

    case dbConf of
      Nothing -> putStrLn "No database configuration found, terminating..."
      Just conf -> do
          pool <- createPool (newConn conf) close 1 64 10
          scotty 3000 $ do
            -- LIST
            get   "/articles" $ do 
                        articles <- liftIO $ listArticles pool  -- get the ist of articles for DB
                        articlesList articles                   -- show articles list

