{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Db
import Views
import Auth
import Domain

import Web.Scotty as WS
import Web.Scotty.Internal.Types (ActionT)
import Network.Wai
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai.Middleware.HttpAuth
import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Pool(Pool, createPool, withResource)
import qualified Data.Text.Lazy as TL
import Data.Aeson 
import Database.PostgreSQL.Simple

import Network.Wreq as NW hiding (basicAuth)
import Control.Lens



-- Parse file "application.conf" and get the DB connection info
makeDbConfig :: C.Config -> IO (Maybe Db.DbConfig)
makeDbConfig conf = do
  name <- C.lookup conf "database.name" :: IO (Maybe String)
  user <- C.lookup conf "database.user" :: IO (Maybe String)
  password <- C.lookup conf "database.password" :: IO (Maybe String)
  return $ DbConfig <$> name
                    <*> user
                    <*> password

-- The function knows which resources are available only for the
-- authenticated users
protectedResources ::  Request -> IO Bool
protectedResources request = do
    let path = pathInfo request
    return $ protect path
    where protect (p : _) =  p == "admin"  -- all requests to /admin/* should be authenticated
          protect _       =  False         -- other requests are allowed for anonymous users

opts = defaults & NW.header "SOAPAction" .~ ["application/json"]

instance ToJSON Login where
    toJSON Login {..} = object
        [
            "username" Data.Aeson..= username,
            "password" Data.Aeson..= password
        ]


main :: IO ()
main = do
    loadedConf <- C.load [C.Required "application.conf"]
    dbConf <- makeDbConfig loadedConf
    
    case dbConf of
      Nothing -> putStrLn "No database configuration found, terminating..."
      Just conf -> do      
          pool <- createPool (newConn conf) close 1 40 10
          scotty 4000 $ do
              middleware $ staticPolicy (noDots >-> addBase "static") -- serve static files
              middleware $ logStdout                                  -- log all requests; for production use logStdout
              middleware $ basicAuth (verifyCredentials pool)         -- check if the user is authenticated for protected resources
                           "Haskell Blog Realm" { authIsProtected = protectedResources } -- function which restricts access to some routes only for authenticated users

              WS.get "/" do r <- liftIO $ NW.post "http://localhost:3000/accounts/login" (toJSON login)
                            raw (r ^. responseBody)
                                       
 
              -- UPDATE
              WS.put    "/admin/articles"  do  article <- getArticleParam -- read the request body, try to parse it into article
                                               updateArticle pool article -- update parsed article in the DB
                                               updatedArticle article     -- show info that the article was updated

            
              

-----------------------------------------------

-- Parse the request body into the Article
getArticleParam :: ActionT TL.Text IO (Maybe Article)
getArticleParam = do b <- body
                     return $ (decode b :: Maybe Article)
                     where makeArticle s = ""
