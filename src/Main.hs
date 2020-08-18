{-# LANGUAGE OverloadedStrings #-}

module Main where

import Db.Db as Db
import Db.Recipes 
import Db.Sensors
import Db.Agents
import Db.Stages
import Db.Users


import Views
import Domain

import Controller.RecipesController
import Controller.IngredientsController

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)

import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.HTTP.Types.Status

import Control.Monad.IO.Class

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Text.Lazy as TL
import Data.Pool(createPool)
import Data.Aeson

import Database.PostgreSQL.Simple



-- Parse file "application.conf" and get the DB connection info
makeDbConfig :: C.Config -> IO (Maybe Db.DbConfig)
makeDbConfig conf = do
    name <- C.lookup conf "database.name" :: IO (Maybe String)
    user <- C.lookup conf "database.user" :: IO (Maybe String)
    dbConfPassword <- C.lookup conf "database.password" :: IO (Maybe String)
    return $ DbConfig <$> name
                    <*> user
                    <*> dbConfPassword

main :: IO ()
main = do
    loadedConf <- C.load [C.Required "application.conf"]
    dbConf <- makeDbConfig loadedConf
    
    case dbConf of
        Nothing -> putStrLn "No database configuration found, terminating..."
        Just conf -> do      
            pool <- createPool (newConn conf) close 1 40 10
            scotty 3000 $ do
                middleware $ staticPolicy (noDots >-> addBase "static") -- serve static files
                middleware $ logStdout                                  -- log all requests; for production use logStdout
             

                -- AUTH
                post   "/accounts/login" $ do 
                                            b <- body
                                            login <- return $ (decode b :: Maybe Login)
                                            result <- liftIO $ findUserByLogin pool (TL.unpack (username login))
                                            case result of 
                                                Nothing -> do 
                                                            jsonResponse (ErrorMessage "User not nothing")
                                                            status badRequest400
                                                Just (User pwd _ usrname lastname role) -> 
                                                            if pwd == (password login) 
                                                            then jsonResponse (UserResponse usrname lastname role) 
                                                                 
                                                            else do 
                                                                    jsonResponse (ErrorMessage "Wrong password") 
                                                                    status badRequest400

                post "/accounts/signup" $ do 
                                            b <- body
                                            user <- return $ (decode b :: Maybe User)
                                            dbNewUser <- liftIO $ insert pool user
                                            case dbNewUser of 
                                                Nothing -> status status400
                                                Just a -> signupResponse 
                                                        where signupResponse = do
                                                                                    jsonResponse a
                                                                                    status status201 

                -- STAGES
                post "/craftbeer/stage" $ do
                                            b <- body
                                            stage <- return $ (decode b :: Maybe Stage)
                                            dbStage <- liftIO $ insert pool stage
                                            case dbStage of 
                                                Nothing -> status status400
                                                Just a -> stageResponse 
                                                        where stageResponse = do
                                                                                    jsonResponse a
                                                                                    status status201 
                                            

                -- SENSORS
                post "/craftbeer/sensor" $ do
                                            b <- body
                                            sensor <- return $ (decode b :: Maybe Sensor)
                                            dbSensor <- liftIO $ insert pool sensor
                                            case dbSensor of 
                                                Nothing -> status status400
                                                Just a -> sensorResponse 
                                                        where sensorResponse = do
                                                                                    jsonResponse a
                                                                                    status status201  

                -- RECIPES
                post "/craftbeer/recipe" $ createRecipe pool body
                delete "/craftbeer/recipe/:id" $ do 
                                                    idd <- param "id" :: ActionM TL.Text
                                                    deleteRecipe pool idd
                                                    status status204
                                                    
                get "/craftbeer/recipe/:id" $ do   
                                                idd <- param "id" :: ActionM TL.Text
                                                maybeRecipe <- liftIO $ findRecipe pool idd
                                                case maybeRecipe of
                                                    Nothing -> status status400
                                                    Just a -> jsonResponse a 

                get "/craftbeer/recipes" $ listRecipes pool                                      
                                                                                        
                                                
                -- INGREDIENTS
                post "/craftbeer/ingredient" $ createIngredient pool body 

                -- AGENTS
                post "/craftbeer/agent" $ do
                                            b <- body
                                            agent <- return $ (decode b :: Maybe Agent)
                                            dbAgent <- liftIO $ insert pool agent
                                            case dbAgent of 
                                                Nothing -> status status400
                                                Just a -> agentResponse 
                                                        where agentResponse = do
                                                                                    jsonResponse a
                                                                                    status status201  
              
                -- LIST
                get    "/articles" $ do 
                                        articles <- liftIO $ listArticles pool  -- get the ist of articles for DB
                                        articlesList articles                   -- show articles list

                -- VIEW
                get    "/articles/:id" $ do 
                                            idd <- param "id" :: ActionM TL.Text -- get the article id from the request
                                            maybeArticle <- liftIO $ findArticle pool idd -- get the article from the DB
                                            viewArticle maybeArticle            -- show the article if it was found


                -- UPDATE
                put    "/admin/articles" $ do 
                                                article <- getArticleParam -- read the request body, try to parse it into article
                                                updateArticle pool article -- update parsed article in the DB
                                                updatedArticle article     -- show info that the article was updated

-----------------------------------------------

-- Parse the request body into the Article
getArticleParam :: ActionT TL.Text IO (Maybe Article)
getArticleParam = do 
                    b <- body
                    return $ (decode b :: Maybe Article)

-- Parse the request body into the Login
getLoginParam :: ActionT TL.Text IO (Maybe Login)
getLoginParam = do 
                    b <- body
                    return $ (decode b :: Maybe Login)


              