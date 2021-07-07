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
import Controller.AgentsController
import Controller.SensorsController
import Controller.StagesController

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
    dbConfname <- C.lookup conf "database.name" :: IO (Maybe String)
    dbConfUser <- C.lookup conf "database.user" :: IO (Maybe String)
    dbConfPassword <- C.lookup conf "database.password" :: IO (Maybe String)
    dbConfHost <- C.lookup conf "database.host" :: IO (Maybe String)
    return $ DbConfig <$> dbConfname
                    <*> dbConfUser
                    <*> dbConfPassword
                    <*> dbConfHost

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
                                            result <- liftIO $ findUserByLogin pool (TL.unpack (getUserName login))
                                            case result of 
                                                Nothing -> do 
                                                            jsonResponse (ErrorMessage "User not nothing")
                                                            status badRequest400
                                                Just (User pwd _ usrname lastname role) -> 
                                                            if pwd == (getPassword login) 
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
                post "/api/craftbeer/stage" $ createStage pool
                

                post "/api/craftbeer/stage/cooking" $ createStageCooking pool              


                delete "/api/craftbeer/stages/cooking/recipe/:id" $ do 
                                                                    idd <- param "id" :: ActionM TL.Text
                                                                    deleteAllStageCooking pool idd

                -- SENSORS

                post "/api/craftbeer/sensor" $ createSensor pool body

                put "/api/craftbeer/sensor/:id" $ do 
                                                    idd <- param "id" :: ActionM TL.Text
                                                    updateSensor pool body idd

                delete "/api/craftbeer/sensor/:id" $ do 
                                                    idd <- param "id" :: ActionM TL.Text
                                                    deleteSensorId pool idd

                get "/api/craftbeer/sensor/:id" $ do
                                                    idd <- param "id" :: ActionM TL.Text
                                                    getSensor pool idd

                get "/api/craftbeer/sensor" $ listSensors pool

                post "/api/craftbeer/sensor" $ do
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
                post "/api/craftbeer/recipe/cooking" $ createRecipeCooking pool body

                get "/api/craftbeer/recipe/cooking" $ listRecipesCooking pool


                post "/api/craftbeer/recipe" $ createRecipe pool body
                                                   
                put "/api/craftbeer/recipe/:id" $ do 
                                                idd <- param "id" :: ActionM TL.Text
                                                updateRecipe pool body idd
                                                
                delete "/api/craftbeer/recipe/:id" $ do 
                                                    idd <- param "id" :: ActionM TL.Text
                                                    deleteRecipeId pool idd
                                                    
                get "/api/craftbeer/recipe/:id" $ do   
                                                idd <- param "id" :: ActionM TL.Text
                                                getRecipe pool idd

                get "/api/craftbeer/recipes" $ listRecipes pool                                      

                get "/api/craftbeer/recipe/:id/ingredients" $ do  
                                                            idd <- param "id" :: ActionM TL.Text
                                                            getIngredientRecipe pool idd   
                
                get "/api/craftbeer/recipe/:id/stages" $ do  
                                                        idd <- param "id" :: ActionM TL.Text
                                                        getStagesRecipe pool idd     
                                                         
                                                
                -- INGREDIENTS
                post "/api/craftbeer/ingredient" $ createIngredient pool body 

                put "/api/craftbeer/ingredient/:id" $ do 
                                                    idd <- param "id" :: ActionM TL.Text
                                                    updateRecipe pool body idd

                delete "/api/craftbeer/ingredient/:id" $ do 
                                                        idd <- param "id" :: ActionM TL.Text
                                                        deleteIngredientId pool idd

                get "/api/craftbeer/ingredient/:id" $ do
                                                    idd <- param "id" :: ActionM TL.Text
                                                    getIngredient pool idd

                get "/api/craftbeer/ingredients" $ listIngredients pool


                -- AGENTS
                post "/api/craftbeer/agent" $ createAgent pool body
                put "/api/craftbeer/agent/:id" $ do 
                                                    idd <- param "id" :: ActionM TL.Text
                                                    updateAgent pool body idd

                delete "/api/craftbeer/agent/:id" $ do 
                                                    idd <- param "id" :: ActionM TL.Text
                                                    deleteAgentId pool idd

                get "/api/craftbeer/agent/:id" $ do
                                                    idd <- param "id" :: ActionM TL.Text
                                                    getAgent pool idd

                get "/api/craftbeer/agent" $ listAgents pool
              
                
-----------------------------------------------

-- Parse the request body into the Login
getLoginParam :: ActionT TL.Text IO (Maybe Login)
getLoginParam = do 
                    b <- body
                    return $ (decode b :: Maybe Login)





              