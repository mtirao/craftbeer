{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Controller.RecipesController where

import Domain
import Views
import Db.Recipes
import Db.Ingredients
import Db.Stages
import Db.Db

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)


import Control.Monad.IO.Class
import Database.PostgreSQL.Simple
import Data.Pool(Pool, createPool, withResource)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

import GHC.Int
import GHC.Generics (Generic)

import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.HTTP.Types.Status

import Data.Aeson

-----CREATE
createRecipe pool cbody  = do
                            b <- cbody
                            rcpe <- return $ (decode b :: Maybe Recipe)
                            case rcpe of
                                Nothing -> status status400
                                Just _ -> createRecipeResponse pool rcpe
                                         

createRecipeResponse pool rcpe = do 
                                dbRecipe <- liftIO $ insert pool rcpe
                                case dbRecipe of
                                    Nothing -> status status400
                                    Just a -> dbRecipeResponse 
                                            where dbRecipeResponse = do
                                                                    jsonResponse a
                                                                    status status201 

createRecipeCooking pool cbody = do
                                    b <- cbody
                                    rcpe <- return $ (decode b :: Maybe RecipeCooking)
                                    case rcpe of
                                        Nothing -> status status400
                                        Just recipe -> do
                                            countRecipe <- liftIO $ (findRecipeCooking pool (recipe_state recipe) :: IO (Maybe Integer))
                                            case countRecipe of
                                                Just _ -> status status400
                                                Nothing -> createRecipeCookingResponse pool rcpe   
                                                                                  

createRecipeCookingResponse pool rcpe = do 
                                dbRecipe <- liftIO $ insert pool rcpe
                                case dbRecipe of
                                    Nothing -> status status400
                                    Just a -> dbRecipeResponse 
                                            where dbRecipeResponse = do
                                                                    jsonResponse a
                                                                    status status201                               

----UPDATE
updateRecipe pool ubody idd  = do
                            b <- ubody
                            rcpe <- return $ (decode b :: Maybe Recipe)
                            case rcpe of
                                Nothing -> status status400
                                Just _ -> updateRecipeResponse pool rcpe idd 

updateRecipeResponse pool urcpe  idd = do 
                                        dbRecipe <- liftIO $ update pool urcpe idd
                                        case dbRecipe of
                                            Nothing -> status status400
                                            Just a -> dbRecipeResponse 
                                                    where dbRecipeResponse = do
                                                                            jsonResponse a
                                                                            status status201  

---GET & LIST
listRecipes pool =  do
                        recipes <- liftIO $ (list pool :: IO [Recipe])
                        jsonResponse recipes 

listRecipesCooking pool =  do
                        recipes <- liftIO $ (list pool :: IO [RecipeCooking])
                        jsonResponse recipes 

getRecipe pool idd = do 
                        maybeRecipe <- liftIO $ (find pool idd :: IO (Maybe Recipe))
                        case maybeRecipe of
                            Nothing -> status status400
                            Just a -> jsonResponse a 

getIngredientRecipe pool idd = do 
                                ingredients <- liftIO $ (findIngredient pool idd :: IO [Ingredient])
                                jsonResponse ingredients 

getStagesRecipe pool idd = do 
                            stages <- liftIO $ (findStage pool idd :: IO [Stage])
                            jsonResponse stages


---DELETE
deleteRecipeId pool idd = do
                            deleteIngredientByRecipe pool idd
                            deleteStageByRecipe pool idd
                            deleteRecipe pool idd
                            status status204