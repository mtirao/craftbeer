{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Controller.RecipesController where

import Domain
import Views
import Db.Recipes
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

listRecipes pool =  do
                        recipes <- liftIO $ findRecipes pool 
                        jsonResponse recipes 

getRecipe pool idd = do 
                        maybeRecipe <- liftIO $ findRecipe pool idd
                        case maybeRecipe of
                            Nothing -> status status400
                            Just a -> jsonResponse a 

deleteRecipeId pool idd = do
                            deleteRecipe pool idd
                            status status204