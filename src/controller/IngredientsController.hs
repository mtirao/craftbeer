{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Controller.IngredientsController where

import Domain
import Views
import Db.Ingredients
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


---CREATE
createIngredient pool bodyI = do
                                b <- bodyI
                                ingredient <- return $ (decode b :: Maybe Ingredient)
                                case ingredient of
                                    Nothing -> status status400
                                    Just _ -> ingredientResponse pool ingredient

ingredientResponse pool ingredient = do 
                                        dbIngredient <- liftIO $ insert pool ingredient
                                        case dbIngredient of
                                                Nothing -> status status400
                                                Just a -> dbIngredientResponse 
                                                        where dbIngredientResponse = do
                                                                                jsonResponse a
                                                                                status status201

----UPDATE
updateIngredientResponse pool urcpe  idd = do 
                                        dbIngredient <- liftIO $ update pool urcpe idd
                                        case dbIngredient of
                                            Nothing -> status status400
                                            Just a -> dbIngredient 
                                                    where dbIngredient = do
                                                                            jsonResponse a
                                                                            status status201  
updateIngredient pool ubody idd = do
                                b <- ubody
                                rcpe <- return $ (decode b :: Maybe Ingredient)
                                case rcpe of
                                    Nothing -> status status400
                                    Just _ -> updateIngredientResponse pool rcpe idd 

---GET & LIST
listIngredients pool =  do
                        ingredients <- liftIO $ (list pool :: IO [Ingredient])
                        jsonResponse ingredients 

getIngredient pool idd = do 
                        maybeIngredient <- liftIO $ (find pool idd :: IO (Maybe Ingredient))
                        case maybeIngredient  of
                            Nothing -> status status400
                            Just a -> jsonResponse a 

---DELETE
deleteIngredientId pool idd = do
                            deleteIngredient pool idd
                            status status204