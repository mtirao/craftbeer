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


createRecipe pool body  = do
                            b <- body
                            rcpe <- return $ (decode b :: Maybe Recipe)
                            case rcpe of
                                Nothing -> status status400
                                Just _ -> recipeResponse 
                                        where recipeResponse = do 
                                                                    dbRecipe <- liftIO $ insert pool rcpe
                                                                    case dbRecipe of
                                                                        Nothing -> status status400
                                                                        Just a -> dbRecipeResponse 
                                                                                where dbRecipeResponse = do
                                                                                                        jsonResponse a
                                                                                                        status status201  