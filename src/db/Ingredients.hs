{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db.Ingredients where

import Db.Db
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


instance DbOperation Ingredient where
    insert pool (Just (Ingredient _ recipe name ingredienttype unit value)) = do
        res <- fetch pool (recipe, name ,ingredienttype, unit, value)
                            "INSERT INTO ingredients(recipe, name, type, unit, value) VALUES(?,?,?,?,?) RETURNING  id, recipe, name, type, unit, value" :: IO [(Maybe Integer, Integer, TL.Text, TL.Text, Integer, Integer)]
        return $ oneIngredient res

    update pool (Just (Ingredient _ recipe name ingredienttype unit value)) id= do
        res <- fetch pool (recipe, name ,ingredienttype, unit, value, id)
                            "UPDATE ingredients set recipe=?, name=?, type=?, unit=?, value=? WHERE id=? RETURNING  id, recipe, name, type, unit, value" :: IO [(Maybe Integer, Integer, TL.Text, TL.Text, Integer, Integer)]
        return $ oneIngredient res
    
    find  pool id = do 
                        res <- fetch pool (Only id) "SELECT id, recipe, name, type, unit, value FROM ingredients WHERE id=?" :: IO [(Maybe Integer, Integer, TL.Text, TL.Text, Integer, Integer)]
                        return $ oneIngredient res

    list  pool = do
                    res <- fetchSimple pool "SELECT id, recipe, name, type, unit, value FROM ingredients" :: IO [(Maybe Integer, Integer, TL.Text, TL.Text, Integer, Integer)]
                    return $ map (\(id, recipe, name, ingredienttype, unit, value) -> Ingredient id recipe name ingredienttype unit value) res

-- Function to convert tuple -> Maybe Ingredient
oneIngredient :: [(Maybe Integer, Integer, TL.Text, TL.Text, Integer, Integer)] -> Maybe Ingredient
oneIngredient ((id, recipe, name ,ingredienttype, unit, value) : _) = Just $ Ingredient id recipe name ingredienttype unit value
oneIngredient _ = Nothing


deleteIngredient :: Pool Connection -> TL.Text -> ActionT TL.Text IO ()
deleteIngredient pool id = do 
                                _ <- liftIO $ execSqlT pool [id] "DELETE FROM ingredients WHERE id=?"
                                return ()

