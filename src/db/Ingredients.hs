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
    insert pool (Just (Ingredient _ recipe name ingredienttype unit)) = do
        res <- fetch pool (recipe, name ,ingredienttype, unit)
                            "INSERT INTO ingredients(recipe, name, type, unit) VALUES(?,?,?,?) RETURNING  id, recipe, name, type, unit" :: IO [(Maybe Integer, Integer, TL.Text, TL.Text, Integer)]
        return $ oneIngredient res
            where oneIngredient ((id, recipe, name ,ingredienttype, unit) : _) = Just $ Ingredient id recipe name ingredienttype unit
                  oneIngredient _ = Nothing

    update pool (Just (Ingredient _ recipe name ingredienttype unit)) id= do
        res <- fetch pool (recipe, name ,ingredienttype, unit, id)
                            "UPDATE ingredients set recipe=?, name=?, type=?, unit=? WHERE id=? RETURNING  id, recipe, name, type, unit" :: IO [(Maybe Integer, Integer, TL.Text, TL.Text, Integer)]
        return $ oneIngredient res
            where oneIngredient ((id, recipe, name ,ingredienttype, unit) : _) = Just $ Ingredient id recipe name ingredienttype unit
                  oneIngredient _ = Nothing
    
    find  pool id = do 
                        res <- fetch pool (Only id) "SELECT id, recipe, name, type, unit FROM ingredients WHERE id=?" :: IO [(Maybe Integer, Integer, TL.Text, TL.Text, Integer)]
                        return $ oneIngredient res
                        where oneIngredient ((id, recipe, name ,ingredienttype, unit) : _) = Just $ Ingredient id recipe name ingredienttype unit
                              oneIngredient _ = Nothing

    list  pool = do
                    res <- fetchSimple pool "SELECT id, recipe, name, type, unit FROM ingredients" :: IO [(Maybe Integer, Integer, TL.Text, TL.Text, Integer)]
                    return $ map (\(id, recipe, name, ingredienttype, unit) -> Ingredient id recipe name ingredienttype unit) res


deleteIngredient :: Pool Connection -> TL.Text -> ActionT TL.Text IO ()
deleteIngredient pool id = do 
                                _ <- liftIO $ execSqlT pool [id] "DELETE FROM ingredients WHERE id=?"
                                return ()

