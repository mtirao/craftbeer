
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db.Recipes where

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


instance DbOperation Recipe where
    insert pool (Just (Recipe _ style name ibu abv color)) = do
        res <- fetch pool (style, name, ibu, abv, color)
                            "INSERT INTO recipes(style, name, ibu, abv, color) VALUES(?,?,?,?,?) RETURNING  id, style, name, ibu, abv, color" :: IO [(Maybe Integer, TL.Text, TL.Text, Integer, Integer, Integer )]
        return $ oneRecipe res

    update pool (Just (Recipe _ style name ibu abv color)) idd= do
        res <- fetch pool (style, name, ibu, abv, color, idd)
                            "UPDATE recipes SET style=?, name=?, ibu=?, abv=?, color=? WHERE id=? RETURNING  id, style, name, ibu, abv, color" :: IO [(Maybe Integer, TL.Text, TL.Text, Integer, Integer, Integer )]
        return $ oneRecipe res
    
    find pool id = do 
                    res <- fetch pool (Only id) "SELECT id, style, name, ibu, abv, color FROM recipes WHERE id=?" :: IO [(Maybe Integer, TL.Text, TL.Text, Integer, Integer, Integer )]
                    return $ oneRecipe res

    list pool = do
                    res <- fetchSimple pool "SELECT id, style, name, ibu, abv, color FROM recipes" :: IO [(Maybe Integer, TL.Text, TL.Text, Integer, Integer, Integer )]
                    return $ map (\(id, style, name, ibu, abv, color) -> Recipe id style name ibu abv color) res


findIngredient pool idd = do
                            res <- fetch pool (Only idd) "SELECT id, recipe, name, type, unit, value FROM ingredients WHERE recipe=?" :: IO [(Maybe Integer, Integer, TL.Text, TL.Text, Integer, Integer)]
                            return $ map (\(id, recipe, name, ingredienttype, unit, value) -> Ingredient id recipe name ingredienttype unit value) res

findStage pool idd = do
                        res <- fetch pool (Only idd) "SELECT id, recipe, type, temp, time FROM stages WHERE recipe=?" :: IO [(Maybe Integer, Integer, Integer, Integer, Integer )]
                        return $ map (\(id, recipeid, recipe_type, temp, time) -> Stage id recipeid recipe_type temp time) res

-- Function to convert tuple -> Maybe Ingredient
oneRecipe :: [(Maybe Integer, TL.Text, TL.Text, Integer, Integer, Integer)] -> Maybe Recipe
oneRecipe ((id, style, name, ibu, abv, color) : _) = Just $ Recipe id style name ibu abv color
oneRecipe _ = Nothing

deleteRecipe :: Pool Connection -> TL.Text -> ActionT TL.Text IO ()
deleteRecipe pool id = do 
                            _ <- liftIO $ execSqlT pool [id] "DELETE FROM recipes WHERE id=?"
                            return ()

