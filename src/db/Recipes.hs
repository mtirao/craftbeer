
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


instance DbOperation RecipeCooking where
    insert pool (Just (RecipeCooking _ recipe state)) = do
        res <- fetch pool (recipe, state)
                            "INSERT INTO recipes_cooking(recipe_id, state) VALUES(?,?) RETURNING  id, recipe_id, state" :: IO [(Maybe Integer, Integer, TL.Text)]
        return $ oneRecipeCooking res

    update pool (Just (RecipeCooking _ _ state)) idd= do
        res <- fetch pool (state, idd)
                            "UPDATE recipes_cooking SET state=? WHERE id=? RETURNING  id, recipe_id, state" :: IO [(Maybe Integer, Integer, TL.Text)]
        return $ oneRecipeCooking res
    
    find pool id = do 
                    res <- fetch pool (Only id) "SELECT id, recipe_id, state FROM recipes_cooking WHERE id=?" :: IO [(Maybe Integer, Integer, TL.Text)]
                    return $ oneRecipeCooking res

    list pool = do
                    res <- fetchSimple pool "SELECT id, recipe_id, state FROM recipes_cooking" :: IO [(Maybe Integer, Integer, TL.Text)]
                    return $ map (\(id, recipeId, state) -> RecipeCooking id recipeId state) res



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

findRecipeCooking :: Pool Connection -> TL.Text -> IO (Maybe Integer)
findRecipeCooking pool state = do
                                [Only i] <- fetch pool (Only state) "SELECT count(*) FROM recipes_cooking WHERE state=?" :: IO [Only Integer]
                                return $ countRecipe i


findIngredient pool idd = do
                            res <- fetch pool (Only idd) "SELECT count(name) FROM  WHERE recipe=?" :: IO [(Maybe Integer, Integer, TL.Text, TL.Text, Integer, Integer)]
                            return $ map (\(id, recipe, name, ingredienttype, unit, value) -> Ingredient id recipe name ingredienttype unit value) res

findStage pool idd = do
                        res <- fetch pool (Only idd) "SELECT id, recipe, type, temp, time FROM stages WHERE recipe=?" :: IO [(Maybe Integer, Integer, Integer, Integer, Integer )]
                        return $ map (\(id, recipeid, recipe_type, temp, time) -> Stage id recipeid recipe_type temp time) res

-- Function to convert tuple -> Maybe Ingredient

countRecipe :: Integer -> Maybe Integer
countRecipe i = if i > 0 then Just i else Nothing

oneRecipe :: [(Maybe Integer, TL.Text, TL.Text, Integer, Integer, Integer)] -> Maybe Recipe
oneRecipe ((id, style, name, ibu, abv, color) : _) = Just $ Recipe id style name ibu abv color
oneRecipe _ = Nothing

oneRecipeCooking :: [(Maybe Integer, Integer, TL.Text)] -> Maybe RecipeCooking
oneRecipeCooking ((id, recipe_id, state) : _) = Just $ RecipeCooking id recipe_id state
oneRecipeCooking _ = Nothing

deleteRecipe :: Pool Connection -> TL.Text -> ActionT TL.Text IO ()
deleteRecipe pool id = do 
                            _ <- liftIO $ execSqlT pool [id] "DELETE FROM recipes WHERE id=?"
                            return ()

