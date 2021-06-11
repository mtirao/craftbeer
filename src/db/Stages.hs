{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db.Stages where

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
import Data.Time.LocalTime
import GHC.Int


instance DbOperation Stage where
    insert pool (Just (Stage _ recipeid stageType temp time)) = do
        res <- fetch pool (recipeid, stageType, temp, time)
                            "INSERT INTO stages(recipe, type, temp, time) VALUES(?,?,?,?) RETURNING  id, recipe, type, temp, time" :: IO [(Maybe Integer, Integer, Integer, Integer, Integer )]
        return $ oneStage res
            where oneStage ((id, recipeid, stageType, temp, time) : _) = Just $ Stage id recipeid stageType temp time
                  oneStage _ = Nothing
    
    update pool (Just (Stage _ recipeid stageType temp time)) id = do
        res <- fetch pool (recipeid, stageType, temp, time, id)
                            "UPDATE stages SET recipe=?, type=?, temp=?, time=? WHERE id=? RETURNING  id,  recipe, type, temp, time" :: IO [(Maybe Integer, Integer, Integer, Integer, Integer )]
        return $ oneStage res
            where oneStage ((id, recipeid, stageType, temp, time) : _) = Just $ Stage id recipeid stageType temp time
                  oneStage _ = Nothing

    find  pool id = do 
                        res <- fetch pool (Only id) "SELECT id, recipe, type, temp, time FROM stages WHERE id=?" :: IO [(Maybe Integer, Integer, Integer, Integer, Integer )]
                        return $ oneStage res
                            where oneStage ((id, recipeid, stageType, temp, time) : _) = Just $ Stage id recipeid stageType temp time
                                  oneStager _ = Nothing

    list  pool = do
                    res <- fetchSimple pool "SELECT id, recipe, type, temp, time FROM stages" :: IO [(Maybe Integer, Integer, Integer, Integer, Integer)]
                    return $ map (\(id, recipeid, stageType, temp, time) -> Stage id recipeid stageType temp time) res



instance DbOperation StageCooking where
    insert pool (Just (StageCooking _ recipeid stageId stageType startTime endTime state)) = do
        res <- fetch pool (stageType, recipeid, stageId, startTime, endTime, state)
                            "INSERT INTO stage_recipes_cooking(type, recipe_id, stage_id, start_time, end_time, state) VALUES(?, ?, ?,?,?,?) RETURNING  id, type, recipe_id, stage_id, start_time, end_time, state" :: IO [(Maybe Integer, Integer, Integer, Integer, LocalTime,  LocalTime,  TL.Text)]
        return $ oneStage res
            where oneStage ((id, stageType, recipeid, stageId, startTime, endTime, state) : _) = Just $ StageCooking id recipeid stageId stageType startTime endTime state
                  oneStage _ = Nothing
    
    update pool (Just (StageCooking _ recipeid stageId stageType startTime endTime state)) id = do
        res <- fetch pool (stageType, recipeid, stageId, startTime, endTime, state, id)
                            "UPDATE stage_recipes_cooking SET type=?, recipe_id=?, stage_id=?, start_time=?, end_time=?, state=? WHERE id=? RETURNING  id, type, recipe_id, stage_id, start_time, end_time, state" :: IO [(Maybe Integer, Integer, Integer, Integer, LocalTime,  LocalTime,  TL.Text)]
        return $ oneStage res
            where oneStage ((id, stageType, recipeid, stageId, startTime, endTime, state) : _) = Just $ StageCooking id recipeid stageId stageType startTime endTime state
                  oneStage _ = Nothing

    find  pool id = do 
                        res <- fetch pool (Only id) "SELECT id, type, recipe_id, stage_id, start_time, end_time, state FROM stage_recipes_cooking WHERE id=?" :: IO [(Maybe Integer, Integer, Integer, Integer, LocalTime,  LocalTime,  TL.Text)]
                        return $ oneStage res
                            where oneStage ((id, stageType, recipeid, stageId, startTime, endTime, state) : _) = Just $ StageCooking id recipeid stageId stageType startTime endTime state
                                  oneStage _ = Nothing

    list  pool = do
                    res <- fetchSimple pool "SELECT id, type, recipe_id, stage_id, start_time, end_time, state FROM stage_recipes_cooking" :: IO [(Maybe Integer, Integer, Integer, Integer, LocalTime,  LocalTime,  TL.Text)]
                    return $ map (\(id, stageType, recipeid, stageId, startTime, endTime, state) -> StageCooking id recipeid stageId stageType startTime endTime state) res


deleteStageRecipeCooking :: Pool Connection -> TL.Text -> ActionT TL.Text IO ()
deleteStageRecipeCooking pool id = do 
                        _ <- liftIO $ execSqlT pool [id] "DELETE FROM stage_recipes_cooking WHERE recipe_id=?"
                        return ()

deleteStage :: Pool Connection -> TL.Text -> ActionT TL.Text IO ()
deleteStage pool id = do 
                        _ <- liftIO $ execSqlT pool [id] "DELETE FROM stages WHERE id=?"
                        return ()

deleteStageByRecipe :: Pool Connection -> TL.Text -> ActionT TL.Text IO ()
deleteStageByRecipe pool id = do 
                        _ <- liftIO $ execSqlT pool [id] "DELETE FROM stages WHERE recipe=?"
                        return ()
