{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}


module Domain where

import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Applicative
import GHC.Generics
import Data.Time.LocalTime

-- Request Json
-- Error

-- Login
data Login = Login Text Text -- username password
    deriving (Show)

instance ToJSON Login where
    toJSON (Login username password) = object
        [
            "username" .= username,
            "password" .= password
        ]

instance FromJSON Login where
    parseJSON (Object v) = Login <$>
        v .:  "username" <*>
        v .:  "password"

-- User
data User = User Text Text Text  Text Text -- password user name lastname role
     deriving (Show)

instance ToJSON User where
    toJSON (User password user name lastname role) = object
        [
            "username" .= user,
            "password" .= password,
            "name" .= name,
            "lastname" .= lastname,
            "role" .= role
        ]

instance FromJSON User where
    parseJSON (Object v) = User <$>
        v .:  "username" <*>
        v .:  "password" <*>
        v .:  "name" <*>
        v .:  "lastname" <*>
        v .:  "role"

-- Stages
data Stage = Stage{
    stageId:: Maybe Integer
    ,belong_to :: Integer
    ,stage_type :: Integer
    ,temp :: Integer
    ,time :: Integer
} deriving (Show)

instance ToJSON Stage where
    toJSON (Stage stageId belong_to stage_type temp time) = object
        [
            "id" .= stageId,
            "recipe" .= belong_to,
            "type" .= stage_type,
            "temp" .= temp,
            "time" .= time
        ]

instance FromJSON Stage where
    parseJSON (Object v) = Stage <$>
        v .:?  "id" <*>
        v .:  "recipe" <*>
        v .:  "type" <*>
        v .:  "temp" <*>
        v .:  "time"


-- Stage Cooking
data StageCooking = StageCooking{
    stageCookingId:: Maybe Integer
    ,stage_belong_to :: Integer
    ,stage_recipe_id :: Integer
    ,stage_cooking_type :: Integer
    ,start_time :: LocalTime
    ,end_time :: LocalTime
    ,state :: Text
} deriving (Show)

instance ToJSON StageCooking where
    toJSON (StageCooking stageId belong_to stage_id stage_type start_time end_time state) = object
        [
            "id" .= stageId,
            "recipe" .= belong_to,
            "stage_id" .= stage_id,
            "type" .= stage_type,
            "start_time" .= start_time,
            "end_time" .= end_time,
            "state" .= state
        ]

instance FromJSON StageCooking where
    parseJSON (Object v) = StageCooking <$>
        v .:?  "id" <*>
        v .:  "recipe" <*>
        v .:  "stage_id" <*>
        v .:  "type" <*>
        v .:  "start_time" <*>
        v .:  "end_time" <*>
        v .:  "state"

-- Sensosrs
data Sensor = Sensor {
    sensorId :: Maybe Integer
    ,sensor_type :: Text
    ,name :: Text
    ,file :: Text
} deriving (Show, Generic)

instance ToJSON Sensor
instance FromJSON Sensor

-- Recipes
data Recipe = Recipe {
    recipeId :: Maybe Integer
    ,style :: Text
    ,recipe_name :: Text
    ,ibu :: Integer
    ,abv :: Integer
    ,color :: Integer
} deriving (Show, Generic)


instance FromJSON Recipe where
    parseJSON (Object v) = Recipe <$>
        v .:?  "id" <*>
        v .:  "style" <*>
        v .:  "name" <*>
        v .:  "ibu" <*>
        v .:  "abv" <*>
        v .:  "color"

instance ToJSON Recipe where
    toJSON Recipe {..} = object 
        [
            "id" .= recipeId
            ,"style" .= style
            ,"name" .= recipe_name
            ,"ibu" .= ibu
            ,"abv" .= abv
            ,"color" .= color
        ]

-- Recipes Cooking
data RecipeCooking = RecipeCooking {
    recipe_id :: Maybe Integer
    ,recipe_cooking_id :: Integer
    ,recipe_state :: Text
} deriving (Show, Generic)


instance FromJSON RecipeCooking where
    parseJSON (Object v) = RecipeCooking <$>
        v .:?  "id" <*>
        v .:  "recipe" <*>
        v .:  "state"

instance ToJSON RecipeCooking where
    toJSON RecipeCooking {..} = object 
        [
            "id" .= recipe_id
            ,"recipe" .= recipe_cooking_id
            ,"state" .= recipe_state
        ]

-- Ingredient
data Ingredient = Ingredient {
    ingredientId:: Maybe Integer
    ,recipe :: Integer
    ,ingredient_name :: Text
    ,ingredient_type :: Text
    ,unit :: Integer
    ,value :: Integer
} deriving (Show, Generic)

instance FromJSON Ingredient where
    parseJSON (Object v) = Ingredient <$>
        v .:?  "id" <*>
        v .:  "recipe" <*>
        v .:  "name" <*>
        v .:  "type" <*>
        v .:  "unit" <*>
        v .:  "value"

instance ToJSON Ingredient where
    toJSON Ingredient {..} = object 
        [
            "id" .= ingredientId
            ,"recipe" .= recipe
            ,"name" .= ingredient_name
            ,"type" .= ingredient_type
            ,"unit" .= unit
            ,"value" .= value
        ]

-- Agents
data Agent = Agent{
    agentId:: Maybe Integer
    ,agent_type :: Text
    ,ip :: Text
} deriving (Show, Generic)

instance FromJSON Agent
instance ToJSON Agent

--ErrorMessage
data ErrorMessage = ErrorMessage Text
    deriving (Show)

instance ToJSON ErrorMessage where
    toJSON (ErrorMessage message) = object
        [
            "error" .= message
        ]

--UserResponse
data UserResponse = UserResponse Text Text Text
     deriving (Show)

instance ToJSON UserResponse where 
    toJSON (UserResponse name lastname role) = object
        [
            "name" .= name,
            "lastname" .= lastname,
            "role" .= role
        ]

-- Getters
getUserName :: Maybe Login -> Text
getUserName a = case a of
                Nothing -> ""
                Just (Login u p) -> u   

getPassword :: Maybe Login -> Text
getPassword a = case a of
                Nothing -> ""
                Just (Login u p) -> p