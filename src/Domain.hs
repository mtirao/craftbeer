{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}

module Domain where

import Data.Text
import Data.Aeson
import Control.Applicative

data Article = Article Integer Text Text -- id title bodyText
     deriving (Show)

-- Json instance definition
instance FromJSON Article where
     parseJSON (Object v) = Article <$>
                            v .:? "id" .!= 0 <*> -- the field "id" is optional
                            v .:  "title"    <*>
                            v .:  "bodyText"

instance ToJSON Article where
     toJSON (Article id title bodyText) =
         object ["id" .= id,
                 "title" .= title,
                 "bodyText" .= bodyText]

-- Data Detail definition
data Detail = Detail Integer Integer Integer Integer -- id recipe_id ingredient_id quantity
     deriving (Show)

-- Json instance definition
instance FromJSON Detail where
     parseJSON (Object v) = Detail <$>
                            v .:? "id" .!= 0 <*> 
                            v .:  "recipe_id"    <*>
                            v .:  "ingredient_id" <*>
                            v .:  "quantity"

instance ToJSON Detail where
     toJSON (Detail id recipe_id ingredient_id quantity) =
         object ["id" .= id,
                 "recipe_id" .= recipe_id,
                 "ingredient_id" .= ingredient_id ,
                 "quantity" .=  quantity]


-- Data Ingredient definition
data Ingredient = Ingredient Integer Text Text Text Integer -- id ident name type unit
     deriving (Show)

-- Json instance definition
instance FromJSON Ingredient where
     parseJSON (Object v) = Ingredient <$>
                            v .:? "id" .!= 0 <*> 
                            v .:  "ident"    <*>
                            v .:  "name" <*>
                            v .:  "type" <*>
                            v .: "unit"

instance ToJSON Ingredient where
     toJSON (Ingredient id ident name type_b unit) =
         object ["id" .= id,
                 "ident" .= ident,
                 "name" .= name ,
                 "type" .=  type_b,
                 "unit" .= unit]

-- Data Recipe definition
data Recipe = Recipe Integer Text Text Text Integer Integer Integer -- id ident stype name ibu abv color
     deriving (Show)

-- Data Stage definition
data Stage = Stage Integer Integer Integer Integer Integer -- id recipe_id type temp time
     deriving (Show)

-- Data User definition
data User = User Integer Text Text Text Text -- id ident password role email
     deriving (Show)


                 
