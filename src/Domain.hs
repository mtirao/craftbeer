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

data Detail = Detail Integer Integer Integer Integer -- id recipe_id ingredient_id quantity
     deriving (Show)

data Ingredient = Ingredient Integer Text Text Text Integer -- id ident name type unit
     deriving (Show)

data Recipe = Recipe Integer Text Text Text Integer Integer Integer -- id ident stype name ibu abv color
     deriving (Show)

data Stage = Stage Integer Integer Integer Integer Integer -- id recipe_id type temp time
     deriving (Show)

data User = User Integer Text Text Text Text -- id ident password role email
     deriving (Show)

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
                 
