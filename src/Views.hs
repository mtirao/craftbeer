{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views where

import Domain
import GHC.Generics
import Web.Scotty as WS
import Data.Monoid (mconcat)
import Data.Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Control.Monad.IO.Class
import Web.Scotty.Internal.Types

-- Class Response
class Response a where
    response :: Maybe a -> ActionM ()

-- Response Instances
instance Response User where
    response user = case user of
                        Just u -> json u
                        Nothing -> json (ErrorMessage "Something unexpected")

instance Response Stage where
    response stage = case stage of
                        Just u -> json u
                        Nothing -> json (ErrorMessage "Something unexpected")

instance Response Sensor where
    response sensor = case sensor of
                        Just u -> json u
                        Nothing -> json (ErrorMessage "Something unexpected")

instance Response Recipe where
    response recipe = case recipe of
                        Just u -> json u
                        Nothing -> json (ErrorMessage "Something unexpected")

instance Response Ingredient where
    response ingredient = case ingredient of
                        Just u -> json u
                        Nothing -> json (ErrorMessage "Something unexpected")

instance Response Agent where
    response agent = case agent of
                        Just u -> json u
                        Nothing -> json (ErrorMessage "Something unexpected")

--------------------------------------------------------------------------------
articlesList :: [Article] -> ActionM ()
articlesList articles = json articles

viewArticle :: Maybe Article -> ActionM ()
viewArticle Nothing = json ()
viewArticle (Just article) = json article

createdArticle :: Maybe Article -> ActionM ()
createdArticle article = json ()

updatedArticle :: Maybe Article -> ActionM ()
updatedArticle article = json ()

deletedArticle :: TL.Text -> ActionM ()
deletedArticle id = json ()

--------------------------------------------------------------------------------

createdUser :: Maybe User -> ActionM ()
createdUser user = case user of
                        Just u -> json u
                        Nothing -> json (ErrorMessage "Something unexpected")

--------------------------------------------------------------------------------
--createdObject :: Maybe DbModels -> ActionM ()
--createdObject dbModels = case dbModelse of
--                            Just u -> json u
--                            Nothin -> json (ErrorMessage "Something unexpected") 
