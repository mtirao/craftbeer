{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views where

import Domain
import GHC.Generics()
import Web.Scotty as WS
import Data.Monoid()
import Data.Text()
import Data.Aeson
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class()
import Web.Scotty.Internal.Types()


-- Class Response
class Response a where
    response :: Maybe a -> ActionM ()

-- Response Instances
instance Response User where
    response user = case user of
                        Just u -> WS.json u
                        Nothing -> WS.json (ErrorMessage "Something unexpected")

instance Response Stage where
    response stage = case stage of
                        Just u -> WS.json u
                        Nothing -> WS.json (ErrorMessage "Something unexpected")

instance Response Sensor where
    response sensor = case sensor of
                        Just u -> WS.json u
                        Nothing -> WS.json (ErrorMessage "Something unexpected")

instance Response Recipe where
    response recipe = case recipe of
                        Just u -> WS.json u
                        Nothing -> WS.json (ErrorMessage "Something unexpected")

instance Response Ingredient where
    response ingredient = case ingredient of
                        Just u -> WS.json u
                        Nothing -> WS.json (ErrorMessage "Something unexpected")

instance Response Agent where
    response agent = case agent of
                        Just u -> WS.json u
                        Nothing -> WS.json (ErrorMessage "Something unexpected")


jsonResponse :: ToJSON a => a -> ActionM ()
jsonResponse e = WS.json e

--------------------------------------------------------------------------------
articlesList :: [Article] -> ActionM ()
articlesList articles = WS.json articles

viewArticle :: Maybe Article -> ActionM ()
viewArticle Nothing = WS.json ()
viewArticle (Just article) = WS.json article

createdArticle :: Maybe Article -> ActionM ()
createdArticle _ = WS.json ()

updatedArticle :: Maybe Article -> ActionM ()
updatedArticle _ = WS.json ()

deletedArticle :: TL.Text -> ActionM ()
deletedArticle _ = WS.json ()

--------------------------------------------------------------------------------

createdUser :: Maybe User -> ActionM ()
createdUser user = case user of
                        Just u -> WS.json u
                        Nothing -> WS.json (ErrorMessage "Something unexpected")

--------------------------------------------------------------------------------
--createdObject :: Maybe DbModels -> ActionM ()
--createdObject dbModels = case dbModelse of
--                            Just u -> json u
--                            Nothin -> json (ErrorMessage "Something unexpected") 
