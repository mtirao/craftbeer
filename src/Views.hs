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
