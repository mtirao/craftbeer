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



articlesList :: [Article] -> ActionM ()
articlesList articles = json articles

viewArticle :: Maybe Article -> ActionM ()
viewArticle Nothing = json ()
viewArticle (Just article) = json article

createdArticle :: Maybe Article -> ActionM ()
createdArticle article = json ()

createdUser :: Maybe User -> ActionM ()
createdUser user = case user of
                        Just u -> json u
                        Nothing -> json (ErrorMessage "Something unexpected")


updatedArticle :: Maybe Article -> ActionM ()
updatedArticle article = json ()

deletedArticle :: TL.Text -> ActionM ()
deletedArticle id = json ()
