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

import Network.Wreq as NW
import Control.Lens
import Data.Aeson.Lens


data Login = Login 
    { username :: Text
      , password :: Text
    }

login = Login "fnisi@wannaplay.club" "3177AppL"


articlesList :: [Article] -> ActionM ()
articlesList articles = WS.json articles


createdArticle :: Maybe Article -> ActionM ()
createdArticle article = WS.json ()

updatedArticle :: Maybe Article -> ActionM ()
updatedArticle article = WS.json ()

deletedArticle :: TL.Text -> ActionM ()
deletedArticle id = WS.json ()
