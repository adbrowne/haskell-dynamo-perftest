{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty

import Data.Monoid (mconcat)
-- import Lib
import Network.AWS.DynamoDB.GetItem
-- import Network.AWS
import            Data.Text                (Text)
import qualified Data.Text                as Text
import Data.List.NonEmpty
import qualified Data.HashMap.Strict as HashMap
import Data.Text.Lazy (fromStrict)
import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson


-- main = do
--   env <- getEnv Sydney Discover
--   rs <- send env (getItem tableName ("id" :| []) :: GetItem)
--   print rs
--  scotty 3000 $ do
--    get "/:id" $ do
--        id <- param "id"
--       let gi = getItem tableName 
--       resp <- send gi
--        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

--import Network.AWS     -- amazonka

import Network.AWS
import Network.AWS.DynamoDB
import System.IO
import Control.Monad.Trans.Resource

tableName = "haskell-perf-baseline"
getMyItem :: Text -> IO (Maybe Text)
getMyItem myid = do
    env <- newEnv Sydney (FromEnv "AWS_ACCESS_KEY_ID" "AWS_SECRET_ACCESS_KEY")
    runResourceT $ runAWS env $ do
        let attrValue = attributeValue & avS  .~ (Just myid)
        let req = (getItem tableName :: GetItem) & giKey .~ (HashMap.singleton "id" attrValue)
        resp <- send req
        let values = HashMap.lookup "S" $ resp ^. girsItem
        return (values >>= \v -> v ^. avS)

main :: IO ()
main = do
  scotty 3000 $ do
    get "/:id" $ do
        --id <- param "id"
        --result <- liftIO $ getMyItem id
        case (Just "fine") of
          Just t -> html $ mconcat ["<h1>Scotty, ", fromStrict t, " me up!</h1>"]
          Nothing -> html "Not found"
