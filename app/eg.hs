


{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           GHC.Generics
import           Servant
import           Network.Wai.Handler.Warp
import           Control.Monad.Logger (runStderrLoggingT)
import           Data.Aeson
import           Data.Text


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name String
    age Int Maybe
    deriving Show Generic
|]

-- Define a ToJSON instance for User
instance ToJSON User where
  toJSON (User name age) = object ["name" .= name, "age" .= age]

-- Define a ToJSON instance for Entity User
instance ToJSON (Entity User) where
  toJSON (Entity userId user) = object ["id" .= userId, "user" .= user]

-- Define your API
type API =
       "hello" :> Get '[PlainText] Text  -- Endpoint for /hello

  :<|> "users" :> Get '[JSON] [Entity User]  -- Existing endpoint for /users

-- Implement the handlers for your API
server :: ConnectionPool -> Server API
server pool = helloHandler :<|> getUsers
  where
    helloHandler :: Handler Text
    helloHandler = return "Hello, world!"

    getUsers :: Handler [Entity User]
    getUsers = liftIO $ runSqlPool (selectList [] []) pool

-- Define your application
app :: ConnectionPool -> Application
app pool = serve api (server pool)
  where
    api :: Proxy API
    api = Proxy

-- Start your Servant application
main :: IO ()
main = do
    -- Set up a connection pool to your PostgreSQL database
    pool <- runStderrLoggingT $ createPostgresqlPool
              "dbname=mydatabase user=dushyant.singh host=localhost port=5432"
              10

    -- Run migrations to create database tables (if necessary)
    runSqlPool (runMigration migrateAll) pool

    -- Start the Warp web server
    run 8082 (app pool)
