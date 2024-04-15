{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Server(runServer) where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Postgresql

import           GHC.Generics
import           Servant
import           Network.Wai.Handler.Warp
import           Control.Monad.Logger (runStderrLoggingT)
import           Data.Aeson
import           Data.Text
import           Data.Maybe 
import           Models


-- Define your API
type API =
       "hello" :> Get '[PlainText] Text 
  :<|> "users" :> Get '[JSON] [Entity DbUser]  -- get all users
  :<|> "register" :> ReqBody '[JSON] UserRegistration :> Post '[PlainText] Text  -- register new user
  :<|> "login" :> ReqBody '[JSON] UserLogin :> Post '[PlainText] Text
  :<|> "logout" :> Delete '[PlainText] Text
  :<|> "movies" :> Get '[JSON] [Entity Movie]  -- get all Movies
  :<|> "movies" :> "add" :> ReqBody '[JSON] Movie :> Post '[JSON] (Entity Movie)  -- add new movie
  :<|> "movie" :> "edit" :> Capture "movieId" MovieId :> ReqBody '[JSON] PartialMovie :> Patch '[JSON] (Entity Movie)  -- edit movie
  :<|> "movie" :> "delete" :> Capture "movieId" MovieId :> Delete '[PlainText] Text  
 
-- DataType for editing movies
data PartialMovie = PartialMovie
    { partialMovieName :: Maybe String
    , partialMovieRating :: Maybe Int
    , partialMovieGenre :: Maybe String
    } deriving (Generic, FromJSON)

-- Define a type for user registration data
data UserRegistration = UserRegistration 
  { regUsername :: String 
  , regPassword :: String
  } deriving (Eq, Show, Generic, FromJSON)

-- Define a type for user login data
data UserLogin = UserLogin
  { loginUsername :: String
  , loginPassword :: String
  } deriving (Eq, Show, Generic, FromJSON)


-- Implement the handlers for your API

helloHandler :: Handler Text
helloHandler = return "Hello, world!"

getUsers :: ConnectionPool -> Handler [Entity DbUser] 
getUsers pool = liftIO $ runSqlPool (selectList [] []) pool

-- Define the handler for registering a new user
registerUserHandler :: ConnectionPool -> UserRegistration -> Handler Text
registerUserHandler pool userReg = do
    -- Check if there's an active user
    activeUser <- liftIO $ runSqlPool (selectFirst [] []) pool :: Handler (Maybe (Entity ActiveUser))
    case activeUser of
        Just _ -> throwError err403 { errBody = "Registration is not allowed as there is an active user." }
        Nothing -> do
            let newUser = DbUser
                    { dbUserUsername = regUsername userReg 
                    , dbUserPassword = regPassword userReg 
                    , dbUserFavouriteMovies = [] -- You may want to initialize this with an empty list or provide default values
                    }
            -- Insert the new user into the database
            _ <- liftIO $ runSqlPool (insert_ newUser) pool
            -- Add the user to the ActiveUser table
            _ <- liftIO $ runSqlPool (insert $ ActiveUser $ regUsername userReg) pool
            return "User registered successfully."

-- Define the handler for user login
loginHandler :: ConnectionPool -> UserLogin -> Handler Text
loginHandler pool (UserLogin username password) = do
    -- Check if ActiveUser table is not empty
    activeUser <- liftIO $ runSqlPool (selectFirst [] []) pool :: Handler (Maybe (Entity ActiveUser))
    case activeUser of
        Just _ -> throwError err403 { errBody = "Login is not allowed as there is an active user." }
        Nothing -> do
            -- Check if the user exists in the database
            maybeUser <- liftIO $ runSqlPool (selectFirst [DbUserUsername ==. username] []) pool
            case maybeUser of
                Nothing -> throwError err404 { errBody = "User not found" }
                Just (Entity userId user) -> do
                    -- Check if the password matches
                    if dbUserPassword user == password
                        then do
                            -- Add the user to ActiveUser table
                            _ <- liftIO $ runSqlPool (insert_ (ActiveUser username)) pool
                            return $ "Login successful for user: " <> pack username
                        else throwError err401 { errBody = "Incorrect password" }

logoutHandler :: ConnectionPool -> Handler Text
logoutHandler pool = do
    -- Delete all entries from the ActiveUser table
    _ <- liftIO $ runSqlPool (deleteWhere ([] :: [Filter ActiveUser])) pool
    return "Logged out successfully. All active users removed."

getMovies :: ConnectionPool -> Handler [Entity Movie]
getMovies pool = liftIO $ runSqlPool (selectList [] []) pool

addMovie :: ConnectionPool -> Movie -> Handler (Entity Movie)
addMovie pool movie = do
    movieId <- liftIO $ runSqlPool (insert movie) pool
    return $ Entity movieId movie 

editMovieHandler :: ConnectionPool -> MovieId -> PartialMovie -> Handler (Entity Movie)
editMovieHandler pool movieId partialMovie = do
    -- Fetch the movie from the database
    maybeMovie <- liftIO $ runSqlPool (get movieId) pool
    case maybeMovie of
        Nothing -> throwError err404  -- Movie not found
        Just oldMovie -> do
            -- Extract the Entity Movie from oldMovie
            let entityOldMovie = Entity movieId oldMovie
            -- Update the movie properties with the new values from the partial movie
            let updatedMovie = oldMovie
                    { movieName = fromMaybe (movieName oldMovie) (partialMovieName partialMovie)
                    , movieRatng = fromMaybe (movieRatng oldMovie) (partialMovieRating partialMovie)
                    , movieGenre = fromMaybe (movieGenre oldMovie) (partialMovieGenre partialMovie)
                    }
            -- Save the updated movie back to the database
            _ <- liftIO $ runSqlPool (Database.Persist.Postgresql.replace movieId updatedMovie) pool
            -- Return the updated movie entity in the response
            return $ Entity movieId updatedMovie

deleteMovieHandler :: ConnectionPool -> MovieId -> Handler Text
deleteMovieHandler pool movieId = do
    --delete movie
    _ <- liftIO $ runSqlPool (delete movieId) pool
    return "Movie Deleted Succesfully"


server :: ConnectionPool -> Server API
server pool = helloHandler 
          :<|> getUsers pool 
          :<|> registerUserHandler pool
          :<|> loginHandler pool
          :<|> logoutHandler pool
          :<|> getMovies pool 
          :<|> addMovie pool 
          :<|> editMovieHandler pool 
          :<|> deleteMovieHandler pool

-- Define your application
app :: ConnectionPool -> Application
app pool = serve api (server pool)
  where
    api :: Proxy API
    api = Proxy

runServer :: IO ()
runServer = do
    -- Set up a connection pool to your PostgreSQL database
    pool <- runStderrLoggingT $ createPostgresqlPool
              "dbname=mydatabase user=dushyant.singh host=localhost port=5432"
              10

    -- Run migrations to create database tables (if necessary)
    runSqlPool (runMigration migrateAll) pool

    -- Start the Warp web server
    run 8080 (app pool)
