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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Models where
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           GHC.Generics
import           Data.Aeson
import           Data.Text


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DbUser
    username String
    password String
    favouriteMovies [MovieId]
    UniqueUsername username
    deriving Show Generic

Movie
    name String
    ratng Int
    genre String
    deriving Show Generic

ActiveUser
    username String
    UniqueActiveUsername username
|]

-- Define a ToJSON instance for User
instance ToJSON DbUser
-- instance ToJSON User where
--   toJSON (User name age email favouriteMovies) = object ["name" .= name, "age" .= age, "email" .= email, "favouriteMovies" .= favouriteMovies]

-- Define a ToJSON instance for Entity User
-- instance ToJSON (Entity User)
-- aeson doesnt know how to handle entity wrapper
--entity wrapper contains id and user
instance ToJSON (Entity DbUser) where
  toJSON (Entity userId user) = object ["id" .= userId, "user" .= user]

-- Define a ToJSON instance for Movie
instance ToJSON Movie
-- instance ToJSON Movie where
--   toJSON (Movie name rating genre) = object ["name" .= name, "rating" .= rating, "genre" .= genre]

-- Define a ToJSON instance for Entity Movie
-- instance ToJSON (Entity Movie)
instance ToJSON (Entity Movie) where
  toJSON (Entity movieId movie) = object ["id" .= movieId, "movie" .= movie]

-- Define a FromJSON instance for User
instance FromJSON DbUser
-- instance FromJSON User where
--   parseJSON = withObject "User" $ \v -> User
--     <$> v .: "name"
--     <*> v .:? "age"
--     <*> v .: "email"
--     <*> v .: "favouriteMovies"

-- Define a FromJSON instance for Movie
instance FromJSON Movie
-- instance FromJSON Movie where
--   parseJSON = withObject "Movie" $ \v -> Movie
--     <$> v .: "name"
--     <*> v .: "rating"
--     <*> v .: "genre"