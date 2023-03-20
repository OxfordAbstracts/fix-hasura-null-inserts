module Main where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut (class DecodeJson, decodeJson, parseJson, printJsonDecodeError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Dotenv (loadFile)
import Effect (Effect)
import Effect.Aff (Error, error, launchAff_, throwError)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign.Object as Object
import Hasura.FixNullInserts (fixNullInserts)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Node.Process (lookupEnv)

main :: Effect Unit
main = launchAff_ do
  void $ loadFile
  database_url <- getEnv "DATABASE_URL"
  gql_url <- getEnv "GQL_URL"
  gql_headers <- fromMaybe Object.empty <$> getEnvMbDecode "GQL_HEADERS"
  out_path <- liftEffect $ fromMaybe "./transformed.gql" <$> lookupEnv "OUT_PATH"
  transformed <- fixNullInserts { database_url, gql_url, gql_headers }
  writeTextFile UTF8 out_path transformed
  where
  getEnv key = liftEffect $ lookupEnv key >>= case _ of
    Just value -> pure value
    Nothing -> throwError $ error $ "Environment variable \"" <> key <> "\" not found"

getEnvMbDecode :: forall m a. MonadEffect m => DecodeJson a => String -> m (Maybe a)
getEnvMbDecode key = liftEffect do
  valueMb <- lookupEnv key
  case valueMb of
    Nothing -> pure Nothing
    Just value -> case decodeJson =<< parseJson value of
      Left err -> throwError $ error $ printJsonDecodeError err
      Right a -> pure $ Just a

rethrow :: forall m a err. MonadThrow Error m => Show err => Either err a -> m a
rethrow = case _ of
  Left err -> throwError $ error $ show err
  Right a -> pure a