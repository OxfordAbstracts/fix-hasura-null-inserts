module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Debug (traceM)
import Dotenv (loadFile)
import Effect (Effect)
import Effect.Aff (error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Process (lookupEnv)
import Pg.GetTables (getTables)

main :: Effect Unit
main = launchAff_ do
  void $ loadFile
  database_url <- getEnv "DATABASE_URL"
  tables <- getTables { url: database_url }
  traceM { tables: show tables }
  pure unit
  where
  getEnv key = liftEffect $ lookupEnv key >>= case _ of
    Just value -> pure value
    Nothing -> throwError $ error $ "Environment variable " <> key <> " not found"

