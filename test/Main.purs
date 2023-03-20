module Test.Main (main) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.GraphQL.AST as AST
import Data.GraphQL.AST.Print (printAst)
import Data.GraphQL.Parser (document)
import Data.Lens (over, preview, prism', traversed, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Profunctor.Choice (class Choice)
import Data.Tuple (Tuple, uncurry)
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, Error, error, throwError)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Gql.Lens (inputObjectTypeDefinitionLens)
import Hasura.FixNullInserts (fixNullInsertsOnAst)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Parsing (runParser)
import Pg.GetTables (TablesKeyed, getTablesKeyed, keyTables)

main :: Effect Unit
main = launchAff_ do
  schemaSrc <- readTextFile UTF8 "./test/schema.graphql"
  doc <- rethrow $ runParser schemaSrc document 
  let
     transformed = fixNullInsertsOnAst testTables doc
    --  inputs :: List AST.InputObjectTypeDefinition
    --  inputs = view (?D inputObjectTypeDefinitionLens) doc
  -- traceM $ printAst transformed 

  writeTextFile UTF8 "./test/transformed.graphql" $ printAst transformed
  pure unit

testTables :: TablesKeyed
testTables = keyTables
  [ { name: "submissions_with_archived"
    , columns:
        [ { name: "event_id"
          , nullable: false
          }
        ]
    }
  ]

rethrow :: forall m a err. MonadThrow Error m => Show err => Either err a -> m a
rethrow = case _ of
  Left err -> throwError $ error $ show err
  Right a -> pure a