module Test.Main (main) where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.GraphQL.AST as AST
import Data.GraphQL.AST.Print (printAst)
import Data.GraphQL.Parser (document)
import Data.Lens (over, prism', traversed)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe)
import Data.Profunctor.Choice (class Choice)
import Data.Tuple (Tuple, uncurry)
import Effect.Aff (Aff, Error, error, throwError)
import Parsing (runParser)
import Pg.GetTables (TablesKeyed, getTablesKeyed)


main :: Effect Unit
main = launchAff_ do
  schema <- readTextFile UTF8 "./test/schema.graphql"

  pure unit
