module Hasura.FixNullInserts (fixNullInserts, fixNullInsertsOnAst) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.GraphQL.AST as AST
import Data.GraphQL.AST.Print (printAst)
import Data.GraphQL.Parser (document)
import Data.Lens (over)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), stripPrefix, trim)
import Data.String.CodeUnits as String
import Effect.Aff (Aff, Error, error, throwError)
import Foreign.Object (Object)
import Gql.Lens (inputFieldsLens, inputObjectTypeDefinitionLens)
import Hasura.GetSchema (getGqlDoc)
import Parsing (runParser)
import Pg.GetTables (TablesKeyed, TableKeyed, getTablesKeyed)

fixNullInserts
  :: { database_url :: String
     , gql_url :: String
     , gql_headers :: Object String
     }
  -> Aff String
fixNullInserts { database_url, gql_url, gql_headers } = do
  tables <- getTablesKeyed { url: database_url }
  doc <- getGqlDoc gql_url gql_headers
  let transformedDoc = fixNullInsertsOnAst tables doc
  pure $ printAst transformedDoc

fixNullInsertsOnAst :: TablesKeyed -> AST.Document -> AST.Document
fixNullInsertsOnAst tables = over (inputObjectTypeDefinitionLens <<< _Newtype) transformInputObjectTypeDefinition
  where

  transformInputObjectTypeDefinition :: AST.T_InputObjectTypeDefinition -> AST.T_InputObjectTypeDefinition
  transformInputObjectTypeDefinition = case _ of
    i@{ description: Just description } | Just table <- lookupTable =<< getPgTableName description ->
      i { inputFieldsDefinition = over (inputFieldsLens <<< _Newtype) (transformInputFieldDefinition table) i.inputFieldsDefinition }

    i -> i

  getPgTableName description =
    trim description
      # stripPrefix (Pattern "input type for inserting data into table \"")
      <#> String.takeWhile (_ /= '"')

  lookupTable name = Map.lookup name tables

  transformInputFieldDefinition :: TableKeyed -> AST.T_InputValueDefinition -> AST.T_InputValueDefinition
  transformInputFieldDefinition pgTable = case _ of
    i@{ name } | Just { nullable } <- Map.lookup name pgTable.columns ->
      i { type = setNullable nullable i.type }
    i -> i

  setNullable :: Boolean -> AST.Type -> AST.Type
  setNullable = case _, _ of
    false, AST.Type_NamedType t -> AST.Type_NonNullType (AST.NonNullType_NamedType t)
    false, AST.Type_ListType t -> AST.Type_NonNullType (AST.NonNullType_ListType t)
    _, t -> t

