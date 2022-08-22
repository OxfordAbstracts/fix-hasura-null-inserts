module Hasura.FixNullInserts (fixNullInserts, fixNullInsertsOnAst) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.GraphQL.AST as AST
import Data.GraphQL.AST.Print (printAst)
import Data.GraphQL.Parser (document)
import Data.Lens (over, prism', traversed)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Profunctor.Choice (class Choice)
import Data.String (Pattern(..), stripPrefix, trim)
import Data.String.CodeUnits as String
import Data.Tuple (Tuple, uncurry)
import Effect.Aff (Aff, Error, error, throwError)
import Parsing (runParser)
import Pg.GetTables (TablesKeyed, TableKeyed, getTablesKeyed)

fixNullInserts
  :: { database_url :: String
     , gql_schema :: String
     , tables_yaml_path :: Maybe String
     }
  -> Aff String
fixNullInserts { database_url, gql_schema } = do
  tables <- getTablesKeyed { url: database_url }
  doc <- rethrow $ runParser gql_schema document
  let transformedDoc = fixNullInsertsOnAst tables doc
  pure $ printAst transformedDoc

fixNullInsertsOnAst :: TablesKeyed -> AST.Document -> AST.Document
fixNullInsertsOnAst tables = over lens transformInputObjectTypeDefinition
  where
  lens = uPrism AST._Document
    <<< traversed
    <<< uPrism AST._Definition_TypeSystemDefinition
    <<< uPrism AST._TypeSystemDefinition_TypeDefinition
    <<< uPrism AST._TypeDefinition_InputObjectTypeDefinition
    <<< _Newtype

  transformInputObjectTypeDefinition :: AST.T_InputObjectTypeDefinition -> AST.T_InputObjectTypeDefinition
  transformInputObjectTypeDefinition = case _ of
    i@{ description: Just description } | Just table <- lookupTable =<< getPgTableName description ->
      i { inputFieldsDefinition = over inputLens (transformInputFieldDefinition table) i.inputFieldsDefinition }
      where
      inputLens =
        traversed
          <<< uPrism AST._InputFieldsDefinition
          <<< traversed
          <<< _Newtype

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
    true, AST.Type_NonNullType (AST.NonNullType_NamedType t) -> AST.Type_NamedType t
    true, AST.Type_NonNullType (AST.NonNullType_ListType t) -> AST.Type_ListType t
    false, AST.Type_NamedType t -> AST.Type_NonNullType (AST.NonNullType_NamedType t)
    false, AST.Type_ListType t -> AST.Type_NonNullType (AST.NonNullType_ListType t)
    _, t -> t

uPrism :: forall s a c. Tuple (a -> s) (s -> Maybe a) -> (Choice c => c a a -> c s s)
uPrism = uncurry prism'

rethrow :: forall t8 t9 a12. MonadThrow Error t8 => Show a12 => Either a12 t9 -> t8 t9
rethrow = case _ of
  Left err -> throwError $ error $ show err
  Right a -> pure a