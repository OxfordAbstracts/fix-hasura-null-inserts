module Gql.Lens where

import Prelude

import Data.GraphQL.AST as AST
import Data.Lens (class Wander, prism', traversed)
import Data.Maybe (Maybe)
import Data.Profunctor.Choice (class Choice)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple, uncurry)

inputObjectTypeDefinitionLens :: forall c. Choice c => Wander c => c AST.InputObjectTypeDefinition AST.InputObjectTypeDefinition -> c AST.Document AST.Document
inputObjectTypeDefinitionLens = uPrism AST._Document
  <<< traversed
  <<< uPrism AST._Definition_TypeSystemDefinition
  <<< uPrism AST._TypeSystemDefinition_TypeDefinition
  <<< uPrism AST._TypeDefinition_InputObjectTypeDefinition

inputFieldsLens :: forall l w. Traversable l => Wander w => w AST.InputValueDefinition AST.InputValueDefinition -> w (l AST.InputFieldsDefinition) (l AST.InputFieldsDefinition)
inputFieldsLens =
  traversed
    <<< uPrism AST._InputFieldsDefinition
    <<< traversed

uPrism :: forall s a c. Tuple (a -> s) (s -> Maybe a) -> (Choice c => c a a -> c s s)
uPrism = uncurry prism'