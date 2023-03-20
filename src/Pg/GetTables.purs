module Pg.GetTables
  ( ConnectionOpts
  , TableKeyed
  , TablesKeyed
  , getTables
  , getTablesKeyed
  , keyTables
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)

getTables :: ConnectionOpts -> Aff (Array Table)
getTables = getTablesImpl >>> Promise.toAffE

getTablesKeyed
  :: ConnectionOpts
  -> Aff TablesKeyed
getTablesKeyed opts = getTables opts <#> keyTables

keyTables :: Array Table -> TablesKeyed
keyTables tables = tables <#> (\t -> t { columns = keyArr _.name t.columns }) # keyArr _.name
  where
  keyArr :: forall k v. Ord k => (v -> k) -> Array v -> Map k v
  keyArr fn arr = Map.fromFoldable $ arr <#> \v -> Tuple (fn v) v

foreign import getTablesImpl :: ConnectionOptsJs -> Effect (Promise (Array Table))

type ConnectionOpts =
  { url :: String
  }

type ConnectionOptsJs =
  { url :: String
  }

type Table =
  { name :: String
  , columns :: Array Column
  }

type Column =
  { name :: String
  , nullable :: Boolean
  }

type TablesKeyed = Map String TableKeyed

type TableKeyed =
  { columns ::
      Map String
        { name :: String
        , nullable :: Boolean
        }
  , name :: String
  }