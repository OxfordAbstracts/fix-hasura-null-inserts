module Pg.GetTables where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Effect (Effect)
import Effect.Aff (Aff)

getTables :: ConnectionOpts -> Aff (Array Table)
getTables = getTablesImpl >>> Promise.toAffE

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