{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKDatabaseOperation@.
module ObjC.CloudKit.CKDatabaseOperation
  ( CKDatabaseOperation
  , IsCKDatabaseOperation(..)
  , database
  , setDatabase
  , databaseSelector
  , setDatabaseSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The database on which to perform the operation.
--
-- If no database is set,
--
-- [self.container privateCloudDatabase]
--
-- is used.  This will also set the container property of the operation's configuration to match the container of the passed-in database.
--
-- ObjC selector: @- database@
database :: IsCKDatabaseOperation ckDatabaseOperation => ckDatabaseOperation -> IO (Id CKDatabase)
database ckDatabaseOperation =
  sendMessage ckDatabaseOperation databaseSelector

-- | The database on which to perform the operation.
--
-- If no database is set,
--
-- [self.container privateCloudDatabase]
--
-- is used.  This will also set the container property of the operation's configuration to match the container of the passed-in database.
--
-- ObjC selector: @- setDatabase:@
setDatabase :: (IsCKDatabaseOperation ckDatabaseOperation, IsCKDatabase value) => ckDatabaseOperation -> value -> IO ()
setDatabase ckDatabaseOperation value =
  sendMessage ckDatabaseOperation setDatabaseSelector (toCKDatabase value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @database@
databaseSelector :: Selector '[] (Id CKDatabase)
databaseSelector = mkSelector "database"

-- | @Selector@ for @setDatabase:@
setDatabaseSelector :: Selector '[Id CKDatabase] ()
setDatabaseSelector = mkSelector "setDatabase:"

