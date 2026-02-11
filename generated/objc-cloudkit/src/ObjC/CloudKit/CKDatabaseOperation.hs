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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.CoreData.Internal.Classes
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
database ckDatabaseOperation  =
  sendMsg ckDatabaseOperation (mkSelector "database") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setDatabase ckDatabaseOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckDatabaseOperation (mkSelector "setDatabase:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @database@
databaseSelector :: Selector
databaseSelector = mkSelector "database"

-- | @Selector@ for @setDatabase:@
setDatabaseSelector :: Selector
setDatabaseSelector = mkSelector "setDatabase:"

