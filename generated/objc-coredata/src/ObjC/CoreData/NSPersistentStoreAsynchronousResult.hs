{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPersistentStoreAsynchronousResult@.
module ObjC.CoreData.NSPersistentStoreAsynchronousResult
  ( NSPersistentStoreAsynchronousResult
  , IsNSPersistentStoreAsynchronousResult(..)
  , cancel
  , managedObjectContext
  , operationError
  , progress
  , cancelSelector
  , managedObjectContextSelector
  , operationErrorSelector
  , progressSelector


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

import ObjC.CoreData.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- cancel@
cancel :: IsNSPersistentStoreAsynchronousResult nsPersistentStoreAsynchronousResult => nsPersistentStoreAsynchronousResult -> IO ()
cancel nsPersistentStoreAsynchronousResult  =
  sendMsg nsPersistentStoreAsynchronousResult (mkSelector "cancel") retVoid []

-- | @- managedObjectContext@
managedObjectContext :: IsNSPersistentStoreAsynchronousResult nsPersistentStoreAsynchronousResult => nsPersistentStoreAsynchronousResult -> IO (Id NSManagedObjectContext)
managedObjectContext nsPersistentStoreAsynchronousResult  =
  sendMsg nsPersistentStoreAsynchronousResult (mkSelector "managedObjectContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- operationError@
operationError :: IsNSPersistentStoreAsynchronousResult nsPersistentStoreAsynchronousResult => nsPersistentStoreAsynchronousResult -> IO (Id NSError)
operationError nsPersistentStoreAsynchronousResult  =
  sendMsg nsPersistentStoreAsynchronousResult (mkSelector "operationError") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- progress@
progress :: IsNSPersistentStoreAsynchronousResult nsPersistentStoreAsynchronousResult => nsPersistentStoreAsynchronousResult -> IO (Id NSProgress)
progress nsPersistentStoreAsynchronousResult  =
  sendMsg nsPersistentStoreAsynchronousResult (mkSelector "progress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @managedObjectContext@
managedObjectContextSelector :: Selector
managedObjectContextSelector = mkSelector "managedObjectContext"

-- | @Selector@ for @operationError@
operationErrorSelector :: Selector
operationErrorSelector = mkSelector "operationError"

-- | @Selector@ for @progress@
progressSelector :: Selector
progressSelector = mkSelector "progress"

