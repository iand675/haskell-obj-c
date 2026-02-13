{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- cancel@
cancel :: IsNSPersistentStoreAsynchronousResult nsPersistentStoreAsynchronousResult => nsPersistentStoreAsynchronousResult -> IO ()
cancel nsPersistentStoreAsynchronousResult =
  sendMessage nsPersistentStoreAsynchronousResult cancelSelector

-- | @- managedObjectContext@
managedObjectContext :: IsNSPersistentStoreAsynchronousResult nsPersistentStoreAsynchronousResult => nsPersistentStoreAsynchronousResult -> IO (Id NSManagedObjectContext)
managedObjectContext nsPersistentStoreAsynchronousResult =
  sendMessage nsPersistentStoreAsynchronousResult managedObjectContextSelector

-- | @- operationError@
operationError :: IsNSPersistentStoreAsynchronousResult nsPersistentStoreAsynchronousResult => nsPersistentStoreAsynchronousResult -> IO (Id NSError)
operationError nsPersistentStoreAsynchronousResult =
  sendMessage nsPersistentStoreAsynchronousResult operationErrorSelector

-- | @- progress@
progress :: IsNSPersistentStoreAsynchronousResult nsPersistentStoreAsynchronousResult => nsPersistentStoreAsynchronousResult -> IO (Id NSProgress)
progress nsPersistentStoreAsynchronousResult =
  sendMessage nsPersistentStoreAsynchronousResult progressSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @managedObjectContext@
managedObjectContextSelector :: Selector '[] (Id NSManagedObjectContext)
managedObjectContextSelector = mkSelector "managedObjectContext"

-- | @Selector@ for @operationError@
operationErrorSelector :: Selector '[] (Id NSError)
operationErrorSelector = mkSelector "operationError"

-- | @Selector@ for @progress@
progressSelector :: Selector '[] (Id NSProgress)
progressSelector = mkSelector "progress"

