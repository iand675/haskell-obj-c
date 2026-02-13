{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPersistentCloudKitContainerEventResult@.
module ObjC.CoreData.NSPersistentCloudKitContainerEventResult
  ( NSPersistentCloudKitContainerEventResult
  , IsNSPersistentCloudKitContainerEventResult(..)
  , init_
  , new
  , result
  , resultType
  , initSelector
  , newSelector
  , resultSelector
  , resultTypeSelector

  -- * Enum types
  , NSPersistentCloudKitContainerEventResultType(NSPersistentCloudKitContainerEventResultType)
  , pattern NSPersistentCloudKitContainerEventResultTypeEvents
  , pattern NSPersistentCloudKitContainerEventResultTypeCountEvents

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.CoreData.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSPersistentCloudKitContainerEventResult nsPersistentCloudKitContainerEventResult => nsPersistentCloudKitContainerEventResult -> IO (Id NSPersistentCloudKitContainerEventResult)
init_ nsPersistentCloudKitContainerEventResult =
  sendOwnedMessage nsPersistentCloudKitContainerEventResult initSelector

-- | @+ new@
new :: IO (Id NSPersistentCloudKitContainerEventResult)
new  =
  do
    cls' <- getRequiredClass "NSPersistentCloudKitContainerEventResult"
    sendOwnedClassMessage cls' newSelector

-- | @- result@
result :: IsNSPersistentCloudKitContainerEventResult nsPersistentCloudKitContainerEventResult => nsPersistentCloudKitContainerEventResult -> IO RawId
result nsPersistentCloudKitContainerEventResult =
  sendMessage nsPersistentCloudKitContainerEventResult resultSelector

-- | @- resultType@
resultType :: IsNSPersistentCloudKitContainerEventResult nsPersistentCloudKitContainerEventResult => nsPersistentCloudKitContainerEventResult -> IO NSPersistentCloudKitContainerEventResultType
resultType nsPersistentCloudKitContainerEventResult =
  sendMessage nsPersistentCloudKitContainerEventResult resultTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSPersistentCloudKitContainerEventResult)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSPersistentCloudKitContainerEventResult)
newSelector = mkSelector "new"

-- | @Selector@ for @result@
resultSelector :: Selector '[] RawId
resultSelector = mkSelector "result"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector '[] NSPersistentCloudKitContainerEventResultType
resultTypeSelector = mkSelector "resultType"

