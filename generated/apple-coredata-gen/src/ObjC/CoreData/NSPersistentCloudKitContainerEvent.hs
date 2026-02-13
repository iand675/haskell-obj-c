{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPersistentCloudKitContainerEvent@.
module ObjC.CoreData.NSPersistentCloudKitContainerEvent
  ( NSPersistentCloudKitContainerEvent
  , IsNSPersistentCloudKitContainerEvent(..)
  , new
  , init_
  , identifier
  , storeIdentifier
  , type_
  , startDate
  , endDate
  , succeeded
  , error_
  , endDateSelector
  , errorSelector
  , identifierSelector
  , initSelector
  , newSelector
  , startDateSelector
  , storeIdentifierSelector
  , succeededSelector
  , typeSelector

  -- * Enum types
  , NSPersistentCloudKitContainerEventType(NSPersistentCloudKitContainerEventType)
  , pattern NSPersistentCloudKitContainerEventTypeSetup
  , pattern NSPersistentCloudKitContainerEventTypeImport
  , pattern NSPersistentCloudKitContainerEventTypeExport

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

-- | @+ new@
new :: IO (Id NSPersistentCloudKitContainerEvent)
new  =
  do
    cls' <- getRequiredClass "NSPersistentCloudKitContainerEvent"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsNSPersistentCloudKitContainerEvent nsPersistentCloudKitContainerEvent => nsPersistentCloudKitContainerEvent -> IO (Id NSPersistentCloudKitContainerEvent)
init_ nsPersistentCloudKitContainerEvent =
  sendOwnedMessage nsPersistentCloudKitContainerEvent initSelector

-- | @- identifier@
identifier :: IsNSPersistentCloudKitContainerEvent nsPersistentCloudKitContainerEvent => nsPersistentCloudKitContainerEvent -> IO (Id NSUUID)
identifier nsPersistentCloudKitContainerEvent =
  sendMessage nsPersistentCloudKitContainerEvent identifierSelector

-- | @- storeIdentifier@
storeIdentifier :: IsNSPersistentCloudKitContainerEvent nsPersistentCloudKitContainerEvent => nsPersistentCloudKitContainerEvent -> IO (Id NSString)
storeIdentifier nsPersistentCloudKitContainerEvent =
  sendMessage nsPersistentCloudKitContainerEvent storeIdentifierSelector

-- | @- type@
type_ :: IsNSPersistentCloudKitContainerEvent nsPersistentCloudKitContainerEvent => nsPersistentCloudKitContainerEvent -> IO NSPersistentCloudKitContainerEventType
type_ nsPersistentCloudKitContainerEvent =
  sendMessage nsPersistentCloudKitContainerEvent typeSelector

-- | @- startDate@
startDate :: IsNSPersistentCloudKitContainerEvent nsPersistentCloudKitContainerEvent => nsPersistentCloudKitContainerEvent -> IO (Id NSDate)
startDate nsPersistentCloudKitContainerEvent =
  sendMessage nsPersistentCloudKitContainerEvent startDateSelector

-- | @- endDate@
endDate :: IsNSPersistentCloudKitContainerEvent nsPersistentCloudKitContainerEvent => nsPersistentCloudKitContainerEvent -> IO (Id NSDate)
endDate nsPersistentCloudKitContainerEvent =
  sendMessage nsPersistentCloudKitContainerEvent endDateSelector

-- | @- succeeded@
succeeded :: IsNSPersistentCloudKitContainerEvent nsPersistentCloudKitContainerEvent => nsPersistentCloudKitContainerEvent -> IO Bool
succeeded nsPersistentCloudKitContainerEvent =
  sendMessage nsPersistentCloudKitContainerEvent succeededSelector

-- | @- error@
error_ :: IsNSPersistentCloudKitContainerEvent nsPersistentCloudKitContainerEvent => nsPersistentCloudKitContainerEvent -> IO (Id NSError)
error_ nsPersistentCloudKitContainerEvent =
  sendMessage nsPersistentCloudKitContainerEvent errorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSPersistentCloudKitContainerEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSPersistentCloudKitContainerEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSUUID)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @storeIdentifier@
storeIdentifierSelector :: Selector '[] (Id NSString)
storeIdentifierSelector = mkSelector "storeIdentifier"

-- | @Selector@ for @type@
typeSelector :: Selector '[] NSPersistentCloudKitContainerEventType
typeSelector = mkSelector "type"

-- | @Selector@ for @startDate@
startDateSelector :: Selector '[] (Id NSDate)
startDateSelector = mkSelector "startDate"

-- | @Selector@ for @endDate@
endDateSelector :: Selector '[] (Id NSDate)
endDateSelector = mkSelector "endDate"

-- | @Selector@ for @succeeded@
succeededSelector :: Selector '[] Bool
succeededSelector = mkSelector "succeeded"

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

