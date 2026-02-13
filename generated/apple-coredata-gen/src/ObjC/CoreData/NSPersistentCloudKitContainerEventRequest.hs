{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPersistentCloudKitContainerEventRequest@.
module ObjC.CoreData.NSPersistentCloudKitContainerEventRequest
  ( NSPersistentCloudKitContainerEventRequest
  , IsNSPersistentCloudKitContainerEventRequest(..)
  , fetchEventsAfterDate
  , fetchEventsAfterEvent
  , fetchEventsMatchingFetchRequest
  , fetchRequestForEvents
  , resultType
  , setResultType
  , fetchEventsAfterDateSelector
  , fetchEventsAfterEventSelector
  , fetchEventsMatchingFetchRequestSelector
  , fetchRequestForEventsSelector
  , resultTypeSelector
  , setResultTypeSelector

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

-- | @+ fetchEventsAfterDate:@
fetchEventsAfterDate :: IsNSDate date => date -> IO (Id NSPersistentCloudKitContainerEventRequest)
fetchEventsAfterDate date =
  do
    cls' <- getRequiredClass "NSPersistentCloudKitContainerEventRequest"
    sendClassMessage cls' fetchEventsAfterDateSelector (toNSDate date)

-- | @+ fetchEventsAfterEvent:@
fetchEventsAfterEvent :: IsNSPersistentCloudKitContainerEvent event => event -> IO (Id NSPersistentCloudKitContainerEventRequest)
fetchEventsAfterEvent event =
  do
    cls' <- getRequiredClass "NSPersistentCloudKitContainerEventRequest"
    sendClassMessage cls' fetchEventsAfterEventSelector (toNSPersistentCloudKitContainerEvent event)

-- | @+ fetchEventsMatchingFetchRequest:@
fetchEventsMatchingFetchRequest :: IsNSFetchRequest fetchRequest => fetchRequest -> IO (Id NSPersistentCloudKitContainerEventRequest)
fetchEventsMatchingFetchRequest fetchRequest =
  do
    cls' <- getRequiredClass "NSPersistentCloudKitContainerEventRequest"
    sendClassMessage cls' fetchEventsMatchingFetchRequestSelector (toNSFetchRequest fetchRequest)

-- | @+ fetchRequestForEvents@
fetchRequestForEvents :: IO (Id NSFetchRequest)
fetchRequestForEvents  =
  do
    cls' <- getRequiredClass "NSPersistentCloudKitContainerEventRequest"
    sendClassMessage cls' fetchRequestForEventsSelector

-- | @- resultType@
resultType :: IsNSPersistentCloudKitContainerEventRequest nsPersistentCloudKitContainerEventRequest => nsPersistentCloudKitContainerEventRequest -> IO NSPersistentCloudKitContainerEventResultType
resultType nsPersistentCloudKitContainerEventRequest =
  sendMessage nsPersistentCloudKitContainerEventRequest resultTypeSelector

-- | @- setResultType:@
setResultType :: IsNSPersistentCloudKitContainerEventRequest nsPersistentCloudKitContainerEventRequest => nsPersistentCloudKitContainerEventRequest -> NSPersistentCloudKitContainerEventResultType -> IO ()
setResultType nsPersistentCloudKitContainerEventRequest value =
  sendMessage nsPersistentCloudKitContainerEventRequest setResultTypeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchEventsAfterDate:@
fetchEventsAfterDateSelector :: Selector '[Id NSDate] (Id NSPersistentCloudKitContainerEventRequest)
fetchEventsAfterDateSelector = mkSelector "fetchEventsAfterDate:"

-- | @Selector@ for @fetchEventsAfterEvent:@
fetchEventsAfterEventSelector :: Selector '[Id NSPersistentCloudKitContainerEvent] (Id NSPersistentCloudKitContainerEventRequest)
fetchEventsAfterEventSelector = mkSelector "fetchEventsAfterEvent:"

-- | @Selector@ for @fetchEventsMatchingFetchRequest:@
fetchEventsMatchingFetchRequestSelector :: Selector '[Id NSFetchRequest] (Id NSPersistentCloudKitContainerEventRequest)
fetchEventsMatchingFetchRequestSelector = mkSelector "fetchEventsMatchingFetchRequest:"

-- | @Selector@ for @fetchRequestForEvents@
fetchRequestForEventsSelector :: Selector '[] (Id NSFetchRequest)
fetchRequestForEventsSelector = mkSelector "fetchRequestForEvents"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector '[] NSPersistentCloudKitContainerEventResultType
resultTypeSelector = mkSelector "resultType"

-- | @Selector@ for @setResultType:@
setResultTypeSelector :: Selector '[NSPersistentCloudKitContainerEventResultType] ()
setResultTypeSelector = mkSelector "setResultType:"

