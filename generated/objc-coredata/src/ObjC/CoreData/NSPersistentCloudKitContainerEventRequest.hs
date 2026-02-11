{-# LANGUAGE PatternSynonyms #-}
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
import ObjC.CoreData.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ fetchEventsAfterDate:@
fetchEventsAfterDate :: IsNSDate date => date -> IO (Id NSPersistentCloudKitContainerEventRequest)
fetchEventsAfterDate date =
  do
    cls' <- getRequiredClass "NSPersistentCloudKitContainerEventRequest"
    withObjCPtr date $ \raw_date ->
      sendClassMsg cls' (mkSelector "fetchEventsAfterDate:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fetchEventsAfterEvent:@
fetchEventsAfterEvent :: IsNSPersistentCloudKitContainerEvent event => event -> IO (Id NSPersistentCloudKitContainerEventRequest)
fetchEventsAfterEvent event =
  do
    cls' <- getRequiredClass "NSPersistentCloudKitContainerEventRequest"
    withObjCPtr event $ \raw_event ->
      sendClassMsg cls' (mkSelector "fetchEventsAfterEvent:") (retPtr retVoid) [argPtr (castPtr raw_event :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fetchEventsMatchingFetchRequest:@
fetchEventsMatchingFetchRequest :: IsNSFetchRequest fetchRequest => fetchRequest -> IO (Id NSPersistentCloudKitContainerEventRequest)
fetchEventsMatchingFetchRequest fetchRequest =
  do
    cls' <- getRequiredClass "NSPersistentCloudKitContainerEventRequest"
    withObjCPtr fetchRequest $ \raw_fetchRequest ->
      sendClassMsg cls' (mkSelector "fetchEventsMatchingFetchRequest:") (retPtr retVoid) [argPtr (castPtr raw_fetchRequest :: Ptr ())] >>= retainedObject . castPtr

-- | @+ fetchRequestForEvents@
fetchRequestForEvents :: IO (Id NSFetchRequest)
fetchRequestForEvents  =
  do
    cls' <- getRequiredClass "NSPersistentCloudKitContainerEventRequest"
    sendClassMsg cls' (mkSelector "fetchRequestForEvents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- resultType@
resultType :: IsNSPersistentCloudKitContainerEventRequest nsPersistentCloudKitContainerEventRequest => nsPersistentCloudKitContainerEventRequest -> IO NSPersistentCloudKitContainerEventResultType
resultType nsPersistentCloudKitContainerEventRequest  =
  fmap (coerce :: CLong -> NSPersistentCloudKitContainerEventResultType) $ sendMsg nsPersistentCloudKitContainerEventRequest (mkSelector "resultType") retCLong []

-- | @- setResultType:@
setResultType :: IsNSPersistentCloudKitContainerEventRequest nsPersistentCloudKitContainerEventRequest => nsPersistentCloudKitContainerEventRequest -> NSPersistentCloudKitContainerEventResultType -> IO ()
setResultType nsPersistentCloudKitContainerEventRequest  value =
  sendMsg nsPersistentCloudKitContainerEventRequest (mkSelector "setResultType:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchEventsAfterDate:@
fetchEventsAfterDateSelector :: Selector
fetchEventsAfterDateSelector = mkSelector "fetchEventsAfterDate:"

-- | @Selector@ for @fetchEventsAfterEvent:@
fetchEventsAfterEventSelector :: Selector
fetchEventsAfterEventSelector = mkSelector "fetchEventsAfterEvent:"

-- | @Selector@ for @fetchEventsMatchingFetchRequest:@
fetchEventsMatchingFetchRequestSelector :: Selector
fetchEventsMatchingFetchRequestSelector = mkSelector "fetchEventsMatchingFetchRequest:"

-- | @Selector@ for @fetchRequestForEvents@
fetchRequestForEventsSelector :: Selector
fetchRequestForEventsSelector = mkSelector "fetchRequestForEvents"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector
resultTypeSelector = mkSelector "resultType"

-- | @Selector@ for @setResultType:@
setResultTypeSelector :: Selector
setResultTypeSelector = mkSelector "setResultType:"

