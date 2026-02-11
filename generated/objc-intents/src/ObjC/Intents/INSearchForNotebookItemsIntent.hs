{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSearchForNotebookItemsIntent@.
module ObjC.Intents.INSearchForNotebookItemsIntent
  ( INSearchForNotebookItemsIntent
  , IsINSearchForNotebookItemsIntent(..)
  , initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_temporalEventTriggerTypes_taskPriority_notebookItemIdentifier
  , initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType
  , initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_notebookItemIdentifier
  , title
  , content
  , itemType
  , status
  , location
  , locationSearchType
  , dateTime
  , dateSearchType
  , temporalEventTriggerTypes
  , taskPriority
  , notebookItemIdentifier
  , initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_temporalEventTriggerTypes_taskPriority_notebookItemIdentifierSelector
  , initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchTypeSelector
  , initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_notebookItemIdentifierSelector
  , titleSelector
  , contentSelector
  , itemTypeSelector
  , statusSelector
  , locationSelector
  , locationSearchTypeSelector
  , dateTimeSelector
  , dateSearchTypeSelector
  , temporalEventTriggerTypesSelector
  , taskPrioritySelector
  , notebookItemIdentifierSelector

  -- * Enum types
  , INDateSearchType(INDateSearchType)
  , pattern INDateSearchTypeUnknown
  , pattern INDateSearchTypeByDueDate
  , pattern INDateSearchTypeByModifiedDate
  , pattern INDateSearchTypeByCreatedDate
  , INLocationSearchType(INLocationSearchType)
  , pattern INLocationSearchTypeUnknown
  , pattern INLocationSearchTypeByLocationTrigger
  , INNotebookItemType(INNotebookItemType)
  , pattern INNotebookItemTypeUnknown
  , pattern INNotebookItemTypeNote
  , pattern INNotebookItemTypeTaskList
  , pattern INNotebookItemTypeTask
  , INTaskPriority(INTaskPriority)
  , pattern INTaskPriorityUnknown
  , pattern INTaskPriorityNotFlagged
  , pattern INTaskPriorityFlagged
  , INTaskStatus(INTaskStatus)
  , pattern INTaskStatusUnknown
  , pattern INTaskStatusNotCompleted
  , pattern INTaskStatusCompleted
  , INTemporalEventTriggerTypeOptions(INTemporalEventTriggerTypeOptions)
  , pattern INTemporalEventTriggerTypeOptionNotScheduled
  , pattern INTemporalEventTriggerTypeOptionScheduledNonRecurring
  , pattern INTemporalEventTriggerTypeOptionScheduledRecurring

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

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithTitle:content:itemType:status:location:locationSearchType:dateTime:dateSearchType:temporalEventTriggerTypes:taskPriority:notebookItemIdentifier:@
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_temporalEventTriggerTypes_taskPriority_notebookItemIdentifier :: (IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent, IsINSpeakableString title, IsNSString content, IsCLPlacemark location, IsINDateComponentsRange dateTime, IsNSString notebookItemIdentifier) => inSearchForNotebookItemsIntent -> title -> content -> INNotebookItemType -> INTaskStatus -> location -> INLocationSearchType -> dateTime -> INDateSearchType -> INTemporalEventTriggerTypeOptions -> INTaskPriority -> notebookItemIdentifier -> IO (Id INSearchForNotebookItemsIntent)
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_temporalEventTriggerTypes_taskPriority_notebookItemIdentifier inSearchForNotebookItemsIntent  title content itemType status location locationSearchType dateTime dateSearchType temporalEventTriggerTypes taskPriority notebookItemIdentifier =
withObjCPtr title $ \raw_title ->
  withObjCPtr content $ \raw_content ->
    withObjCPtr location $ \raw_location ->
      withObjCPtr dateTime $ \raw_dateTime ->
        withObjCPtr notebookItemIdentifier $ \raw_notebookItemIdentifier ->
            sendMsg inSearchForNotebookItemsIntent (mkSelector "initWithTitle:content:itemType:status:location:locationSearchType:dateTime:dateSearchType:temporalEventTriggerTypes:taskPriority:notebookItemIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_content :: Ptr ()), argCLong (coerce itemType), argCLong (coerce status), argPtr (castPtr raw_location :: Ptr ()), argCLong (coerce locationSearchType), argPtr (castPtr raw_dateTime :: Ptr ()), argCLong (coerce dateSearchType), argCULong (coerce temporalEventTriggerTypes), argCLong (coerce taskPriority), argPtr (castPtr raw_notebookItemIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithTitle:content:itemType:status:location:locationSearchType:dateTime:dateSearchType:@
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType :: (IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent, IsINSpeakableString title, IsNSString content, IsCLPlacemark location, IsINDateComponentsRange dateTime) => inSearchForNotebookItemsIntent -> title -> content -> INNotebookItemType -> INTaskStatus -> location -> INLocationSearchType -> dateTime -> INDateSearchType -> IO (Id INSearchForNotebookItemsIntent)
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType inSearchForNotebookItemsIntent  title content itemType status location locationSearchType dateTime dateSearchType =
withObjCPtr title $ \raw_title ->
  withObjCPtr content $ \raw_content ->
    withObjCPtr location $ \raw_location ->
      withObjCPtr dateTime $ \raw_dateTime ->
          sendMsg inSearchForNotebookItemsIntent (mkSelector "initWithTitle:content:itemType:status:location:locationSearchType:dateTime:dateSearchType:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_content :: Ptr ()), argCLong (coerce itemType), argCLong (coerce status), argPtr (castPtr raw_location :: Ptr ()), argCLong (coerce locationSearchType), argPtr (castPtr raw_dateTime :: Ptr ()), argCLong (coerce dateSearchType)] >>= ownedObject . castPtr

-- | @- initWithTitle:content:itemType:status:location:locationSearchType:dateTime:dateSearchType:notebookItemIdentifier:@
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_notebookItemIdentifier :: (IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent, IsINSpeakableString title, IsNSString content, IsCLPlacemark location, IsINDateComponentsRange dateTime, IsNSString notebookItemIdentifier) => inSearchForNotebookItemsIntent -> title -> content -> INNotebookItemType -> INTaskStatus -> location -> INLocationSearchType -> dateTime -> INDateSearchType -> notebookItemIdentifier -> IO (Id INSearchForNotebookItemsIntent)
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_notebookItemIdentifier inSearchForNotebookItemsIntent  title content itemType status location locationSearchType dateTime dateSearchType notebookItemIdentifier =
withObjCPtr title $ \raw_title ->
  withObjCPtr content $ \raw_content ->
    withObjCPtr location $ \raw_location ->
      withObjCPtr dateTime $ \raw_dateTime ->
        withObjCPtr notebookItemIdentifier $ \raw_notebookItemIdentifier ->
            sendMsg inSearchForNotebookItemsIntent (mkSelector "initWithTitle:content:itemType:status:location:locationSearchType:dateTime:dateSearchType:notebookItemIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_content :: Ptr ()), argCLong (coerce itemType), argCLong (coerce status), argPtr (castPtr raw_location :: Ptr ()), argCLong (coerce locationSearchType), argPtr (castPtr raw_dateTime :: Ptr ()), argCLong (coerce dateSearchType), argPtr (castPtr raw_notebookItemIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- title@
title :: IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent => inSearchForNotebookItemsIntent -> IO (Id INSpeakableString)
title inSearchForNotebookItemsIntent  =
  sendMsg inSearchForNotebookItemsIntent (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- content@
content :: IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent => inSearchForNotebookItemsIntent -> IO (Id NSString)
content inSearchForNotebookItemsIntent  =
  sendMsg inSearchForNotebookItemsIntent (mkSelector "content") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- itemType@
itemType :: IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent => inSearchForNotebookItemsIntent -> IO INNotebookItemType
itemType inSearchForNotebookItemsIntent  =
  fmap (coerce :: CLong -> INNotebookItemType) $ sendMsg inSearchForNotebookItemsIntent (mkSelector "itemType") retCLong []

-- | @- status@
status :: IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent => inSearchForNotebookItemsIntent -> IO INTaskStatus
status inSearchForNotebookItemsIntent  =
  fmap (coerce :: CLong -> INTaskStatus) $ sendMsg inSearchForNotebookItemsIntent (mkSelector "status") retCLong []

-- | @- location@
location :: IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent => inSearchForNotebookItemsIntent -> IO (Id CLPlacemark)
location inSearchForNotebookItemsIntent  =
  sendMsg inSearchForNotebookItemsIntent (mkSelector "location") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- locationSearchType@
locationSearchType :: IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent => inSearchForNotebookItemsIntent -> IO INLocationSearchType
locationSearchType inSearchForNotebookItemsIntent  =
  fmap (coerce :: CLong -> INLocationSearchType) $ sendMsg inSearchForNotebookItemsIntent (mkSelector "locationSearchType") retCLong []

-- | @- dateTime@
dateTime :: IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent => inSearchForNotebookItemsIntent -> IO (Id INDateComponentsRange)
dateTime inSearchForNotebookItemsIntent  =
  sendMsg inSearchForNotebookItemsIntent (mkSelector "dateTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dateSearchType@
dateSearchType :: IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent => inSearchForNotebookItemsIntent -> IO INDateSearchType
dateSearchType inSearchForNotebookItemsIntent  =
  fmap (coerce :: CLong -> INDateSearchType) $ sendMsg inSearchForNotebookItemsIntent (mkSelector "dateSearchType") retCLong []

-- | @- temporalEventTriggerTypes@
temporalEventTriggerTypes :: IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent => inSearchForNotebookItemsIntent -> IO INTemporalEventTriggerTypeOptions
temporalEventTriggerTypes inSearchForNotebookItemsIntent  =
  fmap (coerce :: CULong -> INTemporalEventTriggerTypeOptions) $ sendMsg inSearchForNotebookItemsIntent (mkSelector "temporalEventTriggerTypes") retCULong []

-- | @- taskPriority@
taskPriority :: IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent => inSearchForNotebookItemsIntent -> IO INTaskPriority
taskPriority inSearchForNotebookItemsIntent  =
  fmap (coerce :: CLong -> INTaskPriority) $ sendMsg inSearchForNotebookItemsIntent (mkSelector "taskPriority") retCLong []

-- | @- notebookItemIdentifier@
notebookItemIdentifier :: IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent => inSearchForNotebookItemsIntent -> IO (Id NSString)
notebookItemIdentifier inSearchForNotebookItemsIntent  =
  sendMsg inSearchForNotebookItemsIntent (mkSelector "notebookItemIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:content:itemType:status:location:locationSearchType:dateTime:dateSearchType:temporalEventTriggerTypes:taskPriority:notebookItemIdentifier:@
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_temporalEventTriggerTypes_taskPriority_notebookItemIdentifierSelector :: Selector
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_temporalEventTriggerTypes_taskPriority_notebookItemIdentifierSelector = mkSelector "initWithTitle:content:itemType:status:location:locationSearchType:dateTime:dateSearchType:temporalEventTriggerTypes:taskPriority:notebookItemIdentifier:"

-- | @Selector@ for @initWithTitle:content:itemType:status:location:locationSearchType:dateTime:dateSearchType:@
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchTypeSelector :: Selector
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchTypeSelector = mkSelector "initWithTitle:content:itemType:status:location:locationSearchType:dateTime:dateSearchType:"

-- | @Selector@ for @initWithTitle:content:itemType:status:location:locationSearchType:dateTime:dateSearchType:notebookItemIdentifier:@
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_notebookItemIdentifierSelector :: Selector
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_notebookItemIdentifierSelector = mkSelector "initWithTitle:content:itemType:status:location:locationSearchType:dateTime:dateSearchType:notebookItemIdentifier:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @content@
contentSelector :: Selector
contentSelector = mkSelector "content"

-- | @Selector@ for @itemType@
itemTypeSelector :: Selector
itemTypeSelector = mkSelector "itemType"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @location@
locationSelector :: Selector
locationSelector = mkSelector "location"

-- | @Selector@ for @locationSearchType@
locationSearchTypeSelector :: Selector
locationSearchTypeSelector = mkSelector "locationSearchType"

-- | @Selector@ for @dateTime@
dateTimeSelector :: Selector
dateTimeSelector = mkSelector "dateTime"

-- | @Selector@ for @dateSearchType@
dateSearchTypeSelector :: Selector
dateSearchTypeSelector = mkSelector "dateSearchType"

-- | @Selector@ for @temporalEventTriggerTypes@
temporalEventTriggerTypesSelector :: Selector
temporalEventTriggerTypesSelector = mkSelector "temporalEventTriggerTypes"

-- | @Selector@ for @taskPriority@
taskPrioritySelector :: Selector
taskPrioritySelector = mkSelector "taskPriority"

-- | @Selector@ for @notebookItemIdentifier@
notebookItemIdentifierSelector :: Selector
notebookItemIdentifierSelector = mkSelector "notebookItemIdentifier"

