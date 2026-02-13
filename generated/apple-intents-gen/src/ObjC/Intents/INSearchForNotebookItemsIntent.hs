{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , contentSelector
  , dateSearchTypeSelector
  , dateTimeSelector
  , initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchTypeSelector
  , initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_notebookItemIdentifierSelector
  , initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_temporalEventTriggerTypes_taskPriority_notebookItemIdentifierSelector
  , itemTypeSelector
  , locationSearchTypeSelector
  , locationSelector
  , notebookItemIdentifierSelector
  , statusSelector
  , taskPrioritySelector
  , temporalEventTriggerTypesSelector
  , titleSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithTitle:content:itemType:status:location:locationSearchType:dateTime:dateSearchType:temporalEventTriggerTypes:taskPriority:notebookItemIdentifier:@
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_temporalEventTriggerTypes_taskPriority_notebookItemIdentifier :: (IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent, IsINSpeakableString title, IsNSString content, IsCLPlacemark location, IsINDateComponentsRange dateTime, IsNSString notebookItemIdentifier) => inSearchForNotebookItemsIntent -> title -> content -> INNotebookItemType -> INTaskStatus -> location -> INLocationSearchType -> dateTime -> INDateSearchType -> INTemporalEventTriggerTypeOptions -> INTaskPriority -> notebookItemIdentifier -> IO (Id INSearchForNotebookItemsIntent)
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_temporalEventTriggerTypes_taskPriority_notebookItemIdentifier inSearchForNotebookItemsIntent title content itemType status location locationSearchType dateTime dateSearchType temporalEventTriggerTypes taskPriority notebookItemIdentifier =
  sendOwnedMessage inSearchForNotebookItemsIntent initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_temporalEventTriggerTypes_taskPriority_notebookItemIdentifierSelector (toINSpeakableString title) (toNSString content) itemType status (toCLPlacemark location) locationSearchType (toINDateComponentsRange dateTime) dateSearchType temporalEventTriggerTypes taskPriority (toNSString notebookItemIdentifier)

-- | @- initWithTitle:content:itemType:status:location:locationSearchType:dateTime:dateSearchType:@
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType :: (IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent, IsINSpeakableString title, IsNSString content, IsCLPlacemark location, IsINDateComponentsRange dateTime) => inSearchForNotebookItemsIntent -> title -> content -> INNotebookItemType -> INTaskStatus -> location -> INLocationSearchType -> dateTime -> INDateSearchType -> IO (Id INSearchForNotebookItemsIntent)
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType inSearchForNotebookItemsIntent title content itemType status location locationSearchType dateTime dateSearchType =
  sendOwnedMessage inSearchForNotebookItemsIntent initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchTypeSelector (toINSpeakableString title) (toNSString content) itemType status (toCLPlacemark location) locationSearchType (toINDateComponentsRange dateTime) dateSearchType

-- | @- initWithTitle:content:itemType:status:location:locationSearchType:dateTime:dateSearchType:notebookItemIdentifier:@
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_notebookItemIdentifier :: (IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent, IsINSpeakableString title, IsNSString content, IsCLPlacemark location, IsINDateComponentsRange dateTime, IsNSString notebookItemIdentifier) => inSearchForNotebookItemsIntent -> title -> content -> INNotebookItemType -> INTaskStatus -> location -> INLocationSearchType -> dateTime -> INDateSearchType -> notebookItemIdentifier -> IO (Id INSearchForNotebookItemsIntent)
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_notebookItemIdentifier inSearchForNotebookItemsIntent title content itemType status location locationSearchType dateTime dateSearchType notebookItemIdentifier =
  sendOwnedMessage inSearchForNotebookItemsIntent initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_notebookItemIdentifierSelector (toINSpeakableString title) (toNSString content) itemType status (toCLPlacemark location) locationSearchType (toINDateComponentsRange dateTime) dateSearchType (toNSString notebookItemIdentifier)

-- | @- title@
title :: IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent => inSearchForNotebookItemsIntent -> IO (Id INSpeakableString)
title inSearchForNotebookItemsIntent =
  sendMessage inSearchForNotebookItemsIntent titleSelector

-- | @- content@
content :: IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent => inSearchForNotebookItemsIntent -> IO (Id NSString)
content inSearchForNotebookItemsIntent =
  sendMessage inSearchForNotebookItemsIntent contentSelector

-- | @- itemType@
itemType :: IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent => inSearchForNotebookItemsIntent -> IO INNotebookItemType
itemType inSearchForNotebookItemsIntent =
  sendMessage inSearchForNotebookItemsIntent itemTypeSelector

-- | @- status@
status :: IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent => inSearchForNotebookItemsIntent -> IO INTaskStatus
status inSearchForNotebookItemsIntent =
  sendMessage inSearchForNotebookItemsIntent statusSelector

-- | @- location@
location :: IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent => inSearchForNotebookItemsIntent -> IO (Id CLPlacemark)
location inSearchForNotebookItemsIntent =
  sendMessage inSearchForNotebookItemsIntent locationSelector

-- | @- locationSearchType@
locationSearchType :: IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent => inSearchForNotebookItemsIntent -> IO INLocationSearchType
locationSearchType inSearchForNotebookItemsIntent =
  sendMessage inSearchForNotebookItemsIntent locationSearchTypeSelector

-- | @- dateTime@
dateTime :: IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent => inSearchForNotebookItemsIntent -> IO (Id INDateComponentsRange)
dateTime inSearchForNotebookItemsIntent =
  sendMessage inSearchForNotebookItemsIntent dateTimeSelector

-- | @- dateSearchType@
dateSearchType :: IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent => inSearchForNotebookItemsIntent -> IO INDateSearchType
dateSearchType inSearchForNotebookItemsIntent =
  sendMessage inSearchForNotebookItemsIntent dateSearchTypeSelector

-- | @- temporalEventTriggerTypes@
temporalEventTriggerTypes :: IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent => inSearchForNotebookItemsIntent -> IO INTemporalEventTriggerTypeOptions
temporalEventTriggerTypes inSearchForNotebookItemsIntent =
  sendMessage inSearchForNotebookItemsIntent temporalEventTriggerTypesSelector

-- | @- taskPriority@
taskPriority :: IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent => inSearchForNotebookItemsIntent -> IO INTaskPriority
taskPriority inSearchForNotebookItemsIntent =
  sendMessage inSearchForNotebookItemsIntent taskPrioritySelector

-- | @- notebookItemIdentifier@
notebookItemIdentifier :: IsINSearchForNotebookItemsIntent inSearchForNotebookItemsIntent => inSearchForNotebookItemsIntent -> IO (Id NSString)
notebookItemIdentifier inSearchForNotebookItemsIntent =
  sendMessage inSearchForNotebookItemsIntent notebookItemIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:content:itemType:status:location:locationSearchType:dateTime:dateSearchType:temporalEventTriggerTypes:taskPriority:notebookItemIdentifier:@
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_temporalEventTriggerTypes_taskPriority_notebookItemIdentifierSelector :: Selector '[Id INSpeakableString, Id NSString, INNotebookItemType, INTaskStatus, Id CLPlacemark, INLocationSearchType, Id INDateComponentsRange, INDateSearchType, INTemporalEventTriggerTypeOptions, INTaskPriority, Id NSString] (Id INSearchForNotebookItemsIntent)
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_temporalEventTriggerTypes_taskPriority_notebookItemIdentifierSelector = mkSelector "initWithTitle:content:itemType:status:location:locationSearchType:dateTime:dateSearchType:temporalEventTriggerTypes:taskPriority:notebookItemIdentifier:"

-- | @Selector@ for @initWithTitle:content:itemType:status:location:locationSearchType:dateTime:dateSearchType:@
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchTypeSelector :: Selector '[Id INSpeakableString, Id NSString, INNotebookItemType, INTaskStatus, Id CLPlacemark, INLocationSearchType, Id INDateComponentsRange, INDateSearchType] (Id INSearchForNotebookItemsIntent)
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchTypeSelector = mkSelector "initWithTitle:content:itemType:status:location:locationSearchType:dateTime:dateSearchType:"

-- | @Selector@ for @initWithTitle:content:itemType:status:location:locationSearchType:dateTime:dateSearchType:notebookItemIdentifier:@
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_notebookItemIdentifierSelector :: Selector '[Id INSpeakableString, Id NSString, INNotebookItemType, INTaskStatus, Id CLPlacemark, INLocationSearchType, Id INDateComponentsRange, INDateSearchType, Id NSString] (Id INSearchForNotebookItemsIntent)
initWithTitle_content_itemType_status_location_locationSearchType_dateTime_dateSearchType_notebookItemIdentifierSelector = mkSelector "initWithTitle:content:itemType:status:location:locationSearchType:dateTime:dateSearchType:notebookItemIdentifier:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id INSpeakableString)
titleSelector = mkSelector "title"

-- | @Selector@ for @content@
contentSelector :: Selector '[] (Id NSString)
contentSelector = mkSelector "content"

-- | @Selector@ for @itemType@
itemTypeSelector :: Selector '[] INNotebookItemType
itemTypeSelector = mkSelector "itemType"

-- | @Selector@ for @status@
statusSelector :: Selector '[] INTaskStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @location@
locationSelector :: Selector '[] (Id CLPlacemark)
locationSelector = mkSelector "location"

-- | @Selector@ for @locationSearchType@
locationSearchTypeSelector :: Selector '[] INLocationSearchType
locationSearchTypeSelector = mkSelector "locationSearchType"

-- | @Selector@ for @dateTime@
dateTimeSelector :: Selector '[] (Id INDateComponentsRange)
dateTimeSelector = mkSelector "dateTime"

-- | @Selector@ for @dateSearchType@
dateSearchTypeSelector :: Selector '[] INDateSearchType
dateSearchTypeSelector = mkSelector "dateSearchType"

-- | @Selector@ for @temporalEventTriggerTypes@
temporalEventTriggerTypesSelector :: Selector '[] INTemporalEventTriggerTypeOptions
temporalEventTriggerTypesSelector = mkSelector "temporalEventTriggerTypes"

-- | @Selector@ for @taskPriority@
taskPrioritySelector :: Selector '[] INTaskPriority
taskPrioritySelector = mkSelector "taskPriority"

-- | @Selector@ for @notebookItemIdentifier@
notebookItemIdentifierSelector :: Selector '[] (Id NSString)
notebookItemIdentifierSelector = mkSelector "notebookItemIdentifier"

