{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterAccessControlEntryChangedEvent@.
module ObjC.Matter.MTRAccessControlClusterAccessControlEntryChangedEvent
  ( MTRAccessControlClusterAccessControlEntryChangedEvent
  , IsMTRAccessControlClusterAccessControlEntryChangedEvent(..)
  , adminNodeID
  , setAdminNodeID
  , adminPasscodeID
  , setAdminPasscodeID
  , changeType
  , setChangeType
  , latestValue
  , setLatestValue
  , fabricIndex
  , setFabricIndex
  , adminNodeIDSelector
  , adminPasscodeIDSelector
  , changeTypeSelector
  , fabricIndexSelector
  , latestValueSelector
  , setAdminNodeIDSelector
  , setAdminPasscodeIDSelector
  , setChangeTypeSelector
  , setFabricIndexSelector
  , setLatestValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- adminNodeID@
adminNodeID :: IsMTRAccessControlClusterAccessControlEntryChangedEvent mtrAccessControlClusterAccessControlEntryChangedEvent => mtrAccessControlClusterAccessControlEntryChangedEvent -> IO (Id NSNumber)
adminNodeID mtrAccessControlClusterAccessControlEntryChangedEvent =
  sendMessage mtrAccessControlClusterAccessControlEntryChangedEvent adminNodeIDSelector

-- | @- setAdminNodeID:@
setAdminNodeID :: (IsMTRAccessControlClusterAccessControlEntryChangedEvent mtrAccessControlClusterAccessControlEntryChangedEvent, IsNSNumber value) => mtrAccessControlClusterAccessControlEntryChangedEvent -> value -> IO ()
setAdminNodeID mtrAccessControlClusterAccessControlEntryChangedEvent value =
  sendMessage mtrAccessControlClusterAccessControlEntryChangedEvent setAdminNodeIDSelector (toNSNumber value)

-- | @- adminPasscodeID@
adminPasscodeID :: IsMTRAccessControlClusterAccessControlEntryChangedEvent mtrAccessControlClusterAccessControlEntryChangedEvent => mtrAccessControlClusterAccessControlEntryChangedEvent -> IO (Id NSNumber)
adminPasscodeID mtrAccessControlClusterAccessControlEntryChangedEvent =
  sendMessage mtrAccessControlClusterAccessControlEntryChangedEvent adminPasscodeIDSelector

-- | @- setAdminPasscodeID:@
setAdminPasscodeID :: (IsMTRAccessControlClusterAccessControlEntryChangedEvent mtrAccessControlClusterAccessControlEntryChangedEvent, IsNSNumber value) => mtrAccessControlClusterAccessControlEntryChangedEvent -> value -> IO ()
setAdminPasscodeID mtrAccessControlClusterAccessControlEntryChangedEvent value =
  sendMessage mtrAccessControlClusterAccessControlEntryChangedEvent setAdminPasscodeIDSelector (toNSNumber value)

-- | @- changeType@
changeType :: IsMTRAccessControlClusterAccessControlEntryChangedEvent mtrAccessControlClusterAccessControlEntryChangedEvent => mtrAccessControlClusterAccessControlEntryChangedEvent -> IO (Id NSNumber)
changeType mtrAccessControlClusterAccessControlEntryChangedEvent =
  sendMessage mtrAccessControlClusterAccessControlEntryChangedEvent changeTypeSelector

-- | @- setChangeType:@
setChangeType :: (IsMTRAccessControlClusterAccessControlEntryChangedEvent mtrAccessControlClusterAccessControlEntryChangedEvent, IsNSNumber value) => mtrAccessControlClusterAccessControlEntryChangedEvent -> value -> IO ()
setChangeType mtrAccessControlClusterAccessControlEntryChangedEvent value =
  sendMessage mtrAccessControlClusterAccessControlEntryChangedEvent setChangeTypeSelector (toNSNumber value)

-- | @- latestValue@
latestValue :: IsMTRAccessControlClusterAccessControlEntryChangedEvent mtrAccessControlClusterAccessControlEntryChangedEvent => mtrAccessControlClusterAccessControlEntryChangedEvent -> IO (Id MTRAccessControlClusterAccessControlEntryStruct)
latestValue mtrAccessControlClusterAccessControlEntryChangedEvent =
  sendMessage mtrAccessControlClusterAccessControlEntryChangedEvent latestValueSelector

-- | @- setLatestValue:@
setLatestValue :: (IsMTRAccessControlClusterAccessControlEntryChangedEvent mtrAccessControlClusterAccessControlEntryChangedEvent, IsMTRAccessControlClusterAccessControlEntryStruct value) => mtrAccessControlClusterAccessControlEntryChangedEvent -> value -> IO ()
setLatestValue mtrAccessControlClusterAccessControlEntryChangedEvent value =
  sendMessage mtrAccessControlClusterAccessControlEntryChangedEvent setLatestValueSelector (toMTRAccessControlClusterAccessControlEntryStruct value)

-- | @- fabricIndex@
fabricIndex :: IsMTRAccessControlClusterAccessControlEntryChangedEvent mtrAccessControlClusterAccessControlEntryChangedEvent => mtrAccessControlClusterAccessControlEntryChangedEvent -> IO (Id NSNumber)
fabricIndex mtrAccessControlClusterAccessControlEntryChangedEvent =
  sendMessage mtrAccessControlClusterAccessControlEntryChangedEvent fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRAccessControlClusterAccessControlEntryChangedEvent mtrAccessControlClusterAccessControlEntryChangedEvent, IsNSNumber value) => mtrAccessControlClusterAccessControlEntryChangedEvent -> value -> IO ()
setFabricIndex mtrAccessControlClusterAccessControlEntryChangedEvent value =
  sendMessage mtrAccessControlClusterAccessControlEntryChangedEvent setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @adminNodeID@
adminNodeIDSelector :: Selector '[] (Id NSNumber)
adminNodeIDSelector = mkSelector "adminNodeID"

-- | @Selector@ for @setAdminNodeID:@
setAdminNodeIDSelector :: Selector '[Id NSNumber] ()
setAdminNodeIDSelector = mkSelector "setAdminNodeID:"

-- | @Selector@ for @adminPasscodeID@
adminPasscodeIDSelector :: Selector '[] (Id NSNumber)
adminPasscodeIDSelector = mkSelector "adminPasscodeID"

-- | @Selector@ for @setAdminPasscodeID:@
setAdminPasscodeIDSelector :: Selector '[Id NSNumber] ()
setAdminPasscodeIDSelector = mkSelector "setAdminPasscodeID:"

-- | @Selector@ for @changeType@
changeTypeSelector :: Selector '[] (Id NSNumber)
changeTypeSelector = mkSelector "changeType"

-- | @Selector@ for @setChangeType:@
setChangeTypeSelector :: Selector '[Id NSNumber] ()
setChangeTypeSelector = mkSelector "setChangeType:"

-- | @Selector@ for @latestValue@
latestValueSelector :: Selector '[] (Id MTRAccessControlClusterAccessControlEntryStruct)
latestValueSelector = mkSelector "latestValue"

-- | @Selector@ for @setLatestValue:@
setLatestValueSelector :: Selector '[Id MTRAccessControlClusterAccessControlEntryStruct] ()
setLatestValueSelector = mkSelector "setLatestValue:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

