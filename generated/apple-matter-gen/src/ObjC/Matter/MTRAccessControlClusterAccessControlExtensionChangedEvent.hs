{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterAccessControlExtensionChangedEvent@.
module ObjC.Matter.MTRAccessControlClusterAccessControlExtensionChangedEvent
  ( MTRAccessControlClusterAccessControlExtensionChangedEvent
  , IsMTRAccessControlClusterAccessControlExtensionChangedEvent(..)
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
adminNodeID :: IsMTRAccessControlClusterAccessControlExtensionChangedEvent mtrAccessControlClusterAccessControlExtensionChangedEvent => mtrAccessControlClusterAccessControlExtensionChangedEvent -> IO (Id NSNumber)
adminNodeID mtrAccessControlClusterAccessControlExtensionChangedEvent =
  sendMessage mtrAccessControlClusterAccessControlExtensionChangedEvent adminNodeIDSelector

-- | @- setAdminNodeID:@
setAdminNodeID :: (IsMTRAccessControlClusterAccessControlExtensionChangedEvent mtrAccessControlClusterAccessControlExtensionChangedEvent, IsNSNumber value) => mtrAccessControlClusterAccessControlExtensionChangedEvent -> value -> IO ()
setAdminNodeID mtrAccessControlClusterAccessControlExtensionChangedEvent value =
  sendMessage mtrAccessControlClusterAccessControlExtensionChangedEvent setAdminNodeIDSelector (toNSNumber value)

-- | @- adminPasscodeID@
adminPasscodeID :: IsMTRAccessControlClusterAccessControlExtensionChangedEvent mtrAccessControlClusterAccessControlExtensionChangedEvent => mtrAccessControlClusterAccessControlExtensionChangedEvent -> IO (Id NSNumber)
adminPasscodeID mtrAccessControlClusterAccessControlExtensionChangedEvent =
  sendMessage mtrAccessControlClusterAccessControlExtensionChangedEvent adminPasscodeIDSelector

-- | @- setAdminPasscodeID:@
setAdminPasscodeID :: (IsMTRAccessControlClusterAccessControlExtensionChangedEvent mtrAccessControlClusterAccessControlExtensionChangedEvent, IsNSNumber value) => mtrAccessControlClusterAccessControlExtensionChangedEvent -> value -> IO ()
setAdminPasscodeID mtrAccessControlClusterAccessControlExtensionChangedEvent value =
  sendMessage mtrAccessControlClusterAccessControlExtensionChangedEvent setAdminPasscodeIDSelector (toNSNumber value)

-- | @- changeType@
changeType :: IsMTRAccessControlClusterAccessControlExtensionChangedEvent mtrAccessControlClusterAccessControlExtensionChangedEvent => mtrAccessControlClusterAccessControlExtensionChangedEvent -> IO (Id NSNumber)
changeType mtrAccessControlClusterAccessControlExtensionChangedEvent =
  sendMessage mtrAccessControlClusterAccessControlExtensionChangedEvent changeTypeSelector

-- | @- setChangeType:@
setChangeType :: (IsMTRAccessControlClusterAccessControlExtensionChangedEvent mtrAccessControlClusterAccessControlExtensionChangedEvent, IsNSNumber value) => mtrAccessControlClusterAccessControlExtensionChangedEvent -> value -> IO ()
setChangeType mtrAccessControlClusterAccessControlExtensionChangedEvent value =
  sendMessage mtrAccessControlClusterAccessControlExtensionChangedEvent setChangeTypeSelector (toNSNumber value)

-- | @- latestValue@
latestValue :: IsMTRAccessControlClusterAccessControlExtensionChangedEvent mtrAccessControlClusterAccessControlExtensionChangedEvent => mtrAccessControlClusterAccessControlExtensionChangedEvent -> IO (Id MTRAccessControlClusterAccessControlExtensionStruct)
latestValue mtrAccessControlClusterAccessControlExtensionChangedEvent =
  sendMessage mtrAccessControlClusterAccessControlExtensionChangedEvent latestValueSelector

-- | @- setLatestValue:@
setLatestValue :: (IsMTRAccessControlClusterAccessControlExtensionChangedEvent mtrAccessControlClusterAccessControlExtensionChangedEvent, IsMTRAccessControlClusterAccessControlExtensionStruct value) => mtrAccessControlClusterAccessControlExtensionChangedEvent -> value -> IO ()
setLatestValue mtrAccessControlClusterAccessControlExtensionChangedEvent value =
  sendMessage mtrAccessControlClusterAccessControlExtensionChangedEvent setLatestValueSelector (toMTRAccessControlClusterAccessControlExtensionStruct value)

-- | @- fabricIndex@
fabricIndex :: IsMTRAccessControlClusterAccessControlExtensionChangedEvent mtrAccessControlClusterAccessControlExtensionChangedEvent => mtrAccessControlClusterAccessControlExtensionChangedEvent -> IO (Id NSNumber)
fabricIndex mtrAccessControlClusterAccessControlExtensionChangedEvent =
  sendMessage mtrAccessControlClusterAccessControlExtensionChangedEvent fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRAccessControlClusterAccessControlExtensionChangedEvent mtrAccessControlClusterAccessControlExtensionChangedEvent, IsNSNumber value) => mtrAccessControlClusterAccessControlExtensionChangedEvent -> value -> IO ()
setFabricIndex mtrAccessControlClusterAccessControlExtensionChangedEvent value =
  sendMessage mtrAccessControlClusterAccessControlExtensionChangedEvent setFabricIndexSelector (toNSNumber value)

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
latestValueSelector :: Selector '[] (Id MTRAccessControlClusterAccessControlExtensionStruct)
latestValueSelector = mkSelector "latestValue"

-- | @Selector@ for @setLatestValue:@
setLatestValueSelector :: Selector '[Id MTRAccessControlClusterAccessControlExtensionStruct] ()
setLatestValueSelector = mkSelector "setLatestValue:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

