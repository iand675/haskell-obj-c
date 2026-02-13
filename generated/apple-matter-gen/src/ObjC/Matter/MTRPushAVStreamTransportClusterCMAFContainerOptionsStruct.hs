{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterCMAFContainerOptionsStruct@.
module ObjC.Matter.MTRPushAVStreamTransportClusterCMAFContainerOptionsStruct
  ( MTRPushAVStreamTransportClusterCMAFContainerOptionsStruct
  , IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct(..)
  , cmafInterface
  , setCmafInterface
  , segmentDuration
  , setSegmentDuration
  , chunkDuration
  , setChunkDuration
  , sessionGroup
  , setSessionGroup
  , trackName
  , setTrackName
  , cencKey
  , setCencKey
  , cencKeyID
  , setCencKeyID
  , metadataEnabled
  , setMetadataEnabled
  , cencKeyIDSelector
  , cencKeySelector
  , chunkDurationSelector
  , cmafInterfaceSelector
  , metadataEnabledSelector
  , segmentDurationSelector
  , sessionGroupSelector
  , setCencKeyIDSelector
  , setCencKeySelector
  , setChunkDurationSelector
  , setCmafInterfaceSelector
  , setMetadataEnabledSelector
  , setSegmentDurationSelector
  , setSessionGroupSelector
  , setTrackNameSelector
  , trackNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- cmafInterface@
cmafInterface :: IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> IO (Id NSNumber)
cmafInterface mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct cmafInterfaceSelector

-- | @- setCmafInterface:@
setCmafInterface :: (IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> value -> IO ()
setCmafInterface mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct setCmafInterfaceSelector (toNSNumber value)

-- | @- segmentDuration@
segmentDuration :: IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> IO (Id NSNumber)
segmentDuration mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct segmentDurationSelector

-- | @- setSegmentDuration:@
setSegmentDuration :: (IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> value -> IO ()
setSegmentDuration mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct setSegmentDurationSelector (toNSNumber value)

-- | @- chunkDuration@
chunkDuration :: IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> IO (Id NSNumber)
chunkDuration mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct chunkDurationSelector

-- | @- setChunkDuration:@
setChunkDuration :: (IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> value -> IO ()
setChunkDuration mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct setChunkDurationSelector (toNSNumber value)

-- | @- sessionGroup@
sessionGroup :: IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> IO (Id NSNumber)
sessionGroup mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct sessionGroupSelector

-- | @- setSessionGroup:@
setSessionGroup :: (IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> value -> IO ()
setSessionGroup mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct setSessionGroupSelector (toNSNumber value)

-- | @- trackName@
trackName :: IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> IO (Id NSString)
trackName mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct trackNameSelector

-- | @- setTrackName:@
setTrackName :: (IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct, IsNSString value) => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> value -> IO ()
setTrackName mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct setTrackNameSelector (toNSString value)

-- | @- cencKey@
cencKey :: IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> IO (Id NSData)
cencKey mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct cencKeySelector

-- | @- setCencKey:@
setCencKey :: (IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct, IsNSData value) => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> value -> IO ()
setCencKey mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct setCencKeySelector (toNSData value)

-- | @- cencKeyID@
cencKeyID :: IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> IO (Id NSData)
cencKeyID mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct cencKeyIDSelector

-- | @- setCencKeyID:@
setCencKeyID :: (IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct, IsNSData value) => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> value -> IO ()
setCencKeyID mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct setCencKeyIDSelector (toNSData value)

-- | @- metadataEnabled@
metadataEnabled :: IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> IO (Id NSNumber)
metadataEnabled mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct metadataEnabledSelector

-- | @- setMetadataEnabled:@
setMetadataEnabled :: (IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct -> value -> IO ()
setMetadataEnabled mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterCMAFContainerOptionsStruct setMetadataEnabledSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cmafInterface@
cmafInterfaceSelector :: Selector '[] (Id NSNumber)
cmafInterfaceSelector = mkSelector "cmafInterface"

-- | @Selector@ for @setCmafInterface:@
setCmafInterfaceSelector :: Selector '[Id NSNumber] ()
setCmafInterfaceSelector = mkSelector "setCmafInterface:"

-- | @Selector@ for @segmentDuration@
segmentDurationSelector :: Selector '[] (Id NSNumber)
segmentDurationSelector = mkSelector "segmentDuration"

-- | @Selector@ for @setSegmentDuration:@
setSegmentDurationSelector :: Selector '[Id NSNumber] ()
setSegmentDurationSelector = mkSelector "setSegmentDuration:"

-- | @Selector@ for @chunkDuration@
chunkDurationSelector :: Selector '[] (Id NSNumber)
chunkDurationSelector = mkSelector "chunkDuration"

-- | @Selector@ for @setChunkDuration:@
setChunkDurationSelector :: Selector '[Id NSNumber] ()
setChunkDurationSelector = mkSelector "setChunkDuration:"

-- | @Selector@ for @sessionGroup@
sessionGroupSelector :: Selector '[] (Id NSNumber)
sessionGroupSelector = mkSelector "sessionGroup"

-- | @Selector@ for @setSessionGroup:@
setSessionGroupSelector :: Selector '[Id NSNumber] ()
setSessionGroupSelector = mkSelector "setSessionGroup:"

-- | @Selector@ for @trackName@
trackNameSelector :: Selector '[] (Id NSString)
trackNameSelector = mkSelector "trackName"

-- | @Selector@ for @setTrackName:@
setTrackNameSelector :: Selector '[Id NSString] ()
setTrackNameSelector = mkSelector "setTrackName:"

-- | @Selector@ for @cencKey@
cencKeySelector :: Selector '[] (Id NSData)
cencKeySelector = mkSelector "cencKey"

-- | @Selector@ for @setCencKey:@
setCencKeySelector :: Selector '[Id NSData] ()
setCencKeySelector = mkSelector "setCencKey:"

-- | @Selector@ for @cencKeyID@
cencKeyIDSelector :: Selector '[] (Id NSData)
cencKeyIDSelector = mkSelector "cencKeyID"

-- | @Selector@ for @setCencKeyID:@
setCencKeyIDSelector :: Selector '[Id NSData] ()
setCencKeyIDSelector = mkSelector "setCencKeyID:"

-- | @Selector@ for @metadataEnabled@
metadataEnabledSelector :: Selector '[] (Id NSNumber)
metadataEnabledSelector = mkSelector "metadataEnabled"

-- | @Selector@ for @setMetadataEnabled:@
setMetadataEnabledSelector :: Selector '[Id NSNumber] ()
setMetadataEnabledSelector = mkSelector "setMetadataEnabled:"

