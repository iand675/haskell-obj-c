{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadNetworkDirectoryClusterThreadNetworkStruct@.
module ObjC.Matter.MTRThreadNetworkDirectoryClusterThreadNetworkStruct
  ( MTRThreadNetworkDirectoryClusterThreadNetworkStruct
  , IsMTRThreadNetworkDirectoryClusterThreadNetworkStruct(..)
  , extendedPanID
  , setExtendedPanID
  , networkName
  , setNetworkName
  , channel
  , setChannel
  , activeTimestamp
  , setActiveTimestamp
  , activeTimestampSelector
  , channelSelector
  , extendedPanIDSelector
  , networkNameSelector
  , setActiveTimestampSelector
  , setChannelSelector
  , setExtendedPanIDSelector
  , setNetworkNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- extendedPanID@
extendedPanID :: IsMTRThreadNetworkDirectoryClusterThreadNetworkStruct mtrThreadNetworkDirectoryClusterThreadNetworkStruct => mtrThreadNetworkDirectoryClusterThreadNetworkStruct -> IO (Id NSData)
extendedPanID mtrThreadNetworkDirectoryClusterThreadNetworkStruct =
  sendMessage mtrThreadNetworkDirectoryClusterThreadNetworkStruct extendedPanIDSelector

-- | @- setExtendedPanID:@
setExtendedPanID :: (IsMTRThreadNetworkDirectoryClusterThreadNetworkStruct mtrThreadNetworkDirectoryClusterThreadNetworkStruct, IsNSData value) => mtrThreadNetworkDirectoryClusterThreadNetworkStruct -> value -> IO ()
setExtendedPanID mtrThreadNetworkDirectoryClusterThreadNetworkStruct value =
  sendMessage mtrThreadNetworkDirectoryClusterThreadNetworkStruct setExtendedPanIDSelector (toNSData value)

-- | @- networkName@
networkName :: IsMTRThreadNetworkDirectoryClusterThreadNetworkStruct mtrThreadNetworkDirectoryClusterThreadNetworkStruct => mtrThreadNetworkDirectoryClusterThreadNetworkStruct -> IO (Id NSString)
networkName mtrThreadNetworkDirectoryClusterThreadNetworkStruct =
  sendMessage mtrThreadNetworkDirectoryClusterThreadNetworkStruct networkNameSelector

-- | @- setNetworkName:@
setNetworkName :: (IsMTRThreadNetworkDirectoryClusterThreadNetworkStruct mtrThreadNetworkDirectoryClusterThreadNetworkStruct, IsNSString value) => mtrThreadNetworkDirectoryClusterThreadNetworkStruct -> value -> IO ()
setNetworkName mtrThreadNetworkDirectoryClusterThreadNetworkStruct value =
  sendMessage mtrThreadNetworkDirectoryClusterThreadNetworkStruct setNetworkNameSelector (toNSString value)

-- | @- channel@
channel :: IsMTRThreadNetworkDirectoryClusterThreadNetworkStruct mtrThreadNetworkDirectoryClusterThreadNetworkStruct => mtrThreadNetworkDirectoryClusterThreadNetworkStruct -> IO (Id NSNumber)
channel mtrThreadNetworkDirectoryClusterThreadNetworkStruct =
  sendMessage mtrThreadNetworkDirectoryClusterThreadNetworkStruct channelSelector

-- | @- setChannel:@
setChannel :: (IsMTRThreadNetworkDirectoryClusterThreadNetworkStruct mtrThreadNetworkDirectoryClusterThreadNetworkStruct, IsNSNumber value) => mtrThreadNetworkDirectoryClusterThreadNetworkStruct -> value -> IO ()
setChannel mtrThreadNetworkDirectoryClusterThreadNetworkStruct value =
  sendMessage mtrThreadNetworkDirectoryClusterThreadNetworkStruct setChannelSelector (toNSNumber value)

-- | @- activeTimestamp@
activeTimestamp :: IsMTRThreadNetworkDirectoryClusterThreadNetworkStruct mtrThreadNetworkDirectoryClusterThreadNetworkStruct => mtrThreadNetworkDirectoryClusterThreadNetworkStruct -> IO (Id NSNumber)
activeTimestamp mtrThreadNetworkDirectoryClusterThreadNetworkStruct =
  sendMessage mtrThreadNetworkDirectoryClusterThreadNetworkStruct activeTimestampSelector

-- | @- setActiveTimestamp:@
setActiveTimestamp :: (IsMTRThreadNetworkDirectoryClusterThreadNetworkStruct mtrThreadNetworkDirectoryClusterThreadNetworkStruct, IsNSNumber value) => mtrThreadNetworkDirectoryClusterThreadNetworkStruct -> value -> IO ()
setActiveTimestamp mtrThreadNetworkDirectoryClusterThreadNetworkStruct value =
  sendMessage mtrThreadNetworkDirectoryClusterThreadNetworkStruct setActiveTimestampSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @extendedPanID@
extendedPanIDSelector :: Selector '[] (Id NSData)
extendedPanIDSelector = mkSelector "extendedPanID"

-- | @Selector@ for @setExtendedPanID:@
setExtendedPanIDSelector :: Selector '[Id NSData] ()
setExtendedPanIDSelector = mkSelector "setExtendedPanID:"

-- | @Selector@ for @networkName@
networkNameSelector :: Selector '[] (Id NSString)
networkNameSelector = mkSelector "networkName"

-- | @Selector@ for @setNetworkName:@
setNetworkNameSelector :: Selector '[Id NSString] ()
setNetworkNameSelector = mkSelector "setNetworkName:"

-- | @Selector@ for @channel@
channelSelector :: Selector '[] (Id NSNumber)
channelSelector = mkSelector "channel"

-- | @Selector@ for @setChannel:@
setChannelSelector :: Selector '[Id NSNumber] ()
setChannelSelector = mkSelector "setChannel:"

-- | @Selector@ for @activeTimestamp@
activeTimestampSelector :: Selector '[] (Id NSNumber)
activeTimestampSelector = mkSelector "activeTimestamp"

-- | @Selector@ for @setActiveTimestamp:@
setActiveTimestampSelector :: Selector '[Id NSNumber] ()
setActiveTimestampSelector = mkSelector "setActiveTimestamp:"

