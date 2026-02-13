{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterAudioCapabilitiesStruct@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterAudioCapabilitiesStruct
  ( MTRCameraAVStreamManagementClusterAudioCapabilitiesStruct
  , IsMTRCameraAVStreamManagementClusterAudioCapabilitiesStruct(..)
  , maxNumberOfChannels
  , setMaxNumberOfChannels
  , supportedCodecs
  , setSupportedCodecs
  , supportedSampleRates
  , setSupportedSampleRates
  , supportedBitDepths
  , setSupportedBitDepths
  , maxNumberOfChannelsSelector
  , setMaxNumberOfChannelsSelector
  , setSupportedBitDepthsSelector
  , setSupportedCodecsSelector
  , setSupportedSampleRatesSelector
  , supportedBitDepthsSelector
  , supportedCodecsSelector
  , supportedSampleRatesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- maxNumberOfChannels@
maxNumberOfChannels :: IsMTRCameraAVStreamManagementClusterAudioCapabilitiesStruct mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct => mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct -> IO (Id NSNumber)
maxNumberOfChannels mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct =
  sendMessage mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct maxNumberOfChannelsSelector

-- | @- setMaxNumberOfChannels:@
setMaxNumberOfChannels :: (IsMTRCameraAVStreamManagementClusterAudioCapabilitiesStruct mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct -> value -> IO ()
setMaxNumberOfChannels mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct value =
  sendMessage mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct setMaxNumberOfChannelsSelector (toNSNumber value)

-- | @- supportedCodecs@
supportedCodecs :: IsMTRCameraAVStreamManagementClusterAudioCapabilitiesStruct mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct => mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct -> IO (Id NSArray)
supportedCodecs mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct =
  sendMessage mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct supportedCodecsSelector

-- | @- setSupportedCodecs:@
setSupportedCodecs :: (IsMTRCameraAVStreamManagementClusterAudioCapabilitiesStruct mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct, IsNSArray value) => mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct -> value -> IO ()
setSupportedCodecs mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct value =
  sendMessage mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct setSupportedCodecsSelector (toNSArray value)

-- | @- supportedSampleRates@
supportedSampleRates :: IsMTRCameraAVStreamManagementClusterAudioCapabilitiesStruct mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct => mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct -> IO (Id NSArray)
supportedSampleRates mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct =
  sendMessage mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct supportedSampleRatesSelector

-- | @- setSupportedSampleRates:@
setSupportedSampleRates :: (IsMTRCameraAVStreamManagementClusterAudioCapabilitiesStruct mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct, IsNSArray value) => mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct -> value -> IO ()
setSupportedSampleRates mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct value =
  sendMessage mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct setSupportedSampleRatesSelector (toNSArray value)

-- | @- supportedBitDepths@
supportedBitDepths :: IsMTRCameraAVStreamManagementClusterAudioCapabilitiesStruct mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct => mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct -> IO (Id NSArray)
supportedBitDepths mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct =
  sendMessage mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct supportedBitDepthsSelector

-- | @- setSupportedBitDepths:@
setSupportedBitDepths :: (IsMTRCameraAVStreamManagementClusterAudioCapabilitiesStruct mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct, IsNSArray value) => mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct -> value -> IO ()
setSupportedBitDepths mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct value =
  sendMessage mtrCameraAVStreamManagementClusterAudioCapabilitiesStruct setSupportedBitDepthsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @maxNumberOfChannels@
maxNumberOfChannelsSelector :: Selector '[] (Id NSNumber)
maxNumberOfChannelsSelector = mkSelector "maxNumberOfChannels"

-- | @Selector@ for @setMaxNumberOfChannels:@
setMaxNumberOfChannelsSelector :: Selector '[Id NSNumber] ()
setMaxNumberOfChannelsSelector = mkSelector "setMaxNumberOfChannels:"

-- | @Selector@ for @supportedCodecs@
supportedCodecsSelector :: Selector '[] (Id NSArray)
supportedCodecsSelector = mkSelector "supportedCodecs"

-- | @Selector@ for @setSupportedCodecs:@
setSupportedCodecsSelector :: Selector '[Id NSArray] ()
setSupportedCodecsSelector = mkSelector "setSupportedCodecs:"

-- | @Selector@ for @supportedSampleRates@
supportedSampleRatesSelector :: Selector '[] (Id NSArray)
supportedSampleRatesSelector = mkSelector "supportedSampleRates"

-- | @Selector@ for @setSupportedSampleRates:@
setSupportedSampleRatesSelector :: Selector '[Id NSArray] ()
setSupportedSampleRatesSelector = mkSelector "setSupportedSampleRates:"

-- | @Selector@ for @supportedBitDepths@
supportedBitDepthsSelector :: Selector '[] (Id NSArray)
supportedBitDepthsSelector = mkSelector "supportedBitDepths"

-- | @Selector@ for @setSupportedBitDepths:@
setSupportedBitDepthsSelector :: Selector '[Id NSArray] ()
setSupportedBitDepthsSelector = mkSelector "setSupportedBitDepths:"

