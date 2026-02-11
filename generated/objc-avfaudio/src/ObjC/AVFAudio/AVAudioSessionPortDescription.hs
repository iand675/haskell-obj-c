{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Information about a port, a physical connector or audio device.
--
-- Generated bindings for @AVAudioSessionPortDescription@.
module ObjC.AVFAudio.AVAudioSessionPortDescription
  ( AVAudioSessionPortDescription
  , IsAVAudioSessionPortDescription(..)
  , setPreferredDataSource_error
  , portType
  , hasHardwareVoiceCallProcessing
  , spatialAudioEnabled
  , dataSources
  , selectedDataSource
  , preferredDataSource
  , setPreferredDataSource_errorSelector
  , portTypeSelector
  , hasHardwareVoiceCallProcessingSelector
  , spatialAudioEnabledSelector
  , dataSourcesSelector
  , selectedDataSourceSelector
  , preferredDataSourceSelector


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

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Select the preferred data source for this port. The input dataSource parameter must be	one of the dataSources exposed by the dataSources property. Setting a nil value will clear the	preference. Note: if the port is part of the active audio route, changing the data source will	likely result in a route reconfiguration.  If the port is not part of the active route,	selecting a new data source will not result in an immediate route reconfiguration.  Use	AVAudioSession's -setPreferredInput:error: method to activate the port.
--
-- ObjC selector: @- setPreferredDataSource:error:@
setPreferredDataSource_error :: (IsAVAudioSessionPortDescription avAudioSessionPortDescription, IsAVAudioSessionDataSourceDescription dataSource, IsNSError outError) => avAudioSessionPortDescription -> dataSource -> outError -> IO Bool
setPreferredDataSource_error avAudioSessionPortDescription  dataSource outError =
withObjCPtr dataSource $ \raw_dataSource ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSessionPortDescription (mkSelector "setPreferredDataSource:error:") retCULong [argPtr (castPtr raw_dataSource :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- portType@
portType :: IsAVAudioSessionPortDescription avAudioSessionPortDescription => avAudioSessionPortDescription -> IO (Id NSString)
portType avAudioSessionPortDescription  =
  sendMsg avAudioSessionPortDescription (mkSelector "portType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | This property's value will be true if the associated hardware port has built-in	processing for two-way voice communication.
--
-- Applications that use their own proprietary voice processing algorithms should use this property	to decide when to disable processing.  On the other hand, if using Apple's Voice Processing I/O	unit (subtype kAudioUnitSubType_VoiceProcessingIO), the system will automatically manage this	for the application. In particular, ports of type AVAudioSessionPortBluetoothHFP and	AVAudioSessionPortCarAudio often have hardware voice processing.
--
-- ObjC selector: @- hasHardwareVoiceCallProcessing@
hasHardwareVoiceCallProcessing :: IsAVAudioSessionPortDescription avAudioSessionPortDescription => avAudioSessionPortDescription -> IO Bool
hasHardwareVoiceCallProcessing avAudioSessionPortDescription  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSessionPortDescription (mkSelector "hasHardwareVoiceCallProcessing") retCULong []

-- | This property's value will be true if the port supports spatial audio playback and the feature is    enabled.
--
-- 'Now Playing' apps should also inform the system if they support multichannel audio content using    -setSupportsMultichannelContent:error: method. Apps may also register to receive the    AVAudioSessionSpatialPlaybackCapabilitiesChanged notification to detect changes in user preferences that    affect spatial audio playback.
--
-- This property is only relevant in the context of ports that have a small number of hardware channels    (typically 2), but have enhanced capabilities for rendering multi-channel content. Note that some port    types such as USB and HDMI may support multi-channel playback because they have hardware formats supporting    more than 2 channels. For example, many HDMI receivers are connected to multiple speakers and are capable of    rendering 5.1, 7.1, or other popular surround sound formats. Applications interested in utilizing multi-channel    formats should also query AVAudioSession's maximumOutputNumberOfChannels property and make use of    -setPreferredOutputNumberOfChannels:error: to set the preferred number of hardware channels.
--
-- ObjC selector: @- spatialAudioEnabled@
spatialAudioEnabled :: IsAVAudioSessionPortDescription avAudioSessionPortDescription => avAudioSessionPortDescription -> IO Bool
spatialAudioEnabled avAudioSessionPortDescription  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioSessionPortDescription (mkSelector "spatialAudioEnabled") retCULong []

-- | Will be nil if there are no selectable data sources.
--
-- ObjC selector: @- dataSources@
dataSources :: IsAVAudioSessionPortDescription avAudioSessionPortDescription => avAudioSessionPortDescription -> IO (Id NSArray)
dataSources avAudioSessionPortDescription  =
  sendMsg avAudioSessionPortDescription (mkSelector "dataSources") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Will be nil if there are no selectable data sources. In all other cases, this property reflects the currently selected data source.
--
-- ObjC selector: @- selectedDataSource@
selectedDataSource :: IsAVAudioSessionPortDescription avAudioSessionPortDescription => avAudioSessionPortDescription -> IO (Id AVAudioSessionDataSourceDescription)
selectedDataSource avAudioSessionPortDescription  =
  sendMsg avAudioSessionPortDescription (mkSelector "selectedDataSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | This property reflects the application's preferred data source for the Port. Will be nil if there are no selectable data sources or if no preference has been set.
--
-- ObjC selector: @- preferredDataSource@
preferredDataSource :: IsAVAudioSessionPortDescription avAudioSessionPortDescription => avAudioSessionPortDescription -> IO (Id AVAudioSessionDataSourceDescription)
preferredDataSource avAudioSessionPortDescription  =
  sendMsg avAudioSessionPortDescription (mkSelector "preferredDataSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setPreferredDataSource:error:@
setPreferredDataSource_errorSelector :: Selector
setPreferredDataSource_errorSelector = mkSelector "setPreferredDataSource:error:"

-- | @Selector@ for @portType@
portTypeSelector :: Selector
portTypeSelector = mkSelector "portType"

-- | @Selector@ for @hasHardwareVoiceCallProcessing@
hasHardwareVoiceCallProcessingSelector :: Selector
hasHardwareVoiceCallProcessingSelector = mkSelector "hasHardwareVoiceCallProcessing"

-- | @Selector@ for @spatialAudioEnabled@
spatialAudioEnabledSelector :: Selector
spatialAudioEnabledSelector = mkSelector "spatialAudioEnabled"

-- | @Selector@ for @dataSources@
dataSourcesSelector :: Selector
dataSourcesSelector = mkSelector "dataSources"

-- | @Selector@ for @selectedDataSource@
selectedDataSourceSelector :: Selector
selectedDataSourceSelector = mkSelector "selectedDataSource"

-- | @Selector@ for @preferredDataSource@
preferredDataSourceSelector :: Selector
preferredDataSourceSelector = mkSelector "preferredDataSource"

