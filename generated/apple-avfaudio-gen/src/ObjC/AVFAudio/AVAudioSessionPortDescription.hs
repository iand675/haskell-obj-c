{-# LANGUAGE DataKinds #-}
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
  , portName
  , uid
  , hasHardwareVoiceCallProcessing
  , spatialAudioEnabled
  , channels
  , dataSources
  , selectedDataSource
  , preferredDataSource
  , bluetoothMicrophoneExtension
  , bluetoothMicrophoneExtensionSelector
  , channelsSelector
  , dataSourcesSelector
  , hasHardwareVoiceCallProcessingSelector
  , portNameSelector
  , portTypeSelector
  , preferredDataSourceSelector
  , selectedDataSourceSelector
  , setPreferredDataSource_errorSelector
  , spatialAudioEnabledSelector
  , uidSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Select the preferred data source for this port. The input dataSource parameter must be	one of the dataSources exposed by the dataSources property. Setting a nil value will clear the	preference. Note: if the port is part of the active audio route, changing the data source will	likely result in a route reconfiguration.  If the port is not part of the active route,	selecting a new data source will not result in an immediate route reconfiguration.  Use	AVAudioSession's -setPreferredInput:error: method to activate the port.
--
-- ObjC selector: @- setPreferredDataSource:error:@
setPreferredDataSource_error :: (IsAVAudioSessionPortDescription avAudioSessionPortDescription, IsAVAudioSessionDataSourceDescription dataSource, IsNSError outError) => avAudioSessionPortDescription -> dataSource -> outError -> IO Bool
setPreferredDataSource_error avAudioSessionPortDescription dataSource outError =
  sendMessage avAudioSessionPortDescription setPreferredDataSource_errorSelector (toAVAudioSessionDataSourceDescription dataSource) (toNSError outError)

-- | @- portType@
portType :: IsAVAudioSessionPortDescription avAudioSessionPortDescription => avAudioSessionPortDescription -> IO (Id NSString)
portType avAudioSessionPortDescription =
  sendMessage avAudioSessionPortDescription portTypeSelector

-- | A descriptive name for the associated hardware port
--
-- ObjC selector: @- portName@
portName :: IsAVAudioSessionPortDescription avAudioSessionPortDescription => avAudioSessionPortDescription -> IO (Id NSString)
portName avAudioSessionPortDescription =
  sendMessage avAudioSessionPortDescription portNameSelector

-- | A system-assigned unique identifier for the associated hardware port
--
-- ObjC selector: @- UID@
uid :: IsAVAudioSessionPortDescription avAudioSessionPortDescription => avAudioSessionPortDescription -> IO (Id NSString)
uid avAudioSessionPortDescription =
  sendMessage avAudioSessionPortDescription uidSelector

-- | This property's value will be true if the associated hardware port has built-in	processing for two-way voice communication.
--
-- Applications that use their own proprietary voice processing algorithms should use this property	to decide when to disable processing.  On the other hand, if using Apple's Voice Processing I/O	unit (subtype kAudioUnitSubType_VoiceProcessingIO), the system will automatically manage this	for the application. In particular, ports of type AVAudioSessionPortBluetoothHFP and	AVAudioSessionPortCarAudio often have hardware voice processing.
--
-- ObjC selector: @- hasHardwareVoiceCallProcessing@
hasHardwareVoiceCallProcessing :: IsAVAudioSessionPortDescription avAudioSessionPortDescription => avAudioSessionPortDescription -> IO Bool
hasHardwareVoiceCallProcessing avAudioSessionPortDescription =
  sendMessage avAudioSessionPortDescription hasHardwareVoiceCallProcessingSelector

-- | This property's value will be true if the port supports spatial audio playback and the feature is    enabled.
--
-- 'Now Playing' apps should also inform the system if they support multichannel audio content using    -setSupportsMultichannelContent:error: method. Apps may also register to receive the    AVAudioSessionSpatialPlaybackCapabilitiesChanged notification to detect changes in user preferences that    affect spatial audio playback.
--
-- This property is only relevant in the context of ports that have a small number of hardware channels    (typically 2), but have enhanced capabilities for rendering multi-channel content. Note that some port    types such as USB and HDMI may support multi-channel playback because they have hardware formats supporting    more than 2 channels. For example, many HDMI receivers are connected to multiple speakers and are capable of    rendering 5.1, 7.1, or other popular surround sound formats. Applications interested in utilizing multi-channel    formats should also query AVAudioSession's maximumOutputNumberOfChannels property and make use of    -setPreferredOutputNumberOfChannels:error: to set the preferred number of hardware channels.
--
-- ObjC selector: @- spatialAudioEnabled@
spatialAudioEnabled :: IsAVAudioSessionPortDescription avAudioSessionPortDescription => avAudioSessionPortDescription -> IO Bool
spatialAudioEnabled avAudioSessionPortDescription =
  sendMessage avAudioSessionPortDescription spatialAudioEnabledSelector

-- | @- channels@
channels :: IsAVAudioSessionPortDescription avAudioSessionPortDescription => avAudioSessionPortDescription -> IO (Id NSArray)
channels avAudioSessionPortDescription =
  sendMessage avAudioSessionPortDescription channelsSelector

-- | Will be nil if there are no selectable data sources.
--
-- ObjC selector: @- dataSources@
dataSources :: IsAVAudioSessionPortDescription avAudioSessionPortDescription => avAudioSessionPortDescription -> IO (Id NSArray)
dataSources avAudioSessionPortDescription =
  sendMessage avAudioSessionPortDescription dataSourcesSelector

-- | Will be nil if there are no selectable data sources. In all other cases, this property reflects the currently selected data source.
--
-- ObjC selector: @- selectedDataSource@
selectedDataSource :: IsAVAudioSessionPortDescription avAudioSessionPortDescription => avAudioSessionPortDescription -> IO (Id AVAudioSessionDataSourceDescription)
selectedDataSource avAudioSessionPortDescription =
  sendMessage avAudioSessionPortDescription selectedDataSourceSelector

-- | This property reflects the application's preferred data source for the Port. Will be nil if there are no selectable data sources or if no preference has been set.
--
-- ObjC selector: @- preferredDataSource@
preferredDataSource :: IsAVAudioSessionPortDescription avAudioSessionPortDescription => avAudioSessionPortDescription -> IO (Id AVAudioSessionDataSourceDescription)
preferredDataSource avAudioSessionPortDescription =
  sendMessage avAudioSessionPortDescription preferredDataSourceSelector

-- | An optional port extension that describes capabilities relevant to Bluetooth microphone ports.
--
-- This property is optional and will be @nil@ for all ports for which this capability set doesn't apply.
--
-- ObjC selector: @- bluetoothMicrophoneExtension@
bluetoothMicrophoneExtension :: IsAVAudioSessionPortDescription avAudioSessionPortDescription => avAudioSessionPortDescription -> IO (Id AVAudioSessionPortExtensionBluetoothMicrophone)
bluetoothMicrophoneExtension avAudioSessionPortDescription =
  sendMessage avAudioSessionPortDescription bluetoothMicrophoneExtensionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setPreferredDataSource:error:@
setPreferredDataSource_errorSelector :: Selector '[Id AVAudioSessionDataSourceDescription, Id NSError] Bool
setPreferredDataSource_errorSelector = mkSelector "setPreferredDataSource:error:"

-- | @Selector@ for @portType@
portTypeSelector :: Selector '[] (Id NSString)
portTypeSelector = mkSelector "portType"

-- | @Selector@ for @portName@
portNameSelector :: Selector '[] (Id NSString)
portNameSelector = mkSelector "portName"

-- | @Selector@ for @UID@
uidSelector :: Selector '[] (Id NSString)
uidSelector = mkSelector "UID"

-- | @Selector@ for @hasHardwareVoiceCallProcessing@
hasHardwareVoiceCallProcessingSelector :: Selector '[] Bool
hasHardwareVoiceCallProcessingSelector = mkSelector "hasHardwareVoiceCallProcessing"

-- | @Selector@ for @spatialAudioEnabled@
spatialAudioEnabledSelector :: Selector '[] Bool
spatialAudioEnabledSelector = mkSelector "spatialAudioEnabled"

-- | @Selector@ for @channels@
channelsSelector :: Selector '[] (Id NSArray)
channelsSelector = mkSelector "channels"

-- | @Selector@ for @dataSources@
dataSourcesSelector :: Selector '[] (Id NSArray)
dataSourcesSelector = mkSelector "dataSources"

-- | @Selector@ for @selectedDataSource@
selectedDataSourceSelector :: Selector '[] (Id AVAudioSessionDataSourceDescription)
selectedDataSourceSelector = mkSelector "selectedDataSource"

-- | @Selector@ for @preferredDataSource@
preferredDataSourceSelector :: Selector '[] (Id AVAudioSessionDataSourceDescription)
preferredDataSourceSelector = mkSelector "preferredDataSource"

-- | @Selector@ for @bluetoothMicrophoneExtension@
bluetoothMicrophoneExtensionSelector :: Selector '[] (Id AVAudioSessionPortExtensionBluetoothMicrophone)
bluetoothMicrophoneExtensionSelector = mkSelector "bluetoothMicrophoneExtension"

