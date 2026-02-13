{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioEnvironmentNode
--
-- Mixer node that simulates a 3D environment
--
-- AVAudioEnvironmentNode is a mixer node that simulates a 3D audio environment. Any node that         conforms to the AVAudioMixing protocol (e.g. AVAudioPlayerNode) can act as a source in this        environment.
--
-- The environment has an implicit "listener". By controlling the listener's position and        orientation, the application controls the way the user experiences the virtual world.         In addition, this node also defines properties for distance attenuation and reverberation         that help characterize the environment.
--
-- It is important to note that AVAudio3DMixingSourceMode affects how inputs with different channel        configurations are rendered. By default, only inputs with a mono channel are spatialized.
--
-- In order to set the environment node’s output to a multichannel format, use an AVAudioFormat        with a desired AudioChannelLayout.
--
-- Generated bindings for @AVAudioEnvironmentNode@.
module ObjC.AVFAudio.AVAudioEnvironmentNode
  ( AVAudioEnvironmentNode
  , IsAVAudioEnvironmentNode(..)
  , init_
  , outputType
  , setOutputType
  , outputVolume
  , setOutputVolume
  , nextAvailableInputBus
  , listenerPosition
  , setListenerPosition
  , listenerVectorOrientation
  , setListenerVectorOrientation
  , listenerAngularOrientation
  , setListenerAngularOrientation
  , distanceAttenuationParameters
  , reverbParameters
  , applicableRenderingAlgorithms
  , listenerHeadTrackingEnabled
  , setListenerHeadTrackingEnabled
  , applicableRenderingAlgorithmsSelector
  , distanceAttenuationParametersSelector
  , initSelector
  , listenerAngularOrientationSelector
  , listenerHeadTrackingEnabledSelector
  , listenerPositionSelector
  , listenerVectorOrientationSelector
  , nextAvailableInputBusSelector
  , outputTypeSelector
  , outputVolumeSelector
  , reverbParametersSelector
  , setListenerAngularOrientationSelector
  , setListenerHeadTrackingEnabledSelector
  , setListenerPositionSelector
  , setListenerVectorOrientationSelector
  , setOutputTypeSelector
  , setOutputVolumeSelector

  -- * Enum types
  , AVAudioEnvironmentOutputType(AVAudioEnvironmentOutputType)
  , pattern AVAudioEnvironmentOutputTypeAuto
  , pattern AVAudioEnvironmentOutputTypeHeadphones
  , pattern AVAudioEnvironmentOutputTypeBuiltInSpeakers
  , pattern AVAudioEnvironmentOutputTypeExternalSpeakers

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.AVFAudio.Internal.Structs
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> IO (Id AVAudioEnvironmentNode)
init_ avAudioEnvironmentNode =
  sendOwnedMessage avAudioEnvironmentNode initSelector

-- | outputType
--
-- Type of output hardware to be used with AVAudio3DMixingRenderingAlgorithmAuto
--
-- Output hardware cannot be automatically determined in Manual Rendering modes or for wired        output. This property can be used to override the output type if the correct type is known.
--
-- Selecting an output type that does not match the actual hardware can produce unexpected        results, especially with AVAudioEnvironmentOutputTypeBuiltInSpeakers. An app choosing        a value other than AVAudio3DMixingOutputTypeAuto should listen to route change        notifications and update the output type accordingly.
--
-- Default:    AVAudio3DMixingOutputTypeAuto
--
-- ObjC selector: @- outputType@
outputType :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> IO AVAudioEnvironmentOutputType
outputType avAudioEnvironmentNode =
  sendMessage avAudioEnvironmentNode outputTypeSelector

-- | outputType
--
-- Type of output hardware to be used with AVAudio3DMixingRenderingAlgorithmAuto
--
-- Output hardware cannot be automatically determined in Manual Rendering modes or for wired        output. This property can be used to override the output type if the correct type is known.
--
-- Selecting an output type that does not match the actual hardware can produce unexpected        results, especially with AVAudioEnvironmentOutputTypeBuiltInSpeakers. An app choosing        a value other than AVAudio3DMixingOutputTypeAuto should listen to route change        notifications and update the output type accordingly.
--
-- Default:    AVAudio3DMixingOutputTypeAuto
--
-- ObjC selector: @- setOutputType:@
setOutputType :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> AVAudioEnvironmentOutputType -> IO ()
setOutputType avAudioEnvironmentNode value =
  sendMessage avAudioEnvironmentNode setOutputTypeSelector value

-- | outputVolume
--
-- The mixer's output volume.
--
-- This accesses the mixer's output volume (0.0-1.0, inclusive).
--
-- ObjC selector: @- outputVolume@
outputVolume :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> IO CFloat
outputVolume avAudioEnvironmentNode =
  sendMessage avAudioEnvironmentNode outputVolumeSelector

-- | outputVolume
--
-- The mixer's output volume.
--
-- This accesses the mixer's output volume (0.0-1.0, inclusive).
--
-- ObjC selector: @- setOutputVolume:@
setOutputVolume :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> CFloat -> IO ()
setOutputVolume avAudioEnvironmentNode value =
  sendMessage avAudioEnvironmentNode setOutputVolumeSelector value

-- | nextAvailableInputBus
--
-- Find an unused input bus
--
-- This will find and return the first input bus to which no other node is connected.
--
-- ObjC selector: @- nextAvailableInputBus@
nextAvailableInputBus :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> IO CULong
nextAvailableInputBus avAudioEnvironmentNode =
  sendMessage avAudioEnvironmentNode nextAvailableInputBusSelector

-- | listenerPosition
--
-- Sets the listener's position in the 3D environment
--
-- The coordinates are specified in meters.        Default:            The default position of the listener is at the origin.            x: 0.0            y: 0.0            z: 0.0
--
-- ObjC selector: @- listenerPosition@
listenerPosition :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> IO AVAudio3DPoint
listenerPosition avAudioEnvironmentNode =
  sendMessage avAudioEnvironmentNode listenerPositionSelector

-- | listenerPosition
--
-- Sets the listener's position in the 3D environment
--
-- The coordinates are specified in meters.        Default:            The default position of the listener is at the origin.            x: 0.0            y: 0.0            z: 0.0
--
-- ObjC selector: @- setListenerPosition:@
setListenerPosition :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> AVAudio3DPoint -> IO ()
setListenerPosition avAudioEnvironmentNode value =
  sendMessage avAudioEnvironmentNode setListenerPositionSelector value

-- | listenerVectorOrientation
--
-- The listener's orientation in the environment
--
-- Changing listenerVectorOrientation will result in a corresponding change in listenerAngularOrientation.        Default:            The default orientation is with the listener looking directly along the negative Z axis.            forward: (0, 0, -1)            up:      (0, 1, 0)
--
-- ObjC selector: @- listenerVectorOrientation@
listenerVectorOrientation :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> IO AVAudio3DVectorOrientation
listenerVectorOrientation avAudioEnvironmentNode =
  sendMessage avAudioEnvironmentNode listenerVectorOrientationSelector

-- | listenerVectorOrientation
--
-- The listener's orientation in the environment
--
-- Changing listenerVectorOrientation will result in a corresponding change in listenerAngularOrientation.        Default:            The default orientation is with the listener looking directly along the negative Z axis.            forward: (0, 0, -1)            up:      (0, 1, 0)
--
-- ObjC selector: @- setListenerVectorOrientation:@
setListenerVectorOrientation :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> AVAudio3DVectorOrientation -> IO ()
setListenerVectorOrientation avAudioEnvironmentNode value =
  sendMessage avAudioEnvironmentNode setListenerVectorOrientationSelector value

-- | listenerAngularOrientation
--
-- The listener's orientation in the environment
--
-- Changing listenerAngularOrientation will result in a corresponding change in listenerVectorOrientation.        All angles are specified in degrees.        Default:            The default orientation is with the listener looking directly along the negative Z axis.            yaw: 0.0            pitch: 0.0            roll: 0.0
--
-- ObjC selector: @- listenerAngularOrientation@
listenerAngularOrientation :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> IO AVAudio3DAngularOrientation
listenerAngularOrientation avAudioEnvironmentNode =
  sendMessage avAudioEnvironmentNode listenerAngularOrientationSelector

-- | listenerAngularOrientation
--
-- The listener's orientation in the environment
--
-- Changing listenerAngularOrientation will result in a corresponding change in listenerVectorOrientation.        All angles are specified in degrees.        Default:            The default orientation is with the listener looking directly along the negative Z axis.            yaw: 0.0            pitch: 0.0            roll: 0.0
--
-- ObjC selector: @- setListenerAngularOrientation:@
setListenerAngularOrientation :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> AVAudio3DAngularOrientation -> IO ()
setListenerAngularOrientation avAudioEnvironmentNode value =
  sendMessage avAudioEnvironmentNode setListenerAngularOrientationSelector value

-- | distanceAttenuationParameters
--
-- The distance attenuation parameters for the environment
--
-- ObjC selector: @- distanceAttenuationParameters@
distanceAttenuationParameters :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> IO (Id AVAudioEnvironmentDistanceAttenuationParameters)
distanceAttenuationParameters avAudioEnvironmentNode =
  sendMessage avAudioEnvironmentNode distanceAttenuationParametersSelector

-- | reverbParameters
--
-- The reverb parameters for the environment
--
-- ObjC selector: @- reverbParameters@
reverbParameters :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> IO (Id AVAudioEnvironmentReverbParameters)
reverbParameters avAudioEnvironmentNode =
  sendMessage avAudioEnvironmentNode reverbParametersSelector

-- | applicableRenderingAlgorithms
--
-- Returns an array of AVAudio3DMixingRenderingAlgorithm values based on the current output format
--
-- AVAudioEnvironmentNode supports several rendering algorithms per input bus which are defined         in <AVFAudio/AVAudioMixing.h>.
--
-- Depending on the current output format of the environment node, this method returns         an immutable array of the applicable rendering algorithms. This is important when the        environment node has been configured to a multichannel output format because only a subset        of the available rendering algorithms are designed to render to all of the channels.
--
-- This information should be retrieved after a successful connection to the destination node         via the engine's connect method.
--
-- ObjC selector: @- applicableRenderingAlgorithms@
applicableRenderingAlgorithms :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> IO (Id NSArray)
applicableRenderingAlgorithms avAudioEnvironmentNode =
  sendMessage avAudioEnvironmentNode applicableRenderingAlgorithmsSelector

-- | listenerHeadTrackingEnabled
--
-- On capable devices, listener orientation will be automatically rotated based on user's head-orientation if enabled.
--
-- ObjC selector: @- listenerHeadTrackingEnabled@
listenerHeadTrackingEnabled :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> IO Bool
listenerHeadTrackingEnabled avAudioEnvironmentNode =
  sendMessage avAudioEnvironmentNode listenerHeadTrackingEnabledSelector

-- | listenerHeadTrackingEnabled
--
-- On capable devices, listener orientation will be automatically rotated based on user's head-orientation if enabled.
--
-- ObjC selector: @- setListenerHeadTrackingEnabled:@
setListenerHeadTrackingEnabled :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> Bool -> IO ()
setListenerHeadTrackingEnabled avAudioEnvironmentNode value =
  sendMessage avAudioEnvironmentNode setListenerHeadTrackingEnabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAudioEnvironmentNode)
initSelector = mkSelector "init"

-- | @Selector@ for @outputType@
outputTypeSelector :: Selector '[] AVAudioEnvironmentOutputType
outputTypeSelector = mkSelector "outputType"

-- | @Selector@ for @setOutputType:@
setOutputTypeSelector :: Selector '[AVAudioEnvironmentOutputType] ()
setOutputTypeSelector = mkSelector "setOutputType:"

-- | @Selector@ for @outputVolume@
outputVolumeSelector :: Selector '[] CFloat
outputVolumeSelector = mkSelector "outputVolume"

-- | @Selector@ for @setOutputVolume:@
setOutputVolumeSelector :: Selector '[CFloat] ()
setOutputVolumeSelector = mkSelector "setOutputVolume:"

-- | @Selector@ for @nextAvailableInputBus@
nextAvailableInputBusSelector :: Selector '[] CULong
nextAvailableInputBusSelector = mkSelector "nextAvailableInputBus"

-- | @Selector@ for @listenerPosition@
listenerPositionSelector :: Selector '[] AVAudio3DPoint
listenerPositionSelector = mkSelector "listenerPosition"

-- | @Selector@ for @setListenerPosition:@
setListenerPositionSelector :: Selector '[AVAudio3DPoint] ()
setListenerPositionSelector = mkSelector "setListenerPosition:"

-- | @Selector@ for @listenerVectorOrientation@
listenerVectorOrientationSelector :: Selector '[] AVAudio3DVectorOrientation
listenerVectorOrientationSelector = mkSelector "listenerVectorOrientation"

-- | @Selector@ for @setListenerVectorOrientation:@
setListenerVectorOrientationSelector :: Selector '[AVAudio3DVectorOrientation] ()
setListenerVectorOrientationSelector = mkSelector "setListenerVectorOrientation:"

-- | @Selector@ for @listenerAngularOrientation@
listenerAngularOrientationSelector :: Selector '[] AVAudio3DAngularOrientation
listenerAngularOrientationSelector = mkSelector "listenerAngularOrientation"

-- | @Selector@ for @setListenerAngularOrientation:@
setListenerAngularOrientationSelector :: Selector '[AVAudio3DAngularOrientation] ()
setListenerAngularOrientationSelector = mkSelector "setListenerAngularOrientation:"

-- | @Selector@ for @distanceAttenuationParameters@
distanceAttenuationParametersSelector :: Selector '[] (Id AVAudioEnvironmentDistanceAttenuationParameters)
distanceAttenuationParametersSelector = mkSelector "distanceAttenuationParameters"

-- | @Selector@ for @reverbParameters@
reverbParametersSelector :: Selector '[] (Id AVAudioEnvironmentReverbParameters)
reverbParametersSelector = mkSelector "reverbParameters"

-- | @Selector@ for @applicableRenderingAlgorithms@
applicableRenderingAlgorithmsSelector :: Selector '[] (Id NSArray)
applicableRenderingAlgorithmsSelector = mkSelector "applicableRenderingAlgorithms"

-- | @Selector@ for @listenerHeadTrackingEnabled@
listenerHeadTrackingEnabledSelector :: Selector '[] Bool
listenerHeadTrackingEnabledSelector = mkSelector "listenerHeadTrackingEnabled"

-- | @Selector@ for @setListenerHeadTrackingEnabled:@
setListenerHeadTrackingEnabledSelector :: Selector '[Bool] ()
setListenerHeadTrackingEnabledSelector = mkSelector "setListenerHeadTrackingEnabled:"

