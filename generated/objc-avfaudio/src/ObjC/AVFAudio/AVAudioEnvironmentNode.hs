{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , outputTypeSelector
  , setOutputTypeSelector
  , outputVolumeSelector
  , setOutputVolumeSelector
  , nextAvailableInputBusSelector
  , listenerPositionSelector
  , setListenerPositionSelector
  , listenerVectorOrientationSelector
  , setListenerVectorOrientationSelector
  , listenerAngularOrientationSelector
  , setListenerAngularOrientationSelector
  , distanceAttenuationParametersSelector
  , reverbParametersSelector
  , applicableRenderingAlgorithmsSelector
  , listenerHeadTrackingEnabledSelector
  , setListenerHeadTrackingEnabledSelector

  -- * Enum types
  , AVAudioEnvironmentOutputType(AVAudioEnvironmentOutputType)
  , pattern AVAudioEnvironmentOutputTypeAuto
  , pattern AVAudioEnvironmentOutputTypeHeadphones
  , pattern AVAudioEnvironmentOutputTypeBuiltInSpeakers
  , pattern AVAudioEnvironmentOutputTypeExternalSpeakers

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.AVFAudio.Internal.Structs
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> IO (Id AVAudioEnvironmentNode)
init_ avAudioEnvironmentNode  =
  sendMsg avAudioEnvironmentNode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
outputType avAudioEnvironmentNode  =
  fmap (coerce :: CLong -> AVAudioEnvironmentOutputType) $ sendMsg avAudioEnvironmentNode (mkSelector "outputType") retCLong []

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
setOutputType avAudioEnvironmentNode  value =
  sendMsg avAudioEnvironmentNode (mkSelector "setOutputType:") retVoid [argCLong (coerce value)]

-- | outputVolume
--
-- The mixer's output volume.
--
-- This accesses the mixer's output volume (0.0-1.0, inclusive).
--
-- ObjC selector: @- outputVolume@
outputVolume :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> IO CFloat
outputVolume avAudioEnvironmentNode  =
  sendMsg avAudioEnvironmentNode (mkSelector "outputVolume") retCFloat []

-- | outputVolume
--
-- The mixer's output volume.
--
-- This accesses the mixer's output volume (0.0-1.0, inclusive).
--
-- ObjC selector: @- setOutputVolume:@
setOutputVolume :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> CFloat -> IO ()
setOutputVolume avAudioEnvironmentNode  value =
  sendMsg avAudioEnvironmentNode (mkSelector "setOutputVolume:") retVoid [argCFloat (fromIntegral value)]

-- | nextAvailableInputBus
--
-- Find an unused input bus
--
-- This will find and return the first input bus to which no other node is connected.
--
-- ObjC selector: @- nextAvailableInputBus@
nextAvailableInputBus :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> IO CULong
nextAvailableInputBus avAudioEnvironmentNode  =
  sendMsg avAudioEnvironmentNode (mkSelector "nextAvailableInputBus") retCULong []

-- | listenerPosition
--
-- Sets the listener's position in the 3D environment
--
-- The coordinates are specified in meters.        Default:            The default position of the listener is at the origin.            x: 0.0            y: 0.0            z: 0.0
--
-- ObjC selector: @- listenerPosition@
listenerPosition :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> IO AVAudio3DPoint
listenerPosition avAudioEnvironmentNode  =
  sendMsgStret avAudioEnvironmentNode (mkSelector "listenerPosition") retAVAudio3DPoint []

-- | listenerPosition
--
-- Sets the listener's position in the 3D environment
--
-- The coordinates are specified in meters.        Default:            The default position of the listener is at the origin.            x: 0.0            y: 0.0            z: 0.0
--
-- ObjC selector: @- setListenerPosition:@
setListenerPosition :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> AVAudio3DPoint -> IO ()
setListenerPosition avAudioEnvironmentNode  value =
  sendMsg avAudioEnvironmentNode (mkSelector "setListenerPosition:") retVoid [argAVAudio3DPoint value]

-- | listenerVectorOrientation
--
-- The listener's orientation in the environment
--
-- Changing listenerVectorOrientation will result in a corresponding change in listenerAngularOrientation.        Default:            The default orientation is with the listener looking directly along the negative Z axis.            forward: (0, 0, -1)            up:      (0, 1, 0)
--
-- ObjC selector: @- listenerVectorOrientation@
listenerVectorOrientation :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> IO AVAudio3DVectorOrientation
listenerVectorOrientation avAudioEnvironmentNode  =
  sendMsgStret avAudioEnvironmentNode (mkSelector "listenerVectorOrientation") retAVAudio3DVectorOrientation []

-- | listenerVectorOrientation
--
-- The listener's orientation in the environment
--
-- Changing listenerVectorOrientation will result in a corresponding change in listenerAngularOrientation.        Default:            The default orientation is with the listener looking directly along the negative Z axis.            forward: (0, 0, -1)            up:      (0, 1, 0)
--
-- ObjC selector: @- setListenerVectorOrientation:@
setListenerVectorOrientation :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> AVAudio3DVectorOrientation -> IO ()
setListenerVectorOrientation avAudioEnvironmentNode  value =
  sendMsg avAudioEnvironmentNode (mkSelector "setListenerVectorOrientation:") retVoid [argAVAudio3DVectorOrientation value]

-- | listenerAngularOrientation
--
-- The listener's orientation in the environment
--
-- Changing listenerAngularOrientation will result in a corresponding change in listenerVectorOrientation.        All angles are specified in degrees.        Default:            The default orientation is with the listener looking directly along the negative Z axis.            yaw: 0.0            pitch: 0.0            roll: 0.0
--
-- ObjC selector: @- listenerAngularOrientation@
listenerAngularOrientation :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> IO AVAudio3DAngularOrientation
listenerAngularOrientation avAudioEnvironmentNode  =
  sendMsgStret avAudioEnvironmentNode (mkSelector "listenerAngularOrientation") retAVAudio3DAngularOrientation []

-- | listenerAngularOrientation
--
-- The listener's orientation in the environment
--
-- Changing listenerAngularOrientation will result in a corresponding change in listenerVectorOrientation.        All angles are specified in degrees.        Default:            The default orientation is with the listener looking directly along the negative Z axis.            yaw: 0.0            pitch: 0.0            roll: 0.0
--
-- ObjC selector: @- setListenerAngularOrientation:@
setListenerAngularOrientation :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> AVAudio3DAngularOrientation -> IO ()
setListenerAngularOrientation avAudioEnvironmentNode  value =
  sendMsg avAudioEnvironmentNode (mkSelector "setListenerAngularOrientation:") retVoid [argAVAudio3DAngularOrientation value]

-- | distanceAttenuationParameters
--
-- The distance attenuation parameters for the environment
--
-- ObjC selector: @- distanceAttenuationParameters@
distanceAttenuationParameters :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> IO (Id AVAudioEnvironmentDistanceAttenuationParameters)
distanceAttenuationParameters avAudioEnvironmentNode  =
  sendMsg avAudioEnvironmentNode (mkSelector "distanceAttenuationParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | reverbParameters
--
-- The reverb parameters for the environment
--
-- ObjC selector: @- reverbParameters@
reverbParameters :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> IO (Id AVAudioEnvironmentReverbParameters)
reverbParameters avAudioEnvironmentNode  =
  sendMsg avAudioEnvironmentNode (mkSelector "reverbParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

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
applicableRenderingAlgorithms avAudioEnvironmentNode  =
  sendMsg avAudioEnvironmentNode (mkSelector "applicableRenderingAlgorithms") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | listenerHeadTrackingEnabled
--
-- On capable devices, listener orientation will be automatically rotated based on user's head-orientation if enabled.
--
-- ObjC selector: @- listenerHeadTrackingEnabled@
listenerHeadTrackingEnabled :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> IO Bool
listenerHeadTrackingEnabled avAudioEnvironmentNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAudioEnvironmentNode (mkSelector "listenerHeadTrackingEnabled") retCULong []

-- | listenerHeadTrackingEnabled
--
-- On capable devices, listener orientation will be automatically rotated based on user's head-orientation if enabled.
--
-- ObjC selector: @- setListenerHeadTrackingEnabled:@
setListenerHeadTrackingEnabled :: IsAVAudioEnvironmentNode avAudioEnvironmentNode => avAudioEnvironmentNode -> Bool -> IO ()
setListenerHeadTrackingEnabled avAudioEnvironmentNode  value =
  sendMsg avAudioEnvironmentNode (mkSelector "setListenerHeadTrackingEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @outputType@
outputTypeSelector :: Selector
outputTypeSelector = mkSelector "outputType"

-- | @Selector@ for @setOutputType:@
setOutputTypeSelector :: Selector
setOutputTypeSelector = mkSelector "setOutputType:"

-- | @Selector@ for @outputVolume@
outputVolumeSelector :: Selector
outputVolumeSelector = mkSelector "outputVolume"

-- | @Selector@ for @setOutputVolume:@
setOutputVolumeSelector :: Selector
setOutputVolumeSelector = mkSelector "setOutputVolume:"

-- | @Selector@ for @nextAvailableInputBus@
nextAvailableInputBusSelector :: Selector
nextAvailableInputBusSelector = mkSelector "nextAvailableInputBus"

-- | @Selector@ for @listenerPosition@
listenerPositionSelector :: Selector
listenerPositionSelector = mkSelector "listenerPosition"

-- | @Selector@ for @setListenerPosition:@
setListenerPositionSelector :: Selector
setListenerPositionSelector = mkSelector "setListenerPosition:"

-- | @Selector@ for @listenerVectorOrientation@
listenerVectorOrientationSelector :: Selector
listenerVectorOrientationSelector = mkSelector "listenerVectorOrientation"

-- | @Selector@ for @setListenerVectorOrientation:@
setListenerVectorOrientationSelector :: Selector
setListenerVectorOrientationSelector = mkSelector "setListenerVectorOrientation:"

-- | @Selector@ for @listenerAngularOrientation@
listenerAngularOrientationSelector :: Selector
listenerAngularOrientationSelector = mkSelector "listenerAngularOrientation"

-- | @Selector@ for @setListenerAngularOrientation:@
setListenerAngularOrientationSelector :: Selector
setListenerAngularOrientationSelector = mkSelector "setListenerAngularOrientation:"

-- | @Selector@ for @distanceAttenuationParameters@
distanceAttenuationParametersSelector :: Selector
distanceAttenuationParametersSelector = mkSelector "distanceAttenuationParameters"

-- | @Selector@ for @reverbParameters@
reverbParametersSelector :: Selector
reverbParametersSelector = mkSelector "reverbParameters"

-- | @Selector@ for @applicableRenderingAlgorithms@
applicableRenderingAlgorithmsSelector :: Selector
applicableRenderingAlgorithmsSelector = mkSelector "applicableRenderingAlgorithms"

-- | @Selector@ for @listenerHeadTrackingEnabled@
listenerHeadTrackingEnabledSelector :: Selector
listenerHeadTrackingEnabledSelector = mkSelector "listenerHeadTrackingEnabled"

-- | @Selector@ for @setListenerHeadTrackingEnabled:@
setListenerHeadTrackingEnabledSelector :: Selector
setListenerHeadTrackingEnabledSelector = mkSelector "setListenerHeadTrackingEnabled:"

