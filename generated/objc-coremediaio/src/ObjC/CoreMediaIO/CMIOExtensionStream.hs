{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CMIOExtensionStream
--
-- A CMIOExtensionStream describes a stream of media data.
--
-- Generated bindings for @CMIOExtensionStream@.
module ObjC.CoreMediaIO.CMIOExtensionStream
  ( CMIOExtensionStream
  , IsCMIOExtensionStream(..)
  , init_
  , new
  , streamWithLocalizedName_streamID_direction_clockType_source
  , streamWithLocalizedName_streamID_direction_customClockConfiguration_source
  , initWithLocalizedName_streamID_direction_clockType_source
  , initWithLocalizedName_streamID_direction_customClockConfiguration_source
  , notifyPropertiesChanged
  , sendSampleBuffer_discontinuity_hostTimeInNanoseconds
  , consumeSampleBufferFromClient_completionHandler
  , notifyScheduledOutputChanged
  , localizedName
  , streamID
  , direction
  , clockType
  , customClockConfiguration
  , streamingClients
  , initSelector
  , newSelector
  , streamWithLocalizedName_streamID_direction_clockType_sourceSelector
  , streamWithLocalizedName_streamID_direction_customClockConfiguration_sourceSelector
  , initWithLocalizedName_streamID_direction_clockType_sourceSelector
  , initWithLocalizedName_streamID_direction_customClockConfiguration_sourceSelector
  , notifyPropertiesChangedSelector
  , sendSampleBuffer_discontinuity_hostTimeInNanosecondsSelector
  , consumeSampleBufferFromClient_completionHandlerSelector
  , notifyScheduledOutputChangedSelector
  , localizedNameSelector
  , streamIDSelector
  , directionSelector
  , clockTypeSelector
  , customClockConfigurationSelector
  , streamingClientsSelector

  -- * Enum types
  , CMIOExtensionStreamClockType(CMIOExtensionStreamClockType)
  , pattern CMIOExtensionStreamClockTypeHostTime
  , pattern CMIOExtensionStreamClockTypeLinkedCoreAudioDeviceUID
  , pattern CMIOExtensionStreamClockTypeCustom
  , CMIOExtensionStreamDirection(CMIOExtensionStreamDirection)
  , pattern CMIOExtensionStreamDirectionSource
  , pattern CMIOExtensionStreamDirectionSink
  , CMIOExtensionStreamDiscontinuityFlags(CMIOExtensionStreamDiscontinuityFlags)
  , pattern CMIOExtensionStreamDiscontinuityFlagNone
  , pattern CMIOExtensionStreamDiscontinuityFlagUnknown
  , pattern CMIOExtensionStreamDiscontinuityFlagTime
  , pattern CMIOExtensionStreamDiscontinuityFlagSampleDropped

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

import ObjC.CoreMediaIO.Internal.Classes
import ObjC.CoreMediaIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCMIOExtensionStream cmioExtensionStream => cmioExtensionStream -> IO (Id CMIOExtensionStream)
init_ cmioExtensionStream  =
  sendMsg cmioExtensionStream (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CMIOExtensionStream)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionStream"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | streamWithLocalizedName:streamID:direction:clockType:source:
--
-- Returns a stream instance.
--
-- @localizedName@ — The localized name of the stream.
--
-- @streamID@ — The stream identifier.
--
-- @direction@ — The stream direction.
--
-- @clockType@ — The stream clock type.
--
-- @source@ — The stream source.
--
-- Returns: A CMIOExtensionStream instance that provides data.
--
-- Note that the clockType parameter may not be CMIOExtensionStreamClockTypeCustom; that value is reserved for streams created with a custom clock configuration. For streams that have a custom clock, use streamWithLocalizedName:streamID:direction:customClockConfiguration:source:.
--
-- ObjC selector: @+ streamWithLocalizedName:streamID:direction:clockType:source:@
streamWithLocalizedName_streamID_direction_clockType_source :: (IsNSString localizedName, IsNSUUID streamID) => localizedName -> streamID -> CMIOExtensionStreamDirection -> CMIOExtensionStreamClockType -> RawId -> IO (Id CMIOExtensionStream)
streamWithLocalizedName_streamID_direction_clockType_source localizedName streamID direction clockType source =
  do
    cls' <- getRequiredClass "CMIOExtensionStream"
    withObjCPtr localizedName $ \raw_localizedName ->
      withObjCPtr streamID $ \raw_streamID ->
        sendClassMsg cls' (mkSelector "streamWithLocalizedName:streamID:direction:clockType:source:") (retPtr retVoid) [argPtr (castPtr raw_localizedName :: Ptr ()), argPtr (castPtr raw_streamID :: Ptr ()), argCLong (coerce direction), argCLong (coerce clockType), argPtr (castPtr (unRawId source) :: Ptr ())] >>= retainedObject . castPtr

-- | streamWithLocalizedName:streamID:direction:customClockConfiguration:source:
--
-- Returns a stream instance.
--
-- @localizedName@ — The localized name of the stream.
--
-- @streamID@ — The stream identifier.
--
-- @direction@ — The stream direction.
--
-- @customClockConfiguration@ — A CMIOExtensionStreamCustomClockConfiguration object that defines the custom clock configuration.
--
-- @source@ — The stream source.
--
-- Returns: A CMIOExtensionStream instance that provides data.
--
-- ObjC selector: @+ streamWithLocalizedName:streamID:direction:customClockConfiguration:source:@
streamWithLocalizedName_streamID_direction_customClockConfiguration_source :: (IsNSString localizedName, IsNSUUID streamID, IsCMIOExtensionStreamCustomClockConfiguration customClockConfiguration) => localizedName -> streamID -> CMIOExtensionStreamDirection -> customClockConfiguration -> RawId -> IO (Id CMIOExtensionStream)
streamWithLocalizedName_streamID_direction_customClockConfiguration_source localizedName streamID direction customClockConfiguration source =
  do
    cls' <- getRequiredClass "CMIOExtensionStream"
    withObjCPtr localizedName $ \raw_localizedName ->
      withObjCPtr streamID $ \raw_streamID ->
        withObjCPtr customClockConfiguration $ \raw_customClockConfiguration ->
          sendClassMsg cls' (mkSelector "streamWithLocalizedName:streamID:direction:customClockConfiguration:source:") (retPtr retVoid) [argPtr (castPtr raw_localizedName :: Ptr ()), argPtr (castPtr raw_streamID :: Ptr ()), argCLong (coerce direction), argPtr (castPtr raw_customClockConfiguration :: Ptr ()), argPtr (castPtr (unRawId source) :: Ptr ())] >>= retainedObject . castPtr

-- | initWithLocalizedName:streamID:direction:clockType:source:
--
-- Initialize a stream instance.
--
-- @localizedName@ — The localized name of the stream.
--
-- @streamID@ — The stream identifier.
--
-- @direction@ — The stream direction.
--
-- @clockType@ — The stream clock type.
--
-- @source@ — The stream source.
--
-- Returns: A CMIOExtensionStream instance that provides data.
--
-- Note that the clockType parameter may not be CMIOExtensionStreamClockTypeCustom; that value is reserved for streams created with a custom clock configuration. For streams that have a custom clock, use streamWithLocalizedName:streamID:direction:customClockConfiguration:source:.
--
-- ObjC selector: @- initWithLocalizedName:streamID:direction:clockType:source:@
initWithLocalizedName_streamID_direction_clockType_source :: (IsCMIOExtensionStream cmioExtensionStream, IsNSString localizedName, IsNSUUID streamID) => cmioExtensionStream -> localizedName -> streamID -> CMIOExtensionStreamDirection -> CMIOExtensionStreamClockType -> RawId -> IO (Id CMIOExtensionStream)
initWithLocalizedName_streamID_direction_clockType_source cmioExtensionStream  localizedName streamID direction clockType source =
withObjCPtr localizedName $ \raw_localizedName ->
  withObjCPtr streamID $ \raw_streamID ->
      sendMsg cmioExtensionStream (mkSelector "initWithLocalizedName:streamID:direction:clockType:source:") (retPtr retVoid) [argPtr (castPtr raw_localizedName :: Ptr ()), argPtr (castPtr raw_streamID :: Ptr ()), argCLong (coerce direction), argCLong (coerce clockType), argPtr (castPtr (unRawId source) :: Ptr ())] >>= ownedObject . castPtr

-- | initWithLocalizedName:streamID:direction:clockType:source:
--
-- Initialize a stream instance.
--
-- @localizedName@ — The localized name of the stream.
--
-- @streamID@ — The stream identifier.
--
-- @direction@ — The stream direction.
--
-- @customClockConfiguration@ — A CMIOExtensionStreamCustomClockConfiguration object that defines the custom clock configuration.
--
-- @source@ — The stream source.
--
-- Returns: A CMIOExtensionStream instance that provides data.
--
-- ObjC selector: @- initWithLocalizedName:streamID:direction:customClockConfiguration:source:@
initWithLocalizedName_streamID_direction_customClockConfiguration_source :: (IsCMIOExtensionStream cmioExtensionStream, IsNSString localizedName, IsNSUUID streamID, IsCMIOExtensionStreamCustomClockConfiguration customClockConfiguration) => cmioExtensionStream -> localizedName -> streamID -> CMIOExtensionStreamDirection -> customClockConfiguration -> RawId -> IO (Id CMIOExtensionStream)
initWithLocalizedName_streamID_direction_customClockConfiguration_source cmioExtensionStream  localizedName streamID direction customClockConfiguration source =
withObjCPtr localizedName $ \raw_localizedName ->
  withObjCPtr streamID $ \raw_streamID ->
    withObjCPtr customClockConfiguration $ \raw_customClockConfiguration ->
        sendMsg cmioExtensionStream (mkSelector "initWithLocalizedName:streamID:direction:customClockConfiguration:source:") (retPtr retVoid) [argPtr (castPtr raw_localizedName :: Ptr ()), argPtr (castPtr raw_streamID :: Ptr ()), argCLong (coerce direction), argPtr (castPtr raw_customClockConfiguration :: Ptr ()), argPtr (castPtr (unRawId source) :: Ptr ())] >>= ownedObject . castPtr

-- | notifyPropertiesChanged:
--
-- Notify client(s) of stream properties changes.
--
-- @propertyStates@ — The dictionary of properties having changed.
--
-- ObjC selector: @- notifyPropertiesChanged:@
notifyPropertiesChanged :: (IsCMIOExtensionStream cmioExtensionStream, IsNSDictionary propertyStates) => cmioExtensionStream -> propertyStates -> IO ()
notifyPropertiesChanged cmioExtensionStream  propertyStates =
withObjCPtr propertyStates $ \raw_propertyStates ->
    sendMsg cmioExtensionStream (mkSelector "notifyPropertiesChanged:") retVoid [argPtr (castPtr raw_propertyStates :: Ptr ())]

-- | sendSampleBuffer:discontinuity:hostTimeInNanoseconds:
--
-- Send media sample to client(s).
--
-- @sampleBuffer@ — The sample buffer containing media data.
--
-- @discontinuity@ — The discontinuity flag indicating if the sample buffer represents a discontinuity boundary.
--
-- @hostTimeInNanoseconds@ — The host time in nanoseconds when the buffer was captured.
--
-- The sample will be deliver to clients whose media type authorization status is authorized. The sample buffer timestamps should be relative to the clock timebase specified with clockType. Attempting to send a sample buffer on a sink stream will throw an exception.
--
-- ObjC selector: @- sendSampleBuffer:discontinuity:hostTimeInNanoseconds:@
sendSampleBuffer_discontinuity_hostTimeInNanoseconds :: IsCMIOExtensionStream cmioExtensionStream => cmioExtensionStream -> Ptr () -> CMIOExtensionStreamDiscontinuityFlags -> CULong -> IO ()
sendSampleBuffer_discontinuity_hostTimeInNanoseconds cmioExtensionStream  sampleBuffer discontinuity hostTimeInNanoseconds =
  sendMsg cmioExtensionStream (mkSelector "sendSampleBuffer:discontinuity:hostTimeInNanoseconds:") retVoid [argPtr sampleBuffer, argCUInt (coerce discontinuity), argCULong (fromIntegral hostTimeInNanoseconds)]

-- | consumeSampleBufferFromClient:completionHandler:
--
-- Consume a sample buffer from a client.
--
-- @client@ — The client.
--
-- @completionHandler@ — A block that will be called when the operation is completed. If the capture request is successful, the "sampleBuffer" parameter contains a valid CMSampleBuffer, the "sampleBufferSequenceNumber" parameter is the sample buffer sequence number, the "discontinuity" parameter is the discontinuity flag, the "hasMoreSampleBuffers" parameter indicates whether or not more sample buffers are available, the "error" parameter is nil.
--
-- ObjC selector: @- consumeSampleBufferFromClient:completionHandler:@
consumeSampleBufferFromClient_completionHandler :: (IsCMIOExtensionStream cmioExtensionStream, IsCMIOExtensionClient client) => cmioExtensionStream -> client -> Ptr () -> IO ()
consumeSampleBufferFromClient_completionHandler cmioExtensionStream  client completionHandler =
withObjCPtr client $ \raw_client ->
    sendMsg cmioExtensionStream (mkSelector "consumeSampleBufferFromClient:completionHandler:") retVoid [argPtr (castPtr raw_client :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | notifyScheduledOutputChanged:
--
-- Notify client(s) when a particular buffer was output.
--
-- @scheduledOutput@ — The stream scheduled output.
--
-- ObjC selector: @- notifyScheduledOutputChanged:@
notifyScheduledOutputChanged :: (IsCMIOExtensionStream cmioExtensionStream, IsCMIOExtensionScheduledOutput scheduledOutput) => cmioExtensionStream -> scheduledOutput -> IO ()
notifyScheduledOutputChanged cmioExtensionStream  scheduledOutput =
withObjCPtr scheduledOutput $ \raw_scheduledOutput ->
    sendMsg cmioExtensionStream (mkSelector "notifyScheduledOutputChanged:") retVoid [argPtr (castPtr raw_scheduledOutput :: Ptr ())]

-- | localizedName
--
-- The localized name of the stream.
--
-- ObjC selector: @- localizedName@
localizedName :: IsCMIOExtensionStream cmioExtensionStream => cmioExtensionStream -> IO (Id NSString)
localizedName cmioExtensionStream  =
  sendMsg cmioExtensionStream (mkSelector "localizedName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | streamID
--
-- The stream identifier.
--
-- ObjC selector: @- streamID@
streamID :: IsCMIOExtensionStream cmioExtensionStream => cmioExtensionStream -> IO (Id NSUUID)
streamID cmioExtensionStream  =
  sendMsg cmioExtensionStream (mkSelector "streamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | direction
--
-- The stream direction.
--
-- ObjC selector: @- direction@
direction :: IsCMIOExtensionStream cmioExtensionStream => cmioExtensionStream -> IO CMIOExtensionStreamDirection
direction cmioExtensionStream  =
  fmap (coerce :: CLong -> CMIOExtensionStreamDirection) $ sendMsg cmioExtensionStream (mkSelector "direction") retCLong []

-- | clockType
--
-- The stream clock type.
--
-- If the stream was specified with a custom clock configuration, the returned value will be CMIOExtensionStreamClockTypeCustom.
--
-- ObjC selector: @- clockType@
clockType :: IsCMIOExtensionStream cmioExtensionStream => cmioExtensionStream -> IO CMIOExtensionStreamClockType
clockType cmioExtensionStream  =
  fmap (coerce :: CLong -> CMIOExtensionStreamClockType) $ sendMsg cmioExtensionStream (mkSelector "clockType") retCLong []

-- | customClockConfiguration
--
-- Custom clock configuration.
--
-- If the stream was specified using a clockType, the returned value will be nil.
--
-- ObjC selector: @- customClockConfiguration@
customClockConfiguration :: IsCMIOExtensionStream cmioExtensionStream => cmioExtensionStream -> IO (Id CMIOExtensionStreamCustomClockConfiguration)
customClockConfiguration cmioExtensionStream  =
  sendMsg cmioExtensionStream (mkSelector "customClockConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | streamingClients
--
-- The array of streaming clients.
--
-- This property is key-value observable.
--
-- ObjC selector: @- streamingClients@
streamingClients :: IsCMIOExtensionStream cmioExtensionStream => cmioExtensionStream -> IO (Id NSArray)
streamingClients cmioExtensionStream  =
  sendMsg cmioExtensionStream (mkSelector "streamingClients") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @streamWithLocalizedName:streamID:direction:clockType:source:@
streamWithLocalizedName_streamID_direction_clockType_sourceSelector :: Selector
streamWithLocalizedName_streamID_direction_clockType_sourceSelector = mkSelector "streamWithLocalizedName:streamID:direction:clockType:source:"

-- | @Selector@ for @streamWithLocalizedName:streamID:direction:customClockConfiguration:source:@
streamWithLocalizedName_streamID_direction_customClockConfiguration_sourceSelector :: Selector
streamWithLocalizedName_streamID_direction_customClockConfiguration_sourceSelector = mkSelector "streamWithLocalizedName:streamID:direction:customClockConfiguration:source:"

-- | @Selector@ for @initWithLocalizedName:streamID:direction:clockType:source:@
initWithLocalizedName_streamID_direction_clockType_sourceSelector :: Selector
initWithLocalizedName_streamID_direction_clockType_sourceSelector = mkSelector "initWithLocalizedName:streamID:direction:clockType:source:"

-- | @Selector@ for @initWithLocalizedName:streamID:direction:customClockConfiguration:source:@
initWithLocalizedName_streamID_direction_customClockConfiguration_sourceSelector :: Selector
initWithLocalizedName_streamID_direction_customClockConfiguration_sourceSelector = mkSelector "initWithLocalizedName:streamID:direction:customClockConfiguration:source:"

-- | @Selector@ for @notifyPropertiesChanged:@
notifyPropertiesChangedSelector :: Selector
notifyPropertiesChangedSelector = mkSelector "notifyPropertiesChanged:"

-- | @Selector@ for @sendSampleBuffer:discontinuity:hostTimeInNanoseconds:@
sendSampleBuffer_discontinuity_hostTimeInNanosecondsSelector :: Selector
sendSampleBuffer_discontinuity_hostTimeInNanosecondsSelector = mkSelector "sendSampleBuffer:discontinuity:hostTimeInNanoseconds:"

-- | @Selector@ for @consumeSampleBufferFromClient:completionHandler:@
consumeSampleBufferFromClient_completionHandlerSelector :: Selector
consumeSampleBufferFromClient_completionHandlerSelector = mkSelector "consumeSampleBufferFromClient:completionHandler:"

-- | @Selector@ for @notifyScheduledOutputChanged:@
notifyScheduledOutputChangedSelector :: Selector
notifyScheduledOutputChangedSelector = mkSelector "notifyScheduledOutputChanged:"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @streamID@
streamIDSelector :: Selector
streamIDSelector = mkSelector "streamID"

-- | @Selector@ for @direction@
directionSelector :: Selector
directionSelector = mkSelector "direction"

-- | @Selector@ for @clockType@
clockTypeSelector :: Selector
clockTypeSelector = mkSelector "clockType"

-- | @Selector@ for @customClockConfiguration@
customClockConfigurationSelector :: Selector
customClockConfigurationSelector = mkSelector "customClockConfiguration"

-- | @Selector@ for @streamingClients@
streamingClientsSelector :: Selector
streamingClientsSelector = mkSelector "streamingClients"

