{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , source
  , streamingClients
  , clockTypeSelector
  , consumeSampleBufferFromClient_completionHandlerSelector
  , customClockConfigurationSelector
  , directionSelector
  , initSelector
  , initWithLocalizedName_streamID_direction_clockType_sourceSelector
  , initWithLocalizedName_streamID_direction_customClockConfiguration_sourceSelector
  , localizedNameSelector
  , newSelector
  , notifyPropertiesChangedSelector
  , notifyScheduledOutputChangedSelector
  , sendSampleBuffer_discontinuity_hostTimeInNanosecondsSelector
  , sourceSelector
  , streamIDSelector
  , streamWithLocalizedName_streamID_direction_clockType_sourceSelector
  , streamWithLocalizedName_streamID_direction_customClockConfiguration_sourceSelector
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMediaIO.Internal.Classes
import ObjC.CoreMediaIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCMIOExtensionStream cmioExtensionStream => cmioExtensionStream -> IO (Id CMIOExtensionStream)
init_ cmioExtensionStream =
  sendOwnedMessage cmioExtensionStream initSelector

-- | @+ new@
new :: IO (Id CMIOExtensionStream)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionStream"
    sendOwnedClassMessage cls' newSelector

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
    sendClassMessage cls' streamWithLocalizedName_streamID_direction_clockType_sourceSelector (toNSString localizedName) (toNSUUID streamID) direction clockType source

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
    sendClassMessage cls' streamWithLocalizedName_streamID_direction_customClockConfiguration_sourceSelector (toNSString localizedName) (toNSUUID streamID) direction (toCMIOExtensionStreamCustomClockConfiguration customClockConfiguration) source

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
initWithLocalizedName_streamID_direction_clockType_source cmioExtensionStream localizedName streamID direction clockType source =
  sendOwnedMessage cmioExtensionStream initWithLocalizedName_streamID_direction_clockType_sourceSelector (toNSString localizedName) (toNSUUID streamID) direction clockType source

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
initWithLocalizedName_streamID_direction_customClockConfiguration_source cmioExtensionStream localizedName streamID direction customClockConfiguration source =
  sendOwnedMessage cmioExtensionStream initWithLocalizedName_streamID_direction_customClockConfiguration_sourceSelector (toNSString localizedName) (toNSUUID streamID) direction (toCMIOExtensionStreamCustomClockConfiguration customClockConfiguration) source

-- | notifyPropertiesChanged:
--
-- Notify client(s) of stream properties changes.
--
-- @propertyStates@ — The dictionary of properties having changed.
--
-- ObjC selector: @- notifyPropertiesChanged:@
notifyPropertiesChanged :: (IsCMIOExtensionStream cmioExtensionStream, IsNSDictionary propertyStates) => cmioExtensionStream -> propertyStates -> IO ()
notifyPropertiesChanged cmioExtensionStream propertyStates =
  sendMessage cmioExtensionStream notifyPropertiesChangedSelector (toNSDictionary propertyStates)

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
sendSampleBuffer_discontinuity_hostTimeInNanoseconds cmioExtensionStream sampleBuffer discontinuity hostTimeInNanoseconds =
  sendMessage cmioExtensionStream sendSampleBuffer_discontinuity_hostTimeInNanosecondsSelector sampleBuffer discontinuity hostTimeInNanoseconds

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
consumeSampleBufferFromClient_completionHandler cmioExtensionStream client completionHandler =
  sendMessage cmioExtensionStream consumeSampleBufferFromClient_completionHandlerSelector (toCMIOExtensionClient client) completionHandler

-- | notifyScheduledOutputChanged:
--
-- Notify client(s) when a particular buffer was output.
--
-- @scheduledOutput@ — The stream scheduled output.
--
-- ObjC selector: @- notifyScheduledOutputChanged:@
notifyScheduledOutputChanged :: (IsCMIOExtensionStream cmioExtensionStream, IsCMIOExtensionScheduledOutput scheduledOutput) => cmioExtensionStream -> scheduledOutput -> IO ()
notifyScheduledOutputChanged cmioExtensionStream scheduledOutput =
  sendMessage cmioExtensionStream notifyScheduledOutputChangedSelector (toCMIOExtensionScheduledOutput scheduledOutput)

-- | localizedName
--
-- The localized name of the stream.
--
-- ObjC selector: @- localizedName@
localizedName :: IsCMIOExtensionStream cmioExtensionStream => cmioExtensionStream -> IO (Id NSString)
localizedName cmioExtensionStream =
  sendMessage cmioExtensionStream localizedNameSelector

-- | streamID
--
-- The stream identifier.
--
-- ObjC selector: @- streamID@
streamID :: IsCMIOExtensionStream cmioExtensionStream => cmioExtensionStream -> IO (Id NSUUID)
streamID cmioExtensionStream =
  sendMessage cmioExtensionStream streamIDSelector

-- | direction
--
-- The stream direction.
--
-- ObjC selector: @- direction@
direction :: IsCMIOExtensionStream cmioExtensionStream => cmioExtensionStream -> IO CMIOExtensionStreamDirection
direction cmioExtensionStream =
  sendMessage cmioExtensionStream directionSelector

-- | clockType
--
-- The stream clock type.
--
-- If the stream was specified with a custom clock configuration, the returned value will be CMIOExtensionStreamClockTypeCustom.
--
-- ObjC selector: @- clockType@
clockType :: IsCMIOExtensionStream cmioExtensionStream => cmioExtensionStream -> IO CMIOExtensionStreamClockType
clockType cmioExtensionStream =
  sendMessage cmioExtensionStream clockTypeSelector

-- | customClockConfiguration
--
-- Custom clock configuration.
--
-- If the stream was specified using a clockType, the returned value will be nil.
--
-- ObjC selector: @- customClockConfiguration@
customClockConfiguration :: IsCMIOExtensionStream cmioExtensionStream => cmioExtensionStream -> IO (Id CMIOExtensionStreamCustomClockConfiguration)
customClockConfiguration cmioExtensionStream =
  sendMessage cmioExtensionStream customClockConfigurationSelector

-- | source
--
-- The stream source.
--
-- ObjC selector: @- source@
source :: IsCMIOExtensionStream cmioExtensionStream => cmioExtensionStream -> IO RawId
source cmioExtensionStream =
  sendMessage cmioExtensionStream sourceSelector

-- | streamingClients
--
-- The array of streaming clients.
--
-- This property is key-value observable.
--
-- ObjC selector: @- streamingClients@
streamingClients :: IsCMIOExtensionStream cmioExtensionStream => cmioExtensionStream -> IO (Id NSArray)
streamingClients cmioExtensionStream =
  sendMessage cmioExtensionStream streamingClientsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CMIOExtensionStream)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CMIOExtensionStream)
newSelector = mkSelector "new"

-- | @Selector@ for @streamWithLocalizedName:streamID:direction:clockType:source:@
streamWithLocalizedName_streamID_direction_clockType_sourceSelector :: Selector '[Id NSString, Id NSUUID, CMIOExtensionStreamDirection, CMIOExtensionStreamClockType, RawId] (Id CMIOExtensionStream)
streamWithLocalizedName_streamID_direction_clockType_sourceSelector = mkSelector "streamWithLocalizedName:streamID:direction:clockType:source:"

-- | @Selector@ for @streamWithLocalizedName:streamID:direction:customClockConfiguration:source:@
streamWithLocalizedName_streamID_direction_customClockConfiguration_sourceSelector :: Selector '[Id NSString, Id NSUUID, CMIOExtensionStreamDirection, Id CMIOExtensionStreamCustomClockConfiguration, RawId] (Id CMIOExtensionStream)
streamWithLocalizedName_streamID_direction_customClockConfiguration_sourceSelector = mkSelector "streamWithLocalizedName:streamID:direction:customClockConfiguration:source:"

-- | @Selector@ for @initWithLocalizedName:streamID:direction:clockType:source:@
initWithLocalizedName_streamID_direction_clockType_sourceSelector :: Selector '[Id NSString, Id NSUUID, CMIOExtensionStreamDirection, CMIOExtensionStreamClockType, RawId] (Id CMIOExtensionStream)
initWithLocalizedName_streamID_direction_clockType_sourceSelector = mkSelector "initWithLocalizedName:streamID:direction:clockType:source:"

-- | @Selector@ for @initWithLocalizedName:streamID:direction:customClockConfiguration:source:@
initWithLocalizedName_streamID_direction_customClockConfiguration_sourceSelector :: Selector '[Id NSString, Id NSUUID, CMIOExtensionStreamDirection, Id CMIOExtensionStreamCustomClockConfiguration, RawId] (Id CMIOExtensionStream)
initWithLocalizedName_streamID_direction_customClockConfiguration_sourceSelector = mkSelector "initWithLocalizedName:streamID:direction:customClockConfiguration:source:"

-- | @Selector@ for @notifyPropertiesChanged:@
notifyPropertiesChangedSelector :: Selector '[Id NSDictionary] ()
notifyPropertiesChangedSelector = mkSelector "notifyPropertiesChanged:"

-- | @Selector@ for @sendSampleBuffer:discontinuity:hostTimeInNanoseconds:@
sendSampleBuffer_discontinuity_hostTimeInNanosecondsSelector :: Selector '[Ptr (), CMIOExtensionStreamDiscontinuityFlags, CULong] ()
sendSampleBuffer_discontinuity_hostTimeInNanosecondsSelector = mkSelector "sendSampleBuffer:discontinuity:hostTimeInNanoseconds:"

-- | @Selector@ for @consumeSampleBufferFromClient:completionHandler:@
consumeSampleBufferFromClient_completionHandlerSelector :: Selector '[Id CMIOExtensionClient, Ptr ()] ()
consumeSampleBufferFromClient_completionHandlerSelector = mkSelector "consumeSampleBufferFromClient:completionHandler:"

-- | @Selector@ for @notifyScheduledOutputChanged:@
notifyScheduledOutputChangedSelector :: Selector '[Id CMIOExtensionScheduledOutput] ()
notifyScheduledOutputChangedSelector = mkSelector "notifyScheduledOutputChanged:"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector '[] (Id NSString)
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @streamID@
streamIDSelector :: Selector '[] (Id NSUUID)
streamIDSelector = mkSelector "streamID"

-- | @Selector@ for @direction@
directionSelector :: Selector '[] CMIOExtensionStreamDirection
directionSelector = mkSelector "direction"

-- | @Selector@ for @clockType@
clockTypeSelector :: Selector '[] CMIOExtensionStreamClockType
clockTypeSelector = mkSelector "clockType"

-- | @Selector@ for @customClockConfiguration@
customClockConfigurationSelector :: Selector '[] (Id CMIOExtensionStreamCustomClockConfiguration)
customClockConfigurationSelector = mkSelector "customClockConfiguration"

-- | @Selector@ for @source@
sourceSelector :: Selector '[] RawId
sourceSelector = mkSelector "source"

-- | @Selector@ for @streamingClients@
streamingClientsSelector :: Selector '[] (Id NSArray)
streamingClientsSelector = mkSelector "streamingClients"

