{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CMIOExtensionStreamProperties
--
-- A CMIOExtensionStreamProperties describes a CoreMediaIO extension stream properties.
--
-- Generated bindings for @CMIOExtensionStreamProperties@.
module ObjC.CoreMediaIO.CMIOExtensionStreamProperties
  ( CMIOExtensionStreamProperties
  , IsCMIOExtensionStreamProperties(..)
  , init_
  , new
  , streamPropertiesWithDictionary
  , initWithDictionary
  , setPropertyState_forProperty
  , activeFormatIndex
  , setActiveFormatIndex
  , frameDuration
  , setFrameDuration
  , maxFrameDuration
  , setMaxFrameDuration
  , sinkBufferQueueSize
  , setSinkBufferQueueSize
  , sinkBuffersRequiredForStartup
  , setSinkBuffersRequiredForStartup
  , sinkBufferUnderrunCount
  , setSinkBufferUnderrunCount
  , sinkEndOfData
  , setSinkEndOfData
  , propertiesDictionary
  , setPropertiesDictionary
  , activeFormatIndexSelector
  , frameDurationSelector
  , initSelector
  , initWithDictionarySelector
  , maxFrameDurationSelector
  , newSelector
  , propertiesDictionarySelector
  , setActiveFormatIndexSelector
  , setFrameDurationSelector
  , setMaxFrameDurationSelector
  , setPropertiesDictionarySelector
  , setPropertyState_forPropertySelector
  , setSinkBufferQueueSizeSelector
  , setSinkBufferUnderrunCountSelector
  , setSinkBuffersRequiredForStartupSelector
  , setSinkEndOfDataSelector
  , sinkBufferQueueSizeSelector
  , sinkBufferUnderrunCountSelector
  , sinkBuffersRequiredForStartupSelector
  , sinkEndOfDataSelector
  , streamPropertiesWithDictionarySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMediaIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCMIOExtensionStreamProperties cmioExtensionStreamProperties => cmioExtensionStreamProperties -> IO (Id CMIOExtensionStreamProperties)
init_ cmioExtensionStreamProperties =
  sendOwnedMessage cmioExtensionStreamProperties initSelector

-- | @+ new@
new :: IO (Id CMIOExtensionStreamProperties)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionStreamProperties"
    sendOwnedClassMessage cls' newSelector

-- | streamPropertiesWithDictionary:
--
-- Return a stream properties instance.
--
-- @propertiesDictionary@ — The dictionary of properties.
--
-- Returns: A CMIOExtensionStreamProperties instance.
--
-- ObjC selector: @+ streamPropertiesWithDictionary:@
streamPropertiesWithDictionary :: IsNSDictionary propertiesDictionary => propertiesDictionary -> IO (Id CMIOExtensionStreamProperties)
streamPropertiesWithDictionary propertiesDictionary =
  do
    cls' <- getRequiredClass "CMIOExtensionStreamProperties"
    sendClassMessage cls' streamPropertiesWithDictionarySelector (toNSDictionary propertiesDictionary)

-- | initWithDictionary:
--
-- Initialize a stream properties instance.
--
-- @propertiesDictionary@ — The dictionary of properties.
--
-- Returns: A CMIOExtensionStreamProperties instance.
--
-- ObjC selector: @- initWithDictionary:@
initWithDictionary :: (IsCMIOExtensionStreamProperties cmioExtensionStreamProperties, IsNSDictionary propertiesDictionary) => cmioExtensionStreamProperties -> propertiesDictionary -> IO (Id CMIOExtensionStreamProperties)
initWithDictionary cmioExtensionStreamProperties propertiesDictionary =
  sendOwnedMessage cmioExtensionStreamProperties initWithDictionarySelector (toNSDictionary propertiesDictionary)

-- | setPropertyState:forProperty:
--
-- Set the property value.
--
-- @propertyState@ — The property state.
--
-- @property@ — The property key.
--
-- Setting nil to propertyState does remove the property.
--
-- ObjC selector: @- setPropertyState:forProperty:@
setPropertyState_forProperty :: (IsCMIOExtensionStreamProperties cmioExtensionStreamProperties, IsCMIOExtensionPropertyState propertyState, IsNSString property) => cmioExtensionStreamProperties -> propertyState -> property -> IO ()
setPropertyState_forProperty cmioExtensionStreamProperties propertyState property =
  sendMessage cmioExtensionStreamProperties setPropertyState_forPropertySelector (toCMIOExtensionPropertyState propertyState) (toNSString property)

-- | activeFormatIndex
--
-- The active format index.
--
-- The property key is CMIOExtensionPropertyStreamActiveFormatIndex.
--
-- ObjC selector: @- activeFormatIndex@
activeFormatIndex :: IsCMIOExtensionStreamProperties cmioExtensionStreamProperties => cmioExtensionStreamProperties -> IO (Id NSNumber)
activeFormatIndex cmioExtensionStreamProperties =
  sendMessage cmioExtensionStreamProperties activeFormatIndexSelector

-- | activeFormatIndex
--
-- The active format index.
--
-- The property key is CMIOExtensionPropertyStreamActiveFormatIndex.
--
-- ObjC selector: @- setActiveFormatIndex:@
setActiveFormatIndex :: (IsCMIOExtensionStreamProperties cmioExtensionStreamProperties, IsNSNumber value) => cmioExtensionStreamProperties -> value -> IO ()
setActiveFormatIndex cmioExtensionStreamProperties value =
  sendMessage cmioExtensionStreamProperties setActiveFormatIndexSelector (toNSNumber value)

-- | frameDuration
--
-- The frame duration.
--
-- The property key is CMIOExtensionPropertyStreamFrameDuration. The dictionary needs to be a dictionary representing a CMTime struct that is consistent with the frame duration specification provided by the current active format.
--
-- ObjC selector: @- frameDuration@
frameDuration :: IsCMIOExtensionStreamProperties cmioExtensionStreamProperties => cmioExtensionStreamProperties -> IO (Id NSDictionary)
frameDuration cmioExtensionStreamProperties =
  sendMessage cmioExtensionStreamProperties frameDurationSelector

-- | frameDuration
--
-- The frame duration.
--
-- The property key is CMIOExtensionPropertyStreamFrameDuration. The dictionary needs to be a dictionary representing a CMTime struct that is consistent with the frame duration specification provided by the current active format.
--
-- ObjC selector: @- setFrameDuration:@
setFrameDuration :: (IsCMIOExtensionStreamProperties cmioExtensionStreamProperties, IsNSDictionary value) => cmioExtensionStreamProperties -> value -> IO ()
setFrameDuration cmioExtensionStreamProperties value =
  sendMessage cmioExtensionStreamProperties setFrameDurationSelector (toNSDictionary value)

-- | maxFrameDuration
--
-- The maximum frame duration.
--
-- The property key is CMIOExtensionPropertyStreamMaxFrameDuration. The dictionary needs to be a dictionary representing a CMTime struct that is consistent with the frame duration specification provided by the current active format.
--
-- ObjC selector: @- maxFrameDuration@
maxFrameDuration :: IsCMIOExtensionStreamProperties cmioExtensionStreamProperties => cmioExtensionStreamProperties -> IO (Id NSDictionary)
maxFrameDuration cmioExtensionStreamProperties =
  sendMessage cmioExtensionStreamProperties maxFrameDurationSelector

-- | maxFrameDuration
--
-- The maximum frame duration.
--
-- The property key is CMIOExtensionPropertyStreamMaxFrameDuration. The dictionary needs to be a dictionary representing a CMTime struct that is consistent with the frame duration specification provided by the current active format.
--
-- ObjC selector: @- setMaxFrameDuration:@
setMaxFrameDuration :: (IsCMIOExtensionStreamProperties cmioExtensionStreamProperties, IsNSDictionary value) => cmioExtensionStreamProperties -> value -> IO ()
setMaxFrameDuration cmioExtensionStreamProperties value =
  sendMessage cmioExtensionStreamProperties setMaxFrameDurationSelector (toNSDictionary value)

-- | sinkBufferQueueSize
--
-- The sink stream property buffer queue size.
--
-- The property key is CMIOExtensionPropertyStreamSinkBufferQueueSize.
--
-- ObjC selector: @- sinkBufferQueueSize@
sinkBufferQueueSize :: IsCMIOExtensionStreamProperties cmioExtensionStreamProperties => cmioExtensionStreamProperties -> IO (Id NSNumber)
sinkBufferQueueSize cmioExtensionStreamProperties =
  sendMessage cmioExtensionStreamProperties sinkBufferQueueSizeSelector

-- | sinkBufferQueueSize
--
-- The sink stream property buffer queue size.
--
-- The property key is CMIOExtensionPropertyStreamSinkBufferQueueSize.
--
-- ObjC selector: @- setSinkBufferQueueSize:@
setSinkBufferQueueSize :: (IsCMIOExtensionStreamProperties cmioExtensionStreamProperties, IsNSNumber value) => cmioExtensionStreamProperties -> value -> IO ()
setSinkBufferQueueSize cmioExtensionStreamProperties value =
  sendMessage cmioExtensionStreamProperties setSinkBufferQueueSizeSelector (toNSNumber value)

-- | sinkBuffersRequiredForStartup
--
-- The sink stream property for number of buffers required for startup.
--
-- The property key is CMIOExtensionPropertyStreamSinkBuffersRequiredForStartup.
--
-- ObjC selector: @- sinkBuffersRequiredForStartup@
sinkBuffersRequiredForStartup :: IsCMIOExtensionStreamProperties cmioExtensionStreamProperties => cmioExtensionStreamProperties -> IO (Id NSNumber)
sinkBuffersRequiredForStartup cmioExtensionStreamProperties =
  sendMessage cmioExtensionStreamProperties sinkBuffersRequiredForStartupSelector

-- | sinkBuffersRequiredForStartup
--
-- The sink stream property for number of buffers required for startup.
--
-- The property key is CMIOExtensionPropertyStreamSinkBuffersRequiredForStartup.
--
-- ObjC selector: @- setSinkBuffersRequiredForStartup:@
setSinkBuffersRequiredForStartup :: (IsCMIOExtensionStreamProperties cmioExtensionStreamProperties, IsNSNumber value) => cmioExtensionStreamProperties -> value -> IO ()
setSinkBuffersRequiredForStartup cmioExtensionStreamProperties value =
  sendMessage cmioExtensionStreamProperties setSinkBuffersRequiredForStartupSelector (toNSNumber value)

-- | sinkBufferUnderrunCount
--
-- The sink stream property buffer underrun count.
--
-- The property key is CMIOExtensionPropertyStreamSinkBufferUnderrunCount.
--
-- ObjC selector: @- sinkBufferUnderrunCount@
sinkBufferUnderrunCount :: IsCMIOExtensionStreamProperties cmioExtensionStreamProperties => cmioExtensionStreamProperties -> IO (Id NSNumber)
sinkBufferUnderrunCount cmioExtensionStreamProperties =
  sendMessage cmioExtensionStreamProperties sinkBufferUnderrunCountSelector

-- | sinkBufferUnderrunCount
--
-- The sink stream property buffer underrun count.
--
-- The property key is CMIOExtensionPropertyStreamSinkBufferUnderrunCount.
--
-- ObjC selector: @- setSinkBufferUnderrunCount:@
setSinkBufferUnderrunCount :: (IsCMIOExtensionStreamProperties cmioExtensionStreamProperties, IsNSNumber value) => cmioExtensionStreamProperties -> value -> IO ()
setSinkBufferUnderrunCount cmioExtensionStreamProperties value =
  sendMessage cmioExtensionStreamProperties setSinkBufferUnderrunCountSelector (toNSNumber value)

-- | sinkEndOfData
--
-- The sink stream property end of data.
--
-- The property key is CMIOExtensionPropertyStreamSinkEndOfData.
--
-- ObjC selector: @- sinkEndOfData@
sinkEndOfData :: IsCMIOExtensionStreamProperties cmioExtensionStreamProperties => cmioExtensionStreamProperties -> IO (Id NSNumber)
sinkEndOfData cmioExtensionStreamProperties =
  sendMessage cmioExtensionStreamProperties sinkEndOfDataSelector

-- | sinkEndOfData
--
-- The sink stream property end of data.
--
-- The property key is CMIOExtensionPropertyStreamSinkEndOfData.
--
-- ObjC selector: @- setSinkEndOfData:@
setSinkEndOfData :: (IsCMIOExtensionStreamProperties cmioExtensionStreamProperties, IsNSNumber value) => cmioExtensionStreamProperties -> value -> IO ()
setSinkEndOfData cmioExtensionStreamProperties value =
  sendMessage cmioExtensionStreamProperties setSinkEndOfDataSelector (toNSNumber value)

-- | propertiesDictionary
--
-- The dictionary of properties.
--
-- The dictionary containing all keys and values.
--
-- ObjC selector: @- propertiesDictionary@
propertiesDictionary :: IsCMIOExtensionStreamProperties cmioExtensionStreamProperties => cmioExtensionStreamProperties -> IO (Id NSDictionary)
propertiesDictionary cmioExtensionStreamProperties =
  sendMessage cmioExtensionStreamProperties propertiesDictionarySelector

-- | propertiesDictionary
--
-- The dictionary of properties.
--
-- The dictionary containing all keys and values.
--
-- ObjC selector: @- setPropertiesDictionary:@
setPropertiesDictionary :: (IsCMIOExtensionStreamProperties cmioExtensionStreamProperties, IsNSDictionary value) => cmioExtensionStreamProperties -> value -> IO ()
setPropertiesDictionary cmioExtensionStreamProperties value =
  sendMessage cmioExtensionStreamProperties setPropertiesDictionarySelector (toNSDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CMIOExtensionStreamProperties)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CMIOExtensionStreamProperties)
newSelector = mkSelector "new"

-- | @Selector@ for @streamPropertiesWithDictionary:@
streamPropertiesWithDictionarySelector :: Selector '[Id NSDictionary] (Id CMIOExtensionStreamProperties)
streamPropertiesWithDictionarySelector = mkSelector "streamPropertiesWithDictionary:"

-- | @Selector@ for @initWithDictionary:@
initWithDictionarySelector :: Selector '[Id NSDictionary] (Id CMIOExtensionStreamProperties)
initWithDictionarySelector = mkSelector "initWithDictionary:"

-- | @Selector@ for @setPropertyState:forProperty:@
setPropertyState_forPropertySelector :: Selector '[Id CMIOExtensionPropertyState, Id NSString] ()
setPropertyState_forPropertySelector = mkSelector "setPropertyState:forProperty:"

-- | @Selector@ for @activeFormatIndex@
activeFormatIndexSelector :: Selector '[] (Id NSNumber)
activeFormatIndexSelector = mkSelector "activeFormatIndex"

-- | @Selector@ for @setActiveFormatIndex:@
setActiveFormatIndexSelector :: Selector '[Id NSNumber] ()
setActiveFormatIndexSelector = mkSelector "setActiveFormatIndex:"

-- | @Selector@ for @frameDuration@
frameDurationSelector :: Selector '[] (Id NSDictionary)
frameDurationSelector = mkSelector "frameDuration"

-- | @Selector@ for @setFrameDuration:@
setFrameDurationSelector :: Selector '[Id NSDictionary] ()
setFrameDurationSelector = mkSelector "setFrameDuration:"

-- | @Selector@ for @maxFrameDuration@
maxFrameDurationSelector :: Selector '[] (Id NSDictionary)
maxFrameDurationSelector = mkSelector "maxFrameDuration"

-- | @Selector@ for @setMaxFrameDuration:@
setMaxFrameDurationSelector :: Selector '[Id NSDictionary] ()
setMaxFrameDurationSelector = mkSelector "setMaxFrameDuration:"

-- | @Selector@ for @sinkBufferQueueSize@
sinkBufferQueueSizeSelector :: Selector '[] (Id NSNumber)
sinkBufferQueueSizeSelector = mkSelector "sinkBufferQueueSize"

-- | @Selector@ for @setSinkBufferQueueSize:@
setSinkBufferQueueSizeSelector :: Selector '[Id NSNumber] ()
setSinkBufferQueueSizeSelector = mkSelector "setSinkBufferQueueSize:"

-- | @Selector@ for @sinkBuffersRequiredForStartup@
sinkBuffersRequiredForStartupSelector :: Selector '[] (Id NSNumber)
sinkBuffersRequiredForStartupSelector = mkSelector "sinkBuffersRequiredForStartup"

-- | @Selector@ for @setSinkBuffersRequiredForStartup:@
setSinkBuffersRequiredForStartupSelector :: Selector '[Id NSNumber] ()
setSinkBuffersRequiredForStartupSelector = mkSelector "setSinkBuffersRequiredForStartup:"

-- | @Selector@ for @sinkBufferUnderrunCount@
sinkBufferUnderrunCountSelector :: Selector '[] (Id NSNumber)
sinkBufferUnderrunCountSelector = mkSelector "sinkBufferUnderrunCount"

-- | @Selector@ for @setSinkBufferUnderrunCount:@
setSinkBufferUnderrunCountSelector :: Selector '[Id NSNumber] ()
setSinkBufferUnderrunCountSelector = mkSelector "setSinkBufferUnderrunCount:"

-- | @Selector@ for @sinkEndOfData@
sinkEndOfDataSelector :: Selector '[] (Id NSNumber)
sinkEndOfDataSelector = mkSelector "sinkEndOfData"

-- | @Selector@ for @setSinkEndOfData:@
setSinkEndOfDataSelector :: Selector '[Id NSNumber] ()
setSinkEndOfDataSelector = mkSelector "setSinkEndOfData:"

-- | @Selector@ for @propertiesDictionary@
propertiesDictionarySelector :: Selector '[] (Id NSDictionary)
propertiesDictionarySelector = mkSelector "propertiesDictionary"

-- | @Selector@ for @setPropertiesDictionary:@
setPropertiesDictionarySelector :: Selector '[Id NSDictionary] ()
setPropertiesDictionarySelector = mkSelector "setPropertiesDictionary:"

