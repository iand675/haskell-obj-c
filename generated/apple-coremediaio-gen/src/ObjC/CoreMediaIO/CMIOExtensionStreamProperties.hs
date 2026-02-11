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
  , initSelector
  , newSelector
  , streamPropertiesWithDictionarySelector
  , initWithDictionarySelector
  , setPropertyState_forPropertySelector
  , activeFormatIndexSelector
  , setActiveFormatIndexSelector
  , frameDurationSelector
  , setFrameDurationSelector
  , maxFrameDurationSelector
  , setMaxFrameDurationSelector
  , sinkBufferQueueSizeSelector
  , setSinkBufferQueueSizeSelector
  , sinkBuffersRequiredForStartupSelector
  , setSinkBuffersRequiredForStartupSelector
  , sinkBufferUnderrunCountSelector
  , setSinkBufferUnderrunCountSelector
  , sinkEndOfDataSelector
  , setSinkEndOfDataSelector
  , propertiesDictionarySelector
  , setPropertiesDictionarySelector


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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCMIOExtensionStreamProperties cmioExtensionStreamProperties => cmioExtensionStreamProperties -> IO (Id CMIOExtensionStreamProperties)
init_ cmioExtensionStreamProperties  =
    sendMsg cmioExtensionStreamProperties (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CMIOExtensionStreamProperties)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionStreamProperties"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    withObjCPtr propertiesDictionary $ \raw_propertiesDictionary ->
      sendClassMsg cls' (mkSelector "streamPropertiesWithDictionary:") (retPtr retVoid) [argPtr (castPtr raw_propertiesDictionary :: Ptr ())] >>= retainedObject . castPtr

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
initWithDictionary cmioExtensionStreamProperties  propertiesDictionary =
  withObjCPtr propertiesDictionary $ \raw_propertiesDictionary ->
      sendMsg cmioExtensionStreamProperties (mkSelector "initWithDictionary:") (retPtr retVoid) [argPtr (castPtr raw_propertiesDictionary :: Ptr ())] >>= ownedObject . castPtr

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
setPropertyState_forProperty cmioExtensionStreamProperties  propertyState property =
  withObjCPtr propertyState $ \raw_propertyState ->
    withObjCPtr property $ \raw_property ->
        sendMsg cmioExtensionStreamProperties (mkSelector "setPropertyState:forProperty:") retVoid [argPtr (castPtr raw_propertyState :: Ptr ()), argPtr (castPtr raw_property :: Ptr ())]

-- | activeFormatIndex
--
-- The active format index.
--
-- The property key is CMIOExtensionPropertyStreamActiveFormatIndex.
--
-- ObjC selector: @- activeFormatIndex@
activeFormatIndex :: IsCMIOExtensionStreamProperties cmioExtensionStreamProperties => cmioExtensionStreamProperties -> IO (Id NSNumber)
activeFormatIndex cmioExtensionStreamProperties  =
    sendMsg cmioExtensionStreamProperties (mkSelector "activeFormatIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | activeFormatIndex
--
-- The active format index.
--
-- The property key is CMIOExtensionPropertyStreamActiveFormatIndex.
--
-- ObjC selector: @- setActiveFormatIndex:@
setActiveFormatIndex :: (IsCMIOExtensionStreamProperties cmioExtensionStreamProperties, IsNSNumber value) => cmioExtensionStreamProperties -> value -> IO ()
setActiveFormatIndex cmioExtensionStreamProperties  value =
  withObjCPtr value $ \raw_value ->
      sendMsg cmioExtensionStreamProperties (mkSelector "setActiveFormatIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | frameDuration
--
-- The frame duration.
--
-- The property key is CMIOExtensionPropertyStreamFrameDuration. The dictionary needs to be a dictionary representing a CMTime struct that is consistent with the frame duration specification provided by the current active format.
--
-- ObjC selector: @- frameDuration@
frameDuration :: IsCMIOExtensionStreamProperties cmioExtensionStreamProperties => cmioExtensionStreamProperties -> IO (Id NSDictionary)
frameDuration cmioExtensionStreamProperties  =
    sendMsg cmioExtensionStreamProperties (mkSelector "frameDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | frameDuration
--
-- The frame duration.
--
-- The property key is CMIOExtensionPropertyStreamFrameDuration. The dictionary needs to be a dictionary representing a CMTime struct that is consistent with the frame duration specification provided by the current active format.
--
-- ObjC selector: @- setFrameDuration:@
setFrameDuration :: (IsCMIOExtensionStreamProperties cmioExtensionStreamProperties, IsNSDictionary value) => cmioExtensionStreamProperties -> value -> IO ()
setFrameDuration cmioExtensionStreamProperties  value =
  withObjCPtr value $ \raw_value ->
      sendMsg cmioExtensionStreamProperties (mkSelector "setFrameDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | maxFrameDuration
--
-- The maximum frame duration.
--
-- The property key is CMIOExtensionPropertyStreamMaxFrameDuration. The dictionary needs to be a dictionary representing a CMTime struct that is consistent with the frame duration specification provided by the current active format.
--
-- ObjC selector: @- maxFrameDuration@
maxFrameDuration :: IsCMIOExtensionStreamProperties cmioExtensionStreamProperties => cmioExtensionStreamProperties -> IO (Id NSDictionary)
maxFrameDuration cmioExtensionStreamProperties  =
    sendMsg cmioExtensionStreamProperties (mkSelector "maxFrameDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | maxFrameDuration
--
-- The maximum frame duration.
--
-- The property key is CMIOExtensionPropertyStreamMaxFrameDuration. The dictionary needs to be a dictionary representing a CMTime struct that is consistent with the frame duration specification provided by the current active format.
--
-- ObjC selector: @- setMaxFrameDuration:@
setMaxFrameDuration :: (IsCMIOExtensionStreamProperties cmioExtensionStreamProperties, IsNSDictionary value) => cmioExtensionStreamProperties -> value -> IO ()
setMaxFrameDuration cmioExtensionStreamProperties  value =
  withObjCPtr value $ \raw_value ->
      sendMsg cmioExtensionStreamProperties (mkSelector "setMaxFrameDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | sinkBufferQueueSize
--
-- The sink stream property buffer queue size.
--
-- The property key is CMIOExtensionPropertyStreamSinkBufferQueueSize.
--
-- ObjC selector: @- sinkBufferQueueSize@
sinkBufferQueueSize :: IsCMIOExtensionStreamProperties cmioExtensionStreamProperties => cmioExtensionStreamProperties -> IO (Id NSNumber)
sinkBufferQueueSize cmioExtensionStreamProperties  =
    sendMsg cmioExtensionStreamProperties (mkSelector "sinkBufferQueueSize") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sinkBufferQueueSize
--
-- The sink stream property buffer queue size.
--
-- The property key is CMIOExtensionPropertyStreamSinkBufferQueueSize.
--
-- ObjC selector: @- setSinkBufferQueueSize:@
setSinkBufferQueueSize :: (IsCMIOExtensionStreamProperties cmioExtensionStreamProperties, IsNSNumber value) => cmioExtensionStreamProperties -> value -> IO ()
setSinkBufferQueueSize cmioExtensionStreamProperties  value =
  withObjCPtr value $ \raw_value ->
      sendMsg cmioExtensionStreamProperties (mkSelector "setSinkBufferQueueSize:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | sinkBuffersRequiredForStartup
--
-- The sink stream property for number of buffers required for startup.
--
-- The property key is CMIOExtensionPropertyStreamSinkBuffersRequiredForStartup.
--
-- ObjC selector: @- sinkBuffersRequiredForStartup@
sinkBuffersRequiredForStartup :: IsCMIOExtensionStreamProperties cmioExtensionStreamProperties => cmioExtensionStreamProperties -> IO (Id NSNumber)
sinkBuffersRequiredForStartup cmioExtensionStreamProperties  =
    sendMsg cmioExtensionStreamProperties (mkSelector "sinkBuffersRequiredForStartup") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sinkBuffersRequiredForStartup
--
-- The sink stream property for number of buffers required for startup.
--
-- The property key is CMIOExtensionPropertyStreamSinkBuffersRequiredForStartup.
--
-- ObjC selector: @- setSinkBuffersRequiredForStartup:@
setSinkBuffersRequiredForStartup :: (IsCMIOExtensionStreamProperties cmioExtensionStreamProperties, IsNSNumber value) => cmioExtensionStreamProperties -> value -> IO ()
setSinkBuffersRequiredForStartup cmioExtensionStreamProperties  value =
  withObjCPtr value $ \raw_value ->
      sendMsg cmioExtensionStreamProperties (mkSelector "setSinkBuffersRequiredForStartup:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | sinkBufferUnderrunCount
--
-- The sink stream property buffer underrun count.
--
-- The property key is CMIOExtensionPropertyStreamSinkBufferUnderrunCount.
--
-- ObjC selector: @- sinkBufferUnderrunCount@
sinkBufferUnderrunCount :: IsCMIOExtensionStreamProperties cmioExtensionStreamProperties => cmioExtensionStreamProperties -> IO (Id NSNumber)
sinkBufferUnderrunCount cmioExtensionStreamProperties  =
    sendMsg cmioExtensionStreamProperties (mkSelector "sinkBufferUnderrunCount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sinkBufferUnderrunCount
--
-- The sink stream property buffer underrun count.
--
-- The property key is CMIOExtensionPropertyStreamSinkBufferUnderrunCount.
--
-- ObjC selector: @- setSinkBufferUnderrunCount:@
setSinkBufferUnderrunCount :: (IsCMIOExtensionStreamProperties cmioExtensionStreamProperties, IsNSNumber value) => cmioExtensionStreamProperties -> value -> IO ()
setSinkBufferUnderrunCount cmioExtensionStreamProperties  value =
  withObjCPtr value $ \raw_value ->
      sendMsg cmioExtensionStreamProperties (mkSelector "setSinkBufferUnderrunCount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | sinkEndOfData
--
-- The sink stream property end of data.
--
-- The property key is CMIOExtensionPropertyStreamSinkEndOfData.
--
-- ObjC selector: @- sinkEndOfData@
sinkEndOfData :: IsCMIOExtensionStreamProperties cmioExtensionStreamProperties => cmioExtensionStreamProperties -> IO (Id NSNumber)
sinkEndOfData cmioExtensionStreamProperties  =
    sendMsg cmioExtensionStreamProperties (mkSelector "sinkEndOfData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sinkEndOfData
--
-- The sink stream property end of data.
--
-- The property key is CMIOExtensionPropertyStreamSinkEndOfData.
--
-- ObjC selector: @- setSinkEndOfData:@
setSinkEndOfData :: (IsCMIOExtensionStreamProperties cmioExtensionStreamProperties, IsNSNumber value) => cmioExtensionStreamProperties -> value -> IO ()
setSinkEndOfData cmioExtensionStreamProperties  value =
  withObjCPtr value $ \raw_value ->
      sendMsg cmioExtensionStreamProperties (mkSelector "setSinkEndOfData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | propertiesDictionary
--
-- The dictionary of properties.
--
-- The dictionary containing all keys and values.
--
-- ObjC selector: @- propertiesDictionary@
propertiesDictionary :: IsCMIOExtensionStreamProperties cmioExtensionStreamProperties => cmioExtensionStreamProperties -> IO (Id NSDictionary)
propertiesDictionary cmioExtensionStreamProperties  =
    sendMsg cmioExtensionStreamProperties (mkSelector "propertiesDictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | propertiesDictionary
--
-- The dictionary of properties.
--
-- The dictionary containing all keys and values.
--
-- ObjC selector: @- setPropertiesDictionary:@
setPropertiesDictionary :: (IsCMIOExtensionStreamProperties cmioExtensionStreamProperties, IsNSDictionary value) => cmioExtensionStreamProperties -> value -> IO ()
setPropertiesDictionary cmioExtensionStreamProperties  value =
  withObjCPtr value $ \raw_value ->
      sendMsg cmioExtensionStreamProperties (mkSelector "setPropertiesDictionary:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @streamPropertiesWithDictionary:@
streamPropertiesWithDictionarySelector :: Selector
streamPropertiesWithDictionarySelector = mkSelector "streamPropertiesWithDictionary:"

-- | @Selector@ for @initWithDictionary:@
initWithDictionarySelector :: Selector
initWithDictionarySelector = mkSelector "initWithDictionary:"

-- | @Selector@ for @setPropertyState:forProperty:@
setPropertyState_forPropertySelector :: Selector
setPropertyState_forPropertySelector = mkSelector "setPropertyState:forProperty:"

-- | @Selector@ for @activeFormatIndex@
activeFormatIndexSelector :: Selector
activeFormatIndexSelector = mkSelector "activeFormatIndex"

-- | @Selector@ for @setActiveFormatIndex:@
setActiveFormatIndexSelector :: Selector
setActiveFormatIndexSelector = mkSelector "setActiveFormatIndex:"

-- | @Selector@ for @frameDuration@
frameDurationSelector :: Selector
frameDurationSelector = mkSelector "frameDuration"

-- | @Selector@ for @setFrameDuration:@
setFrameDurationSelector :: Selector
setFrameDurationSelector = mkSelector "setFrameDuration:"

-- | @Selector@ for @maxFrameDuration@
maxFrameDurationSelector :: Selector
maxFrameDurationSelector = mkSelector "maxFrameDuration"

-- | @Selector@ for @setMaxFrameDuration:@
setMaxFrameDurationSelector :: Selector
setMaxFrameDurationSelector = mkSelector "setMaxFrameDuration:"

-- | @Selector@ for @sinkBufferQueueSize@
sinkBufferQueueSizeSelector :: Selector
sinkBufferQueueSizeSelector = mkSelector "sinkBufferQueueSize"

-- | @Selector@ for @setSinkBufferQueueSize:@
setSinkBufferQueueSizeSelector :: Selector
setSinkBufferQueueSizeSelector = mkSelector "setSinkBufferQueueSize:"

-- | @Selector@ for @sinkBuffersRequiredForStartup@
sinkBuffersRequiredForStartupSelector :: Selector
sinkBuffersRequiredForStartupSelector = mkSelector "sinkBuffersRequiredForStartup"

-- | @Selector@ for @setSinkBuffersRequiredForStartup:@
setSinkBuffersRequiredForStartupSelector :: Selector
setSinkBuffersRequiredForStartupSelector = mkSelector "setSinkBuffersRequiredForStartup:"

-- | @Selector@ for @sinkBufferUnderrunCount@
sinkBufferUnderrunCountSelector :: Selector
sinkBufferUnderrunCountSelector = mkSelector "sinkBufferUnderrunCount"

-- | @Selector@ for @setSinkBufferUnderrunCount:@
setSinkBufferUnderrunCountSelector :: Selector
setSinkBufferUnderrunCountSelector = mkSelector "setSinkBufferUnderrunCount:"

-- | @Selector@ for @sinkEndOfData@
sinkEndOfDataSelector :: Selector
sinkEndOfDataSelector = mkSelector "sinkEndOfData"

-- | @Selector@ for @setSinkEndOfData:@
setSinkEndOfDataSelector :: Selector
setSinkEndOfDataSelector = mkSelector "setSinkEndOfData:"

-- | @Selector@ for @propertiesDictionary@
propertiesDictionarySelector :: Selector
propertiesDictionarySelector = mkSelector "propertiesDictionary"

-- | @Selector@ for @setPropertiesDictionary:@
setPropertiesDictionarySelector :: Selector
setPropertiesDictionarySelector = mkSelector "setPropertiesDictionary:"

