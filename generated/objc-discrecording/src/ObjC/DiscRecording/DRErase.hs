{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DRErase@.
module ObjC.DiscRecording.DRErase
  ( DRErase
  , IsDRErase(..)
  , eraseForDevice
  , initWithDevice
  , start
  , status
  , properties
  , setProperties
  , device
  , eraseType
  , setEraseType
  , eraseForDeviceSelector
  , initWithDeviceSelector
  , startSelector
  , statusSelector
  , propertiesSelector
  , setPropertiesSelector
  , deviceSelector
  , eraseTypeSelector
  , setEraseTypeSelector


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

import ObjC.DiscRecording.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | eraseForDevice:
--
-- Creates and returns an erase object.
--
-- An erase object created with this method is ready to erase media.
--
-- @device@ — Device to use for the erase.
--
-- Returns: An autoreleased DRErase object.
--
-- ObjC selector: @+ eraseForDevice:@
eraseForDevice :: IsDRDevice device => device -> IO (Id DRErase)
eraseForDevice device =
  do
    cls' <- getRequiredClass "DRErase"
    withObjCPtr device $ \raw_device ->
      sendClassMsg cls' (mkSelector "eraseForDevice:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ())] >>= retainedObject . castPtr

-- | initWithDevice:
--
-- Initializes an erase object.
--
-- An erase object created with this method is ready to erase media.
--
-- @device@ — Device to use for the erase.
--
-- Returns: A DRErase object.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: (IsDRErase drErase, IsDRDevice device) => drErase -> device -> IO RawId
initWithDevice drErase  device =
withObjCPtr device $ \raw_device ->
    fmap (RawId . castPtr) $ sendMsg drErase (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ())]

-- | start
--
-- Begin the process of erasing media.
--
-- This method only kicks off the erase. Once the erasure starts,					control returns to the caller. The caller can monitor the progress 					of the erase by listening for a notification or by polling
--
-- //apple_ref/occ/instm/DRErase/status status
--
-- .
--
-- ObjC selector: @- start@
start :: IsDRErase drErase => drErase -> IO ()
start drErase  =
  sendMsg drErase (mkSelector "start") retVoid []

-- | status
--
-- Returns a dictionary containing the status of the erase.
--
-- The same dictionary is returned 					through the
--
-- //apple_ref/occ/data/DREraseStatusChangedNotification DREraseStatusChangedNotification
--
-- notification.
--
-- Returns: An NSDictionary containing the status of the erase.
--
-- ObjC selector: @- status@
status :: IsDRErase drErase => drErase -> IO (Id NSDictionary)
status drErase  =
  sendMsg drErase (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | properties
--
-- Returns the properties dictionary of the erase.
--
-- Returns: An NSDictionary containing the properties of the erase.
--
-- ObjC selector: @- properties@
properties :: IsDRErase drErase => drErase -> IO (Id NSDictionary)
properties drErase  =
  sendMsg drErase (mkSelector "properties") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setProperties:
--
-- Sets the properties dictionary of the erase
--
-- @properties@ — NSDictionary of the properties to set.
--
-- ObjC selector: @- setProperties:@
setProperties :: (IsDRErase drErase, IsNSDictionary properties) => drErase -> properties -> IO ()
setProperties drErase  properties =
withObjCPtr properties $ \raw_properties ->
    sendMsg drErase (mkSelector "setProperties:") retVoid [argPtr (castPtr raw_properties :: Ptr ())]

-- | device
--
-- Returns the device being used for the erase.
--
-- Returns: The DRDevice the erase will use.
--
-- ObjC selector: @- device@
device :: IsDRErase drErase => drErase -> IO (Id DRDevice)
device drErase  =
  sendMsg drErase (mkSelector "device") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | eraseType
--
-- Returns the type of erase to be performed.
--
-- Returns: An NSString
--
-- ObjC selector: @- eraseType@
eraseType :: IsDRErase drErase => drErase -> IO (Id NSString)
eraseType drErase  =
  sendMsg drErase (mkSelector "eraseType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setEraseType:
--
-- Sets the type of erase to perform.
--
-- @type@ — The type of erase.
--
-- ObjC selector: @- setEraseType:@
setEraseType :: (IsDRErase drErase, IsNSString type_) => drErase -> type_ -> IO ()
setEraseType drErase  type_ =
withObjCPtr type_ $ \raw_type_ ->
    sendMsg drErase (mkSelector "setEraseType:") retVoid [argPtr (castPtr raw_type_ :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @eraseForDevice:@
eraseForDeviceSelector :: Selector
eraseForDeviceSelector = mkSelector "eraseForDevice:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @start@
startSelector :: Selector
startSelector = mkSelector "start"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @properties@
propertiesSelector :: Selector
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @setProperties:@
setPropertiesSelector :: Selector
setPropertiesSelector = mkSelector "setProperties:"

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

-- | @Selector@ for @eraseType@
eraseTypeSelector :: Selector
eraseTypeSelector = mkSelector "eraseType"

-- | @Selector@ for @setEraseType:@
setEraseTypeSelector :: Selector
setEraseTypeSelector = mkSelector "setEraseType:"

