{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSKeyedUnarchiver
--
-- A NSKeyedArchiver that supports the MPSDeviceProvider protocol for MPSKernel decoding
--
-- Generated bindings for @MPSKeyedUnarchiver@.
module ObjC.MetalPerformanceShaders.MPSKeyedUnarchiver
  ( MPSKeyedUnarchiver
  , IsMPSKeyedUnarchiver(..)
  , unarchivedObjectOfClasses_fromData_device_error
  , unarchivedObjectOfClass_fromData_device_error
  , initForReadingFromData_device_error
  , mpsMTLDevice
  , unarchivedObjectOfClasses_fromData_error
  , unarchivedObjectOfClass_fromData_error
  , init_
  , initForReadingFromData_error
  , unarchiveObjectWithData
  , unarchiveObjectWithData_device
  , unarchiveTopLevelObjectWithData_error
  , unarchiveTopLevelObjectWithData_device_error
  , unarchiveObjectWithFile
  , initForReadingWithData
  , unarchiveObjectWithFile_device
  , initWithDevice
  , initForReadingWithData_device
  , unarchivedObjectOfClasses_fromData_device_errorSelector
  , unarchivedObjectOfClass_fromData_device_errorSelector
  , initForReadingFromData_device_errorSelector
  , mpsMTLDeviceSelector
  , unarchivedObjectOfClasses_fromData_errorSelector
  , unarchivedObjectOfClass_fromData_errorSelector
  , initSelector
  , initForReadingFromData_errorSelector
  , unarchiveObjectWithDataSelector
  , unarchiveObjectWithData_deviceSelector
  , unarchiveTopLevelObjectWithData_errorSelector
  , unarchiveTopLevelObjectWithData_device_errorSelector
  , unarchiveObjectWithFileSelector
  , initForReadingWithDataSelector
  , unarchiveObjectWithFile_deviceSelector
  , initWithDeviceSelector
  , initForReadingWithData_deviceSelector


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

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ unarchivedObjectOfClasses:fromData:device:error:@
unarchivedObjectOfClasses_fromData_device_error :: (IsNSSet classes, IsNSData data_, IsNSError error_) => classes -> data_ -> RawId -> error_ -> IO RawId
unarchivedObjectOfClasses_fromData_device_error classes data_ device error_ =
  do
    cls' <- getRequiredClass "MPSKeyedUnarchiver"
    withObjCPtr classes $ \raw_classes ->
      withObjCPtr data_ $ \raw_data_ ->
        withObjCPtr error_ $ \raw_error_ ->
          fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "unarchivedObjectOfClasses:fromData:device:error:") (retPtr retVoid) [argPtr (castPtr raw_classes :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ unarchivedObjectOfClass:fromData:device:error:@
unarchivedObjectOfClass_fromData_device_error :: (IsNSData data_, IsNSError error_) => Class -> data_ -> RawId -> error_ -> IO RawId
unarchivedObjectOfClass_fromData_device_error cls data_ device error_ =
  do
    cls' <- getRequiredClass "MPSKeyedUnarchiver"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "unarchivedObjectOfClass:fromData:device:error:") (retPtr retVoid) [argPtr (unClass cls), argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- initForReadingFromData:device:error:@
initForReadingFromData_device_error :: (IsMPSKeyedUnarchiver mpsKeyedUnarchiver, IsNSData data_, IsNSError error_) => mpsKeyedUnarchiver -> data_ -> RawId -> error_ -> IO (Id MPSKeyedUnarchiver)
initForReadingFromData_device_error mpsKeyedUnarchiver  data_ device error_ =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg mpsKeyedUnarchiver (mkSelector "initForReadingFromData:device:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | Reports which device to use for unarchiving MPSKernels
--
-- ObjC selector: @- mpsMTLDevice@
mpsMTLDevice :: IsMPSKeyedUnarchiver mpsKeyedUnarchiver => mpsKeyedUnarchiver -> IO RawId
mpsMTLDevice mpsKeyedUnarchiver  =
  fmap (RawId . castPtr) $ sendMsg mpsKeyedUnarchiver (mkSelector "mpsMTLDevice") (retPtr retVoid) []

-- | @+ unarchivedObjectOfClasses:fromData:error:@
unarchivedObjectOfClasses_fromData_error :: (IsNSSet classes, IsNSData data_, IsNSError error_) => classes -> data_ -> error_ -> IO RawId
unarchivedObjectOfClasses_fromData_error classes data_ error_ =
  do
    cls' <- getRequiredClass "MPSKeyedUnarchiver"
    withObjCPtr classes $ \raw_classes ->
      withObjCPtr data_ $ \raw_data_ ->
        withObjCPtr error_ $ \raw_error_ ->
          fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "unarchivedObjectOfClasses:fromData:error:") (retPtr retVoid) [argPtr (castPtr raw_classes :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ unarchivedObjectOfClass:fromData:error:@
unarchivedObjectOfClass_fromData_error :: (IsNSData data_, IsNSError error_) => Class -> data_ -> error_ -> IO RawId
unarchivedObjectOfClass_fromData_error cls data_ error_ =
  do
    cls' <- getRequiredClass "MPSKeyedUnarchiver"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "unarchivedObjectOfClass:fromData:error:") (retPtr retVoid) [argPtr (unClass cls), argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- init@
init_ :: IsMPSKeyedUnarchiver mpsKeyedUnarchiver => mpsKeyedUnarchiver -> IO (Id MPSKeyedUnarchiver)
init_ mpsKeyedUnarchiver  =
  sendMsg mpsKeyedUnarchiver (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initForReadingFromData:error:@
initForReadingFromData_error :: (IsMPSKeyedUnarchiver mpsKeyedUnarchiver, IsNSData data_, IsNSError error_) => mpsKeyedUnarchiver -> data_ -> error_ -> IO (Id MPSKeyedUnarchiver)
initForReadingFromData_error mpsKeyedUnarchiver  data_ error_ =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg mpsKeyedUnarchiver (mkSelector "initForReadingFromData:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @+ unarchiveObjectWithData:@
unarchiveObjectWithData :: IsNSData data_ => data_ -> IO RawId
unarchiveObjectWithData data_ =
  do
    cls' <- getRequiredClass "MPSKeyedUnarchiver"
    withObjCPtr data_ $ \raw_data_ ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "unarchiveObjectWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())]

-- | @+ unarchiveObjectWithData:device:@
unarchiveObjectWithData_device :: IsNSData data_ => data_ -> RawId -> IO RawId
unarchiveObjectWithData_device data_ device =
  do
    cls' <- getRequiredClass "MPSKeyedUnarchiver"
    withObjCPtr data_ $ \raw_data_ ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "unarchiveObjectWithData:device:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())]

-- | @+ unarchiveTopLevelObjectWithData:error:@
unarchiveTopLevelObjectWithData_error :: (IsNSData data_, IsNSError error_) => data_ -> error_ -> IO RawId
unarchiveTopLevelObjectWithData_error data_ error_ =
  do
    cls' <- getRequiredClass "MPSKeyedUnarchiver"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "unarchiveTopLevelObjectWithData:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ unarchiveTopLevelObjectWithData:device:error:@
unarchiveTopLevelObjectWithData_device_error :: (IsNSData data_, IsNSError error_) => data_ -> RawId -> error_ -> IO RawId
unarchiveTopLevelObjectWithData_device_error data_ device error_ =
  do
    cls' <- getRequiredClass "MPSKeyedUnarchiver"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "unarchiveTopLevelObjectWithData:device:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ unarchiveObjectWithFile:@
unarchiveObjectWithFile :: IsNSString path => path -> IO RawId
unarchiveObjectWithFile path =
  do
    cls' <- getRequiredClass "MPSKeyedUnarchiver"
    withObjCPtr path $ \raw_path ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "unarchiveObjectWithFile:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())]

-- | @- initForReadingWithData:@
initForReadingWithData :: (IsMPSKeyedUnarchiver mpsKeyedUnarchiver, IsNSData data_) => mpsKeyedUnarchiver -> data_ -> IO (Id MPSKeyedUnarchiver)
initForReadingWithData mpsKeyedUnarchiver  data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg mpsKeyedUnarchiver (mkSelector "initForReadingWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | @+ unarchiveObjectWithFile:device:@
unarchiveObjectWithFile_device :: IsNSString path => path -> RawId -> IO RawId
unarchiveObjectWithFile_device path device =
  do
    cls' <- getRequiredClass "MPSKeyedUnarchiver"
    withObjCPtr path $ \raw_path ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "unarchiveObjectWithFile:device:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())]

-- | @- initWithDevice:@
initWithDevice :: IsMPSKeyedUnarchiver mpsKeyedUnarchiver => mpsKeyedUnarchiver -> RawId -> IO (Id MPSKeyedUnarchiver)
initWithDevice mpsKeyedUnarchiver  device =
  sendMsg mpsKeyedUnarchiver (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initForReadingWithData:device:@
initForReadingWithData_device :: (IsMPSKeyedUnarchiver mpsKeyedUnarchiver, IsNSData data_) => mpsKeyedUnarchiver -> data_ -> RawId -> IO (Id MPSKeyedUnarchiver)
initForReadingWithData_device mpsKeyedUnarchiver  data_ device =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg mpsKeyedUnarchiver (mkSelector "initForReadingWithData:device:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unarchivedObjectOfClasses:fromData:device:error:@
unarchivedObjectOfClasses_fromData_device_errorSelector :: Selector
unarchivedObjectOfClasses_fromData_device_errorSelector = mkSelector "unarchivedObjectOfClasses:fromData:device:error:"

-- | @Selector@ for @unarchivedObjectOfClass:fromData:device:error:@
unarchivedObjectOfClass_fromData_device_errorSelector :: Selector
unarchivedObjectOfClass_fromData_device_errorSelector = mkSelector "unarchivedObjectOfClass:fromData:device:error:"

-- | @Selector@ for @initForReadingFromData:device:error:@
initForReadingFromData_device_errorSelector :: Selector
initForReadingFromData_device_errorSelector = mkSelector "initForReadingFromData:device:error:"

-- | @Selector@ for @mpsMTLDevice@
mpsMTLDeviceSelector :: Selector
mpsMTLDeviceSelector = mkSelector "mpsMTLDevice"

-- | @Selector@ for @unarchivedObjectOfClasses:fromData:error:@
unarchivedObjectOfClasses_fromData_errorSelector :: Selector
unarchivedObjectOfClasses_fromData_errorSelector = mkSelector "unarchivedObjectOfClasses:fromData:error:"

-- | @Selector@ for @unarchivedObjectOfClass:fromData:error:@
unarchivedObjectOfClass_fromData_errorSelector :: Selector
unarchivedObjectOfClass_fromData_errorSelector = mkSelector "unarchivedObjectOfClass:fromData:error:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initForReadingFromData:error:@
initForReadingFromData_errorSelector :: Selector
initForReadingFromData_errorSelector = mkSelector "initForReadingFromData:error:"

-- | @Selector@ for @unarchiveObjectWithData:@
unarchiveObjectWithDataSelector :: Selector
unarchiveObjectWithDataSelector = mkSelector "unarchiveObjectWithData:"

-- | @Selector@ for @unarchiveObjectWithData:device:@
unarchiveObjectWithData_deviceSelector :: Selector
unarchiveObjectWithData_deviceSelector = mkSelector "unarchiveObjectWithData:device:"

-- | @Selector@ for @unarchiveTopLevelObjectWithData:error:@
unarchiveTopLevelObjectWithData_errorSelector :: Selector
unarchiveTopLevelObjectWithData_errorSelector = mkSelector "unarchiveTopLevelObjectWithData:error:"

-- | @Selector@ for @unarchiveTopLevelObjectWithData:device:error:@
unarchiveTopLevelObjectWithData_device_errorSelector :: Selector
unarchiveTopLevelObjectWithData_device_errorSelector = mkSelector "unarchiveTopLevelObjectWithData:device:error:"

-- | @Selector@ for @unarchiveObjectWithFile:@
unarchiveObjectWithFileSelector :: Selector
unarchiveObjectWithFileSelector = mkSelector "unarchiveObjectWithFile:"

-- | @Selector@ for @initForReadingWithData:@
initForReadingWithDataSelector :: Selector
initForReadingWithDataSelector = mkSelector "initForReadingWithData:"

-- | @Selector@ for @unarchiveObjectWithFile:device:@
unarchiveObjectWithFile_deviceSelector :: Selector
unarchiveObjectWithFile_deviceSelector = mkSelector "unarchiveObjectWithFile:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initForReadingWithData:device:@
initForReadingWithData_deviceSelector :: Selector
initForReadingWithData_deviceSelector = mkSelector "initForReadingWithData:device:"

