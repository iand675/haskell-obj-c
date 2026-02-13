{-# LANGUAGE DataKinds #-}
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
  , initForReadingFromData_device_errorSelector
  , initForReadingFromData_errorSelector
  , initForReadingWithDataSelector
  , initForReadingWithData_deviceSelector
  , initSelector
  , initWithDeviceSelector
  , mpsMTLDeviceSelector
  , unarchiveObjectWithDataSelector
  , unarchiveObjectWithData_deviceSelector
  , unarchiveObjectWithFileSelector
  , unarchiveObjectWithFile_deviceSelector
  , unarchiveTopLevelObjectWithData_device_errorSelector
  , unarchiveTopLevelObjectWithData_errorSelector
  , unarchivedObjectOfClass_fromData_device_errorSelector
  , unarchivedObjectOfClass_fromData_errorSelector
  , unarchivedObjectOfClasses_fromData_device_errorSelector
  , unarchivedObjectOfClasses_fromData_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ unarchivedObjectOfClasses:fromData:device:error:@
unarchivedObjectOfClasses_fromData_device_error :: (IsNSSet classes, IsNSData data_, IsNSError error_) => classes -> data_ -> RawId -> error_ -> IO RawId
unarchivedObjectOfClasses_fromData_device_error classes data_ device error_ =
  do
    cls' <- getRequiredClass "MPSKeyedUnarchiver"
    sendClassMessage cls' unarchivedObjectOfClasses_fromData_device_errorSelector (toNSSet classes) (toNSData data_) device (toNSError error_)

-- | @+ unarchivedObjectOfClass:fromData:device:error:@
unarchivedObjectOfClass_fromData_device_error :: (IsNSData data_, IsNSError error_) => Class -> data_ -> RawId -> error_ -> IO RawId
unarchivedObjectOfClass_fromData_device_error cls data_ device error_ =
  do
    cls' <- getRequiredClass "MPSKeyedUnarchiver"
    sendClassMessage cls' unarchivedObjectOfClass_fromData_device_errorSelector cls (toNSData data_) device (toNSError error_)

-- | @- initForReadingFromData:device:error:@
initForReadingFromData_device_error :: (IsMPSKeyedUnarchiver mpsKeyedUnarchiver, IsNSData data_, IsNSError error_) => mpsKeyedUnarchiver -> data_ -> RawId -> error_ -> IO (Id MPSKeyedUnarchiver)
initForReadingFromData_device_error mpsKeyedUnarchiver data_ device error_ =
  sendOwnedMessage mpsKeyedUnarchiver initForReadingFromData_device_errorSelector (toNSData data_) device (toNSError error_)

-- | Reports which device to use for unarchiving MPSKernels
--
-- ObjC selector: @- mpsMTLDevice@
mpsMTLDevice :: IsMPSKeyedUnarchiver mpsKeyedUnarchiver => mpsKeyedUnarchiver -> IO RawId
mpsMTLDevice mpsKeyedUnarchiver =
  sendMessage mpsKeyedUnarchiver mpsMTLDeviceSelector

-- | @+ unarchivedObjectOfClasses:fromData:error:@
unarchivedObjectOfClasses_fromData_error :: (IsNSSet classes, IsNSData data_, IsNSError error_) => classes -> data_ -> error_ -> IO RawId
unarchivedObjectOfClasses_fromData_error classes data_ error_ =
  do
    cls' <- getRequiredClass "MPSKeyedUnarchiver"
    sendClassMessage cls' unarchivedObjectOfClasses_fromData_errorSelector (toNSSet classes) (toNSData data_) (toNSError error_)

-- | @+ unarchivedObjectOfClass:fromData:error:@
unarchivedObjectOfClass_fromData_error :: (IsNSData data_, IsNSError error_) => Class -> data_ -> error_ -> IO RawId
unarchivedObjectOfClass_fromData_error cls data_ error_ =
  do
    cls' <- getRequiredClass "MPSKeyedUnarchiver"
    sendClassMessage cls' unarchivedObjectOfClass_fromData_errorSelector cls (toNSData data_) (toNSError error_)

-- | @- init@
init_ :: IsMPSKeyedUnarchiver mpsKeyedUnarchiver => mpsKeyedUnarchiver -> IO (Id MPSKeyedUnarchiver)
init_ mpsKeyedUnarchiver =
  sendOwnedMessage mpsKeyedUnarchiver initSelector

-- | @- initForReadingFromData:error:@
initForReadingFromData_error :: (IsMPSKeyedUnarchiver mpsKeyedUnarchiver, IsNSData data_, IsNSError error_) => mpsKeyedUnarchiver -> data_ -> error_ -> IO (Id MPSKeyedUnarchiver)
initForReadingFromData_error mpsKeyedUnarchiver data_ error_ =
  sendOwnedMessage mpsKeyedUnarchiver initForReadingFromData_errorSelector (toNSData data_) (toNSError error_)

-- | @+ unarchiveObjectWithData:@
unarchiveObjectWithData :: IsNSData data_ => data_ -> IO RawId
unarchiveObjectWithData data_ =
  do
    cls' <- getRequiredClass "MPSKeyedUnarchiver"
    sendClassMessage cls' unarchiveObjectWithDataSelector (toNSData data_)

-- | @+ unarchiveObjectWithData:device:@
unarchiveObjectWithData_device :: IsNSData data_ => data_ -> RawId -> IO RawId
unarchiveObjectWithData_device data_ device =
  do
    cls' <- getRequiredClass "MPSKeyedUnarchiver"
    sendClassMessage cls' unarchiveObjectWithData_deviceSelector (toNSData data_) device

-- | @+ unarchiveTopLevelObjectWithData:error:@
unarchiveTopLevelObjectWithData_error :: (IsNSData data_, IsNSError error_) => data_ -> error_ -> IO RawId
unarchiveTopLevelObjectWithData_error data_ error_ =
  do
    cls' <- getRequiredClass "MPSKeyedUnarchiver"
    sendClassMessage cls' unarchiveTopLevelObjectWithData_errorSelector (toNSData data_) (toNSError error_)

-- | @+ unarchiveTopLevelObjectWithData:device:error:@
unarchiveTopLevelObjectWithData_device_error :: (IsNSData data_, IsNSError error_) => data_ -> RawId -> error_ -> IO RawId
unarchiveTopLevelObjectWithData_device_error data_ device error_ =
  do
    cls' <- getRequiredClass "MPSKeyedUnarchiver"
    sendClassMessage cls' unarchiveTopLevelObjectWithData_device_errorSelector (toNSData data_) device (toNSError error_)

-- | @+ unarchiveObjectWithFile:@
unarchiveObjectWithFile :: IsNSString path => path -> IO RawId
unarchiveObjectWithFile path =
  do
    cls' <- getRequiredClass "MPSKeyedUnarchiver"
    sendClassMessage cls' unarchiveObjectWithFileSelector (toNSString path)

-- | @- initForReadingWithData:@
initForReadingWithData :: (IsMPSKeyedUnarchiver mpsKeyedUnarchiver, IsNSData data_) => mpsKeyedUnarchiver -> data_ -> IO (Id MPSKeyedUnarchiver)
initForReadingWithData mpsKeyedUnarchiver data_ =
  sendOwnedMessage mpsKeyedUnarchiver initForReadingWithDataSelector (toNSData data_)

-- | @+ unarchiveObjectWithFile:device:@
unarchiveObjectWithFile_device :: IsNSString path => path -> RawId -> IO RawId
unarchiveObjectWithFile_device path device =
  do
    cls' <- getRequiredClass "MPSKeyedUnarchiver"
    sendClassMessage cls' unarchiveObjectWithFile_deviceSelector (toNSString path) device

-- | @- initWithDevice:@
initWithDevice :: IsMPSKeyedUnarchiver mpsKeyedUnarchiver => mpsKeyedUnarchiver -> RawId -> IO (Id MPSKeyedUnarchiver)
initWithDevice mpsKeyedUnarchiver device =
  sendOwnedMessage mpsKeyedUnarchiver initWithDeviceSelector device

-- | @- initForReadingWithData:device:@
initForReadingWithData_device :: (IsMPSKeyedUnarchiver mpsKeyedUnarchiver, IsNSData data_) => mpsKeyedUnarchiver -> data_ -> RawId -> IO (Id MPSKeyedUnarchiver)
initForReadingWithData_device mpsKeyedUnarchiver data_ device =
  sendOwnedMessage mpsKeyedUnarchiver initForReadingWithData_deviceSelector (toNSData data_) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unarchivedObjectOfClasses:fromData:device:error:@
unarchivedObjectOfClasses_fromData_device_errorSelector :: Selector '[Id NSSet, Id NSData, RawId, Id NSError] RawId
unarchivedObjectOfClasses_fromData_device_errorSelector = mkSelector "unarchivedObjectOfClasses:fromData:device:error:"

-- | @Selector@ for @unarchivedObjectOfClass:fromData:device:error:@
unarchivedObjectOfClass_fromData_device_errorSelector :: Selector '[Class, Id NSData, RawId, Id NSError] RawId
unarchivedObjectOfClass_fromData_device_errorSelector = mkSelector "unarchivedObjectOfClass:fromData:device:error:"

-- | @Selector@ for @initForReadingFromData:device:error:@
initForReadingFromData_device_errorSelector :: Selector '[Id NSData, RawId, Id NSError] (Id MPSKeyedUnarchiver)
initForReadingFromData_device_errorSelector = mkSelector "initForReadingFromData:device:error:"

-- | @Selector@ for @mpsMTLDevice@
mpsMTLDeviceSelector :: Selector '[] RawId
mpsMTLDeviceSelector = mkSelector "mpsMTLDevice"

-- | @Selector@ for @unarchivedObjectOfClasses:fromData:error:@
unarchivedObjectOfClasses_fromData_errorSelector :: Selector '[Id NSSet, Id NSData, Id NSError] RawId
unarchivedObjectOfClasses_fromData_errorSelector = mkSelector "unarchivedObjectOfClasses:fromData:error:"

-- | @Selector@ for @unarchivedObjectOfClass:fromData:error:@
unarchivedObjectOfClass_fromData_errorSelector :: Selector '[Class, Id NSData, Id NSError] RawId
unarchivedObjectOfClass_fromData_errorSelector = mkSelector "unarchivedObjectOfClass:fromData:error:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPSKeyedUnarchiver)
initSelector = mkSelector "init"

-- | @Selector@ for @initForReadingFromData:error:@
initForReadingFromData_errorSelector :: Selector '[Id NSData, Id NSError] (Id MPSKeyedUnarchiver)
initForReadingFromData_errorSelector = mkSelector "initForReadingFromData:error:"

-- | @Selector@ for @unarchiveObjectWithData:@
unarchiveObjectWithDataSelector :: Selector '[Id NSData] RawId
unarchiveObjectWithDataSelector = mkSelector "unarchiveObjectWithData:"

-- | @Selector@ for @unarchiveObjectWithData:device:@
unarchiveObjectWithData_deviceSelector :: Selector '[Id NSData, RawId] RawId
unarchiveObjectWithData_deviceSelector = mkSelector "unarchiveObjectWithData:device:"

-- | @Selector@ for @unarchiveTopLevelObjectWithData:error:@
unarchiveTopLevelObjectWithData_errorSelector :: Selector '[Id NSData, Id NSError] RawId
unarchiveTopLevelObjectWithData_errorSelector = mkSelector "unarchiveTopLevelObjectWithData:error:"

-- | @Selector@ for @unarchiveTopLevelObjectWithData:device:error:@
unarchiveTopLevelObjectWithData_device_errorSelector :: Selector '[Id NSData, RawId, Id NSError] RawId
unarchiveTopLevelObjectWithData_device_errorSelector = mkSelector "unarchiveTopLevelObjectWithData:device:error:"

-- | @Selector@ for @unarchiveObjectWithFile:@
unarchiveObjectWithFileSelector :: Selector '[Id NSString] RawId
unarchiveObjectWithFileSelector = mkSelector "unarchiveObjectWithFile:"

-- | @Selector@ for @initForReadingWithData:@
initForReadingWithDataSelector :: Selector '[Id NSData] (Id MPSKeyedUnarchiver)
initForReadingWithDataSelector = mkSelector "initForReadingWithData:"

-- | @Selector@ for @unarchiveObjectWithFile:device:@
unarchiveObjectWithFile_deviceSelector :: Selector '[Id NSString, RawId] RawId
unarchiveObjectWithFile_deviceSelector = mkSelector "unarchiveObjectWithFile:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSKeyedUnarchiver)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initForReadingWithData:device:@
initForReadingWithData_deviceSelector :: Selector '[Id NSData, RawId] (Id MPSKeyedUnarchiver)
initForReadingWithData_deviceSelector = mkSelector "initForReadingWithData:device:"

