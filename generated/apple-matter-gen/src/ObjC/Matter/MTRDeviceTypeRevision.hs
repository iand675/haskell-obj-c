{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A representation of a "device type revision" in the sense used in the Matter specification.  This has an identifier and a version number.
--
-- Generated bindings for @MTRDeviceTypeRevision@.
module ObjC.Matter.MTRDeviceTypeRevision
  ( MTRDeviceTypeRevision
  , IsMTRDeviceTypeRevision(..)
  , init_
  , new
  , initWithDeviceTypeID_revision
  , initWithDeviceTypeStruct
  , deviceTypeID
  , deviceTypeRevision
  , typeInformation
  , deviceTypeIDSelector
  , deviceTypeRevisionSelector
  , initSelector
  , initWithDeviceTypeID_revisionSelector
  , initWithDeviceTypeStructSelector
  , newSelector
  , typeInformationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRDeviceTypeRevision mtrDeviceTypeRevision => mtrDeviceTypeRevision -> IO (Id MTRDeviceTypeRevision)
init_ mtrDeviceTypeRevision =
  sendOwnedMessage mtrDeviceTypeRevision initSelector

-- | @+ new@
new :: IO (Id MTRDeviceTypeRevision)
new  =
  do
    cls' <- getRequiredClass "MTRDeviceTypeRevision"
    sendOwnedClassMessage cls' newSelector

-- | The provided deviceTypeID must be in the range 0xVVVV0000-0xVVVVBFFF, where VVVV is the vendor identifier (0 for standard device types).
--
-- The provided deviceTypeRevision must be in the range 1-65535.
--
-- ObjC selector: @- initWithDeviceTypeID:revision:@
initWithDeviceTypeID_revision :: (IsMTRDeviceTypeRevision mtrDeviceTypeRevision, IsNSNumber deviceTypeID, IsNSNumber revision) => mtrDeviceTypeRevision -> deviceTypeID -> revision -> IO (Id MTRDeviceTypeRevision)
initWithDeviceTypeID_revision mtrDeviceTypeRevision deviceTypeID revision =
  sendOwnedMessage mtrDeviceTypeRevision initWithDeviceTypeID_revisionSelector (toNSNumber deviceTypeID) (toNSNumber revision)

-- | Initializes the receiver based on the values in the specified struct.
--
-- ObjC selector: @- initWithDeviceTypeStruct:@
initWithDeviceTypeStruct :: (IsMTRDeviceTypeRevision mtrDeviceTypeRevision, IsMTRDescriptorClusterDeviceTypeStruct deviceTypeStruct) => mtrDeviceTypeRevision -> deviceTypeStruct -> IO (Id MTRDeviceTypeRevision)
initWithDeviceTypeStruct mtrDeviceTypeRevision deviceTypeStruct =
  sendOwnedMessage mtrDeviceTypeRevision initWithDeviceTypeStructSelector (toMTRDescriptorClusterDeviceTypeStruct deviceTypeStruct)

-- | @- deviceTypeID@
deviceTypeID :: IsMTRDeviceTypeRevision mtrDeviceTypeRevision => mtrDeviceTypeRevision -> IO (Id NSNumber)
deviceTypeID mtrDeviceTypeRevision =
  sendMessage mtrDeviceTypeRevision deviceTypeIDSelector

-- | @- deviceTypeRevision@
deviceTypeRevision :: IsMTRDeviceTypeRevision mtrDeviceTypeRevision => mtrDeviceTypeRevision -> IO (Id NSNumber)
deviceTypeRevision mtrDeviceTypeRevision =
  sendMessage mtrDeviceTypeRevision deviceTypeRevisionSelector

-- | Returns the MTRDeviceType corresponding to deviceTypeID, or nil if deviceTypeID does not represent a known device type.
--
-- ObjC selector: @- typeInformation@
typeInformation :: IsMTRDeviceTypeRevision mtrDeviceTypeRevision => mtrDeviceTypeRevision -> IO (Id MTRDeviceType)
typeInformation mtrDeviceTypeRevision =
  sendMessage mtrDeviceTypeRevision typeInformationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRDeviceTypeRevision)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRDeviceTypeRevision)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDeviceTypeID:revision:@
initWithDeviceTypeID_revisionSelector :: Selector '[Id NSNumber, Id NSNumber] (Id MTRDeviceTypeRevision)
initWithDeviceTypeID_revisionSelector = mkSelector "initWithDeviceTypeID:revision:"

-- | @Selector@ for @initWithDeviceTypeStruct:@
initWithDeviceTypeStructSelector :: Selector '[Id MTRDescriptorClusterDeviceTypeStruct] (Id MTRDeviceTypeRevision)
initWithDeviceTypeStructSelector = mkSelector "initWithDeviceTypeStruct:"

-- | @Selector@ for @deviceTypeID@
deviceTypeIDSelector :: Selector '[] (Id NSNumber)
deviceTypeIDSelector = mkSelector "deviceTypeID"

-- | @Selector@ for @deviceTypeRevision@
deviceTypeRevisionSelector :: Selector '[] (Id NSNumber)
deviceTypeRevisionSelector = mkSelector "deviceTypeRevision"

-- | @Selector@ for @typeInformation@
typeInformationSelector :: Selector '[] (Id MTRDeviceType)
typeInformationSelector = mkSelector "typeInformation"

