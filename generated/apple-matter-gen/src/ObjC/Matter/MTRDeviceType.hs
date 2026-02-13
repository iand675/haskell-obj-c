{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Meta-data about a device type defined in the Matter specification.
--
-- Generated bindings for @MTRDeviceType@.
module ObjC.Matter.MTRDeviceType
  ( MTRDeviceType
  , IsMTRDeviceType(..)
  , deviceTypeForID
  , init_
  , new
  , id_
  , name
  , isUtility
  , deviceTypeForIDSelector
  , idSelector
  , initSelector
  , isUtilitySelector
  , nameSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns an MTRDeviceType for the given ID, if the ID is known.  Returns nil for unknown IDs.
--
-- ObjC selector: @+ deviceTypeForID:@
deviceTypeForID :: IsNSNumber deviceTypeID => deviceTypeID -> IO (Id MTRDeviceType)
deviceTypeForID deviceTypeID =
  do
    cls' <- getRequiredClass "MTRDeviceType"
    sendClassMessage cls' deviceTypeForIDSelector (toNSNumber deviceTypeID)

-- | @- init@
init_ :: IsMTRDeviceType mtrDeviceType => mtrDeviceType -> IO (Id MTRDeviceType)
init_ mtrDeviceType =
  sendOwnedMessage mtrDeviceType initSelector

-- | @+ new@
new :: IO (Id MTRDeviceType)
new  =
  do
    cls' <- getRequiredClass "MTRDeviceType"
    sendOwnedClassMessage cls' newSelector

-- | The identifier of the device type (32-bit unsigned integer).
--
-- ObjC selector: @- id@
id_ :: IsMTRDeviceType mtrDeviceType => mtrDeviceType -> IO (Id NSNumber)
id_ mtrDeviceType =
  sendMessage mtrDeviceType idSelector

-- | Returns the name of the device type.
--
-- ObjC selector: @- name@
name :: IsMTRDeviceType mtrDeviceType => mtrDeviceType -> IO (Id NSString)
name mtrDeviceType =
  sendMessage mtrDeviceType nameSelector

-- | Returns whether this is a utility device type.
--
-- ObjC selector: @- isUtility@
isUtility :: IsMTRDeviceType mtrDeviceType => mtrDeviceType -> IO Bool
isUtility mtrDeviceType =
  sendMessage mtrDeviceType isUtilitySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deviceTypeForID:@
deviceTypeForIDSelector :: Selector '[Id NSNumber] (Id MTRDeviceType)
deviceTypeForIDSelector = mkSelector "deviceTypeForID:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRDeviceType)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRDeviceType)
newSelector = mkSelector "new"

-- | @Selector@ for @id@
idSelector :: Selector '[] (Id NSNumber)
idSelector = mkSelector "id"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @isUtility@
isUtilitySelector :: Selector '[] Bool
isUtilitySelector = mkSelector "isUtility"

