{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommissionableBrowserResult@.
module ObjC.Matter.MTRCommissionableBrowserResult
  ( MTRCommissionableBrowserResult
  , IsMTRCommissionableBrowserResult(..)
  , instanceName
  , vendorID
  , productID
  , discriminator
  , commissioningMode
  , commissioningModeSelector
  , discriminatorSelector
  , instanceNameSelector
  , productIDSelector
  , vendorIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | For a node advertising over DNS-SD, the instance name is a dynamic, pseudo-randomly selected, 64-bit temporary unique identifier, expressed as a fixed-length sixteen-character hexadecimal string, encoded as ASCII text using capital letters.
--
-- For a node advertising over Bluetooth Low Energy, the instance name is always "BLE".
--
-- ObjC selector: @- instanceName@
instanceName :: IsMTRCommissionableBrowserResult mtrCommissionableBrowserResult => mtrCommissionableBrowserResult -> IO (Id NSString)
instanceName mtrCommissionableBrowserResult =
  sendMessage mtrCommissionableBrowserResult instanceNameSelector

-- | A 16-bit unsigned value identifying the device manufacturer.
--
-- ObjC selector: @- vendorID@
vendorID :: IsMTRCommissionableBrowserResult mtrCommissionableBrowserResult => mtrCommissionableBrowserResult -> IO (Id NSNumber)
vendorID mtrCommissionableBrowserResult =
  sendMessage mtrCommissionableBrowserResult vendorIDSelector

-- | A 16-bit unsigned value identifying the product.
--
-- ObjC selector: @- productID@
productID :: IsMTRCommissionableBrowserResult mtrCommissionableBrowserResult => mtrCommissionableBrowserResult -> IO (Id NSNumber)
productID mtrCommissionableBrowserResult =
  sendMessage mtrCommissionableBrowserResult productIDSelector

-- | A 12-bit value matching the field of the same name in MTRSetupPayload.
--
-- ObjC selector: @- discriminator@
discriminator :: IsMTRCommissionableBrowserResult mtrCommissionableBrowserResult => mtrCommissionableBrowserResult -> IO (Id NSNumber)
discriminator mtrCommissionableBrowserResult =
  sendMessage mtrCommissionableBrowserResult discriminatorSelector

-- | A boolean indicating whether the device has a commissioning window open.
--
-- ObjC selector: @- commissioningMode@
commissioningMode :: IsMTRCommissionableBrowserResult mtrCommissionableBrowserResult => mtrCommissionableBrowserResult -> IO Bool
commissioningMode mtrCommissionableBrowserResult =
  sendMessage mtrCommissionableBrowserResult commissioningModeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @instanceName@
instanceNameSelector :: Selector '[] (Id NSString)
instanceNameSelector = mkSelector "instanceName"

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector '[] (Id NSNumber)
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @productID@
productIDSelector :: Selector '[] (Id NSNumber)
productIDSelector = mkSelector "productID"

-- | @Selector@ for @discriminator@
discriminatorSelector :: Selector '[] (Id NSNumber)
discriminatorSelector = mkSelector "discriminator"

-- | @Selector@ for @commissioningMode@
commissioningModeSelector :: Selector '[] Bool
commissioningModeSelector = mkSelector "commissioningMode"

