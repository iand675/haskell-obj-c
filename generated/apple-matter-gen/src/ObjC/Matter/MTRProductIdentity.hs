{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A representation of a (vendor, product) pair that identifies a specific product.
--
-- Generated bindings for @MTRProductIdentity@.
module ObjC.Matter.MTRProductIdentity
  ( MTRProductIdentity
  , IsMTRProductIdentity(..)
  , init_
  , new
  , initWithVendorID_productID
  , vendorID
  , productID
  , initSelector
  , initWithVendorID_productIDSelector
  , newSelector
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

-- | @- init@
init_ :: IsMTRProductIdentity mtrProductIdentity => mtrProductIdentity -> IO (Id MTRProductIdentity)
init_ mtrProductIdentity =
  sendOwnedMessage mtrProductIdentity initSelector

-- | @+ new@
new :: IO (Id MTRProductIdentity)
new  =
  do
    cls' <- getRequiredClass "MTRProductIdentity"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithVendorID:productID:@
initWithVendorID_productID :: (IsMTRProductIdentity mtrProductIdentity, IsNSNumber vendorID, IsNSNumber productID) => mtrProductIdentity -> vendorID -> productID -> IO (Id MTRProductIdentity)
initWithVendorID_productID mtrProductIdentity vendorID productID =
  sendOwnedMessage mtrProductIdentity initWithVendorID_productIDSelector (toNSNumber vendorID) (toNSNumber productID)

-- | @- vendorID@
vendorID :: IsMTRProductIdentity mtrProductIdentity => mtrProductIdentity -> IO (Id NSNumber)
vendorID mtrProductIdentity =
  sendMessage mtrProductIdentity vendorIDSelector

-- | @- productID@
productID :: IsMTRProductIdentity mtrProductIdentity => mtrProductIdentity -> IO (Id NSNumber)
productID mtrProductIdentity =
  sendMessage mtrProductIdentity productIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRProductIdentity)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRProductIdentity)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithVendorID:productID:@
initWithVendorID_productIDSelector :: Selector '[Id NSNumber, Id NSNumber] (Id MTRProductIdentity)
initWithVendorID_productIDSelector = mkSelector "initWithVendorID:productID:"

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector '[] (Id NSNumber)
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @productID@
productIDSelector :: Selector '[] (Id NSNumber)
productIDSelector = mkSelector "productID"

