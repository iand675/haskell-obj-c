{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKAddress@.
module ObjC.MapKit.MKAddress
  ( MKAddress
  , IsMKAddress(..)
  , initWithFullAddress_shortAddress
  , init_
  , new
  , fullAddress
  , shortAddress
  , fullAddressSelector
  , initSelector
  , initWithFullAddress_shortAddressSelector
  , newSelector
  , shortAddressSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithFullAddress:shortAddress:@
initWithFullAddress_shortAddress :: (IsMKAddress mkAddress, IsNSString fullAddress, IsNSString shortAddress) => mkAddress -> fullAddress -> shortAddress -> IO (Id MKAddress)
initWithFullAddress_shortAddress mkAddress fullAddress shortAddress =
  sendOwnedMessage mkAddress initWithFullAddress_shortAddressSelector (toNSString fullAddress) (toNSString shortAddress)

-- | @- init@
init_ :: IsMKAddress mkAddress => mkAddress -> IO (Id MKAddress)
init_ mkAddress =
  sendOwnedMessage mkAddress initSelector

-- | @+ new@
new :: IO (Id MKAddress)
new  =
  do
    cls' <- getRequiredClass "MKAddress"
    sendOwnedClassMessage cls' newSelector

-- | @- fullAddress@
fullAddress :: IsMKAddress mkAddress => mkAddress -> IO (Id NSString)
fullAddress mkAddress =
  sendMessage mkAddress fullAddressSelector

-- | @- shortAddress@
shortAddress :: IsMKAddress mkAddress => mkAddress -> IO (Id NSString)
shortAddress mkAddress =
  sendMessage mkAddress shortAddressSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFullAddress:shortAddress:@
initWithFullAddress_shortAddressSelector :: Selector '[Id NSString, Id NSString] (Id MKAddress)
initWithFullAddress_shortAddressSelector = mkSelector "initWithFullAddress:shortAddress:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MKAddress)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MKAddress)
newSelector = mkSelector "new"

-- | @Selector@ for @fullAddress@
fullAddressSelector :: Selector '[] (Id NSString)
fullAddressSelector = mkSelector "fullAddress"

-- | @Selector@ for @shortAddress@
shortAddressSelector :: Selector '[] (Id NSString)
shortAddressSelector = mkSelector "shortAddress"

