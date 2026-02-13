{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLPlacemark@.
module ObjC.Intents.CLPlacemark
  ( CLPlacemark
  , IsCLPlacemark(..)
  , placemarkWithLocation_name_postalAddress
  , placemarkWithLocation_name_postalAddressSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Contacts.Internal.Classes
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ placemarkWithLocation:name:postalAddress:@
placemarkWithLocation_name_postalAddress :: (IsNSString name, IsCNPostalAddress postalAddress) => RawId -> name -> postalAddress -> IO (Id CLPlacemark)
placemarkWithLocation_name_postalAddress location name postalAddress =
  do
    cls' <- getRequiredClass "CLPlacemark"
    sendClassMessage cls' placemarkWithLocation_name_postalAddressSelector location (toNSString name) (toCNPostalAddress postalAddress)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @placemarkWithLocation:name:postalAddress:@
placemarkWithLocation_name_postalAddressSelector :: Selector '[RawId, Id NSString, Id CNPostalAddress] (Id CLPlacemark)
placemarkWithLocation_name_postalAddressSelector = mkSelector "placemarkWithLocation:name:postalAddress:"

