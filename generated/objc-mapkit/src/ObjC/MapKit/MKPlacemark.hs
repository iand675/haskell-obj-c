{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKPlacemark@.
module ObjC.MapKit.MKPlacemark
  ( MKPlacemark
  , IsMKPlacemark(..)
  , initWithCoordinate
  , initWithCoordinate_addressDictionary
  , initWithCoordinate_postalAddress
  , countryCode
  , initWithCoordinateSelector
  , initWithCoordinate_addressDictionarySelector
  , initWithCoordinate_postalAddressSelector
  , countryCodeSelector


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

import ObjC.MapKit.Internal.Classes
import ObjC.CoreLocation.Internal.Structs
import ObjC.Contacts.Internal.Classes
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCoordinate:@
initWithCoordinate :: IsMKPlacemark mkPlacemark => mkPlacemark -> CLLocationCoordinate2D -> IO (Id MKPlacemark)
initWithCoordinate mkPlacemark  coordinate =
  sendMsg mkPlacemark (mkSelector "initWithCoordinate:") (retPtr retVoid) [argCLLocationCoordinate2D coordinate] >>= ownedObject . castPtr

-- | @- initWithCoordinate:addressDictionary:@
initWithCoordinate_addressDictionary :: (IsMKPlacemark mkPlacemark, IsNSDictionary addressDictionary) => mkPlacemark -> CLLocationCoordinate2D -> addressDictionary -> IO (Id MKPlacemark)
initWithCoordinate_addressDictionary mkPlacemark  coordinate addressDictionary =
withObjCPtr addressDictionary $ \raw_addressDictionary ->
    sendMsg mkPlacemark (mkSelector "initWithCoordinate:addressDictionary:") (retPtr retVoid) [argCLLocationCoordinate2D coordinate, argPtr (castPtr raw_addressDictionary :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoordinate:postalAddress:@
initWithCoordinate_postalAddress :: (IsMKPlacemark mkPlacemark, IsCNPostalAddress postalAddress) => mkPlacemark -> CLLocationCoordinate2D -> postalAddress -> IO (Id MKPlacemark)
initWithCoordinate_postalAddress mkPlacemark  coordinate postalAddress =
withObjCPtr postalAddress $ \raw_postalAddress ->
    sendMsg mkPlacemark (mkSelector "initWithCoordinate:postalAddress:") (retPtr retVoid) [argCLLocationCoordinate2D coordinate, argPtr (castPtr raw_postalAddress :: Ptr ())] >>= ownedObject . castPtr

-- | @- countryCode@
countryCode :: IsMKPlacemark mkPlacemark => mkPlacemark -> IO (Id NSString)
countryCode mkPlacemark  =
  sendMsg mkPlacemark (mkSelector "countryCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoordinate:@
initWithCoordinateSelector :: Selector
initWithCoordinateSelector = mkSelector "initWithCoordinate:"

-- | @Selector@ for @initWithCoordinate:addressDictionary:@
initWithCoordinate_addressDictionarySelector :: Selector
initWithCoordinate_addressDictionarySelector = mkSelector "initWithCoordinate:addressDictionary:"

-- | @Selector@ for @initWithCoordinate:postalAddress:@
initWithCoordinate_postalAddressSelector :: Selector
initWithCoordinate_postalAddressSelector = mkSelector "initWithCoordinate:postalAddress:"

-- | @Selector@ for @countryCode@
countryCodeSelector :: Selector
countryCodeSelector = mkSelector "countryCode"

