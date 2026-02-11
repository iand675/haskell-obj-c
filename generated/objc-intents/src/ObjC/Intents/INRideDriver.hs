{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRideDriver@.
module ObjC.Intents.INRideDriver
  ( INRideDriver
  , IsINRideDriver(..)
  , initWithPhoneNumber_nameComponents_displayName_image_rating
  , initWithPersonHandle_nameComponents_displayName_image_rating_phoneNumber
  , initWithHandle_displayName_image_rating_phoneNumber
  , initWithHandle_nameComponents_image_rating_phoneNumber
  , rating
  , phoneNumber
  , initWithPhoneNumber_nameComponents_displayName_image_ratingSelector
  , initWithPersonHandle_nameComponents_displayName_image_rating_phoneNumberSelector
  , initWithHandle_displayName_image_rating_phoneNumberSelector
  , initWithHandle_nameComponents_image_rating_phoneNumberSelector
  , ratingSelector
  , phoneNumberSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPhoneNumber:nameComponents:displayName:image:rating:@
initWithPhoneNumber_nameComponents_displayName_image_rating :: (IsINRideDriver inRideDriver, IsNSString phoneNumber, IsNSPersonNameComponents nameComponents, IsNSString displayName, IsINImage image, IsNSString rating) => inRideDriver -> phoneNumber -> nameComponents -> displayName -> image -> rating -> IO (Id INRideDriver)
initWithPhoneNumber_nameComponents_displayName_image_rating inRideDriver  phoneNumber nameComponents displayName image rating =
withObjCPtr phoneNumber $ \raw_phoneNumber ->
  withObjCPtr nameComponents $ \raw_nameComponents ->
    withObjCPtr displayName $ \raw_displayName ->
      withObjCPtr image $ \raw_image ->
        withObjCPtr rating $ \raw_rating ->
            sendMsg inRideDriver (mkSelector "initWithPhoneNumber:nameComponents:displayName:image:rating:") (retPtr retVoid) [argPtr (castPtr raw_phoneNumber :: Ptr ()), argPtr (castPtr raw_nameComponents :: Ptr ()), argPtr (castPtr raw_displayName :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_rating :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithPersonHandle:nameComponents:displayName:image:rating:phoneNumber:@
initWithPersonHandle_nameComponents_displayName_image_rating_phoneNumber :: (IsINRideDriver inRideDriver, IsINPersonHandle personHandle, IsNSPersonNameComponents nameComponents, IsNSString displayName, IsINImage image, IsNSString rating, IsNSString phoneNumber) => inRideDriver -> personHandle -> nameComponents -> displayName -> image -> rating -> phoneNumber -> IO (Id INRideDriver)
initWithPersonHandle_nameComponents_displayName_image_rating_phoneNumber inRideDriver  personHandle nameComponents displayName image rating phoneNumber =
withObjCPtr personHandle $ \raw_personHandle ->
  withObjCPtr nameComponents $ \raw_nameComponents ->
    withObjCPtr displayName $ \raw_displayName ->
      withObjCPtr image $ \raw_image ->
        withObjCPtr rating $ \raw_rating ->
          withObjCPtr phoneNumber $ \raw_phoneNumber ->
              sendMsg inRideDriver (mkSelector "initWithPersonHandle:nameComponents:displayName:image:rating:phoneNumber:") (retPtr retVoid) [argPtr (castPtr raw_personHandle :: Ptr ()), argPtr (castPtr raw_nameComponents :: Ptr ()), argPtr (castPtr raw_displayName :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_rating :: Ptr ()), argPtr (castPtr raw_phoneNumber :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithHandle:displayName:image:rating:phoneNumber:@
initWithHandle_displayName_image_rating_phoneNumber :: (IsINRideDriver inRideDriver, IsNSString handle, IsNSString displayName, IsINImage image, IsNSString rating, IsNSString phoneNumber) => inRideDriver -> handle -> displayName -> image -> rating -> phoneNumber -> IO (Id INRideDriver)
initWithHandle_displayName_image_rating_phoneNumber inRideDriver  handle displayName image rating phoneNumber =
withObjCPtr handle $ \raw_handle ->
  withObjCPtr displayName $ \raw_displayName ->
    withObjCPtr image $ \raw_image ->
      withObjCPtr rating $ \raw_rating ->
        withObjCPtr phoneNumber $ \raw_phoneNumber ->
            sendMsg inRideDriver (mkSelector "initWithHandle:displayName:image:rating:phoneNumber:") (retPtr retVoid) [argPtr (castPtr raw_handle :: Ptr ()), argPtr (castPtr raw_displayName :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_rating :: Ptr ()), argPtr (castPtr raw_phoneNumber :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithHandle:nameComponents:image:rating:phoneNumber:@
initWithHandle_nameComponents_image_rating_phoneNumber :: (IsINRideDriver inRideDriver, IsNSString handle, IsNSPersonNameComponents nameComponents, IsINImage image, IsNSString rating, IsNSString phoneNumber) => inRideDriver -> handle -> nameComponents -> image -> rating -> phoneNumber -> IO (Id INRideDriver)
initWithHandle_nameComponents_image_rating_phoneNumber inRideDriver  handle nameComponents image rating phoneNumber =
withObjCPtr handle $ \raw_handle ->
  withObjCPtr nameComponents $ \raw_nameComponents ->
    withObjCPtr image $ \raw_image ->
      withObjCPtr rating $ \raw_rating ->
        withObjCPtr phoneNumber $ \raw_phoneNumber ->
            sendMsg inRideDriver (mkSelector "initWithHandle:nameComponents:image:rating:phoneNumber:") (retPtr retVoid) [argPtr (castPtr raw_handle :: Ptr ()), argPtr (castPtr raw_nameComponents :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_rating :: Ptr ()), argPtr (castPtr raw_phoneNumber :: Ptr ())] >>= ownedObject . castPtr

-- | @- rating@
rating :: IsINRideDriver inRideDriver => inRideDriver -> IO (Id NSString)
rating inRideDriver  =
  sendMsg inRideDriver (mkSelector "rating") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- phoneNumber@
phoneNumber :: IsINRideDriver inRideDriver => inRideDriver -> IO (Id NSString)
phoneNumber inRideDriver  =
  sendMsg inRideDriver (mkSelector "phoneNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPhoneNumber:nameComponents:displayName:image:rating:@
initWithPhoneNumber_nameComponents_displayName_image_ratingSelector :: Selector
initWithPhoneNumber_nameComponents_displayName_image_ratingSelector = mkSelector "initWithPhoneNumber:nameComponents:displayName:image:rating:"

-- | @Selector@ for @initWithPersonHandle:nameComponents:displayName:image:rating:phoneNumber:@
initWithPersonHandle_nameComponents_displayName_image_rating_phoneNumberSelector :: Selector
initWithPersonHandle_nameComponents_displayName_image_rating_phoneNumberSelector = mkSelector "initWithPersonHandle:nameComponents:displayName:image:rating:phoneNumber:"

-- | @Selector@ for @initWithHandle:displayName:image:rating:phoneNumber:@
initWithHandle_displayName_image_rating_phoneNumberSelector :: Selector
initWithHandle_displayName_image_rating_phoneNumberSelector = mkSelector "initWithHandle:displayName:image:rating:phoneNumber:"

-- | @Selector@ for @initWithHandle:nameComponents:image:rating:phoneNumber:@
initWithHandle_nameComponents_image_rating_phoneNumberSelector :: Selector
initWithHandle_nameComponents_image_rating_phoneNumberSelector = mkSelector "initWithHandle:nameComponents:image:rating:phoneNumber:"

-- | @Selector@ for @rating@
ratingSelector :: Selector
ratingSelector = mkSelector "rating"

-- | @Selector@ for @phoneNumber@
phoneNumberSelector :: Selector
phoneNumberSelector = mkSelector "phoneNumber"

