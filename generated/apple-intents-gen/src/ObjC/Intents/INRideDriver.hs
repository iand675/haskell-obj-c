{-# LANGUAGE DataKinds #-}
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
  , initWithHandle_displayName_image_rating_phoneNumberSelector
  , initWithHandle_nameComponents_image_rating_phoneNumberSelector
  , initWithPersonHandle_nameComponents_displayName_image_rating_phoneNumberSelector
  , initWithPhoneNumber_nameComponents_displayName_image_ratingSelector
  , phoneNumberSelector
  , ratingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPhoneNumber:nameComponents:displayName:image:rating:@
initWithPhoneNumber_nameComponents_displayName_image_rating :: (IsINRideDriver inRideDriver, IsNSString phoneNumber, IsNSPersonNameComponents nameComponents, IsNSString displayName, IsINImage image, IsNSString rating) => inRideDriver -> phoneNumber -> nameComponents -> displayName -> image -> rating -> IO (Id INRideDriver)
initWithPhoneNumber_nameComponents_displayName_image_rating inRideDriver phoneNumber nameComponents displayName image rating =
  sendOwnedMessage inRideDriver initWithPhoneNumber_nameComponents_displayName_image_ratingSelector (toNSString phoneNumber) (toNSPersonNameComponents nameComponents) (toNSString displayName) (toINImage image) (toNSString rating)

-- | @- initWithPersonHandle:nameComponents:displayName:image:rating:phoneNumber:@
initWithPersonHandle_nameComponents_displayName_image_rating_phoneNumber :: (IsINRideDriver inRideDriver, IsINPersonHandle personHandle, IsNSPersonNameComponents nameComponents, IsNSString displayName, IsINImage image, IsNSString rating, IsNSString phoneNumber) => inRideDriver -> personHandle -> nameComponents -> displayName -> image -> rating -> phoneNumber -> IO (Id INRideDriver)
initWithPersonHandle_nameComponents_displayName_image_rating_phoneNumber inRideDriver personHandle nameComponents displayName image rating phoneNumber =
  sendOwnedMessage inRideDriver initWithPersonHandle_nameComponents_displayName_image_rating_phoneNumberSelector (toINPersonHandle personHandle) (toNSPersonNameComponents nameComponents) (toNSString displayName) (toINImage image) (toNSString rating) (toNSString phoneNumber)

-- | @- initWithHandle:displayName:image:rating:phoneNumber:@
initWithHandle_displayName_image_rating_phoneNumber :: (IsINRideDriver inRideDriver, IsNSString handle, IsNSString displayName, IsINImage image, IsNSString rating, IsNSString phoneNumber) => inRideDriver -> handle -> displayName -> image -> rating -> phoneNumber -> IO (Id INRideDriver)
initWithHandle_displayName_image_rating_phoneNumber inRideDriver handle displayName image rating phoneNumber =
  sendOwnedMessage inRideDriver initWithHandle_displayName_image_rating_phoneNumberSelector (toNSString handle) (toNSString displayName) (toINImage image) (toNSString rating) (toNSString phoneNumber)

-- | @- initWithHandle:nameComponents:image:rating:phoneNumber:@
initWithHandle_nameComponents_image_rating_phoneNumber :: (IsINRideDriver inRideDriver, IsNSString handle, IsNSPersonNameComponents nameComponents, IsINImage image, IsNSString rating, IsNSString phoneNumber) => inRideDriver -> handle -> nameComponents -> image -> rating -> phoneNumber -> IO (Id INRideDriver)
initWithHandle_nameComponents_image_rating_phoneNumber inRideDriver handle nameComponents image rating phoneNumber =
  sendOwnedMessage inRideDriver initWithHandle_nameComponents_image_rating_phoneNumberSelector (toNSString handle) (toNSPersonNameComponents nameComponents) (toINImage image) (toNSString rating) (toNSString phoneNumber)

-- | @- rating@
rating :: IsINRideDriver inRideDriver => inRideDriver -> IO (Id NSString)
rating inRideDriver =
  sendMessage inRideDriver ratingSelector

-- | @- phoneNumber@
phoneNumber :: IsINRideDriver inRideDriver => inRideDriver -> IO (Id NSString)
phoneNumber inRideDriver =
  sendMessage inRideDriver phoneNumberSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPhoneNumber:nameComponents:displayName:image:rating:@
initWithPhoneNumber_nameComponents_displayName_image_ratingSelector :: Selector '[Id NSString, Id NSPersonNameComponents, Id NSString, Id INImage, Id NSString] (Id INRideDriver)
initWithPhoneNumber_nameComponents_displayName_image_ratingSelector = mkSelector "initWithPhoneNumber:nameComponents:displayName:image:rating:"

-- | @Selector@ for @initWithPersonHandle:nameComponents:displayName:image:rating:phoneNumber:@
initWithPersonHandle_nameComponents_displayName_image_rating_phoneNumberSelector :: Selector '[Id INPersonHandle, Id NSPersonNameComponents, Id NSString, Id INImage, Id NSString, Id NSString] (Id INRideDriver)
initWithPersonHandle_nameComponents_displayName_image_rating_phoneNumberSelector = mkSelector "initWithPersonHandle:nameComponents:displayName:image:rating:phoneNumber:"

-- | @Selector@ for @initWithHandle:displayName:image:rating:phoneNumber:@
initWithHandle_displayName_image_rating_phoneNumberSelector :: Selector '[Id NSString, Id NSString, Id INImage, Id NSString, Id NSString] (Id INRideDriver)
initWithHandle_displayName_image_rating_phoneNumberSelector = mkSelector "initWithHandle:displayName:image:rating:phoneNumber:"

-- | @Selector@ for @initWithHandle:nameComponents:image:rating:phoneNumber:@
initWithHandle_nameComponents_image_rating_phoneNumberSelector :: Selector '[Id NSString, Id NSPersonNameComponents, Id INImage, Id NSString, Id NSString] (Id INRideDriver)
initWithHandle_nameComponents_image_rating_phoneNumberSelector = mkSelector "initWithHandle:nameComponents:image:rating:phoneNumber:"

-- | @Selector@ for @rating@
ratingSelector :: Selector '[] (Id NSString)
ratingSelector = mkSelector "rating"

-- | @Selector@ for @phoneNumber@
phoneNumberSelector :: Selector '[] (Id NSString)
phoneNumberSelector = mkSelector "phoneNumber"

