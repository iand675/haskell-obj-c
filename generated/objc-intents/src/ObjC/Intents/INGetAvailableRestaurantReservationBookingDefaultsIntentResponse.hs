{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INGetAvailableRestaurantReservationBookingDefaultsIntentResponse@.
module ObjC.Intents.INGetAvailableRestaurantReservationBookingDefaultsIntentResponse
  ( INGetAvailableRestaurantReservationBookingDefaultsIntentResponse
  , IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse(..)
  , initWithDefaultPartySize_defaultBookingDate_code_userActivity
  , defaultPartySize
  , defaultBookingDate
  , maximumPartySize
  , setMaximumPartySize
  , minimumPartySize
  , setMinimumPartySize
  , providerImage
  , setProviderImage
  , code
  , initWithDefaultPartySize_defaultBookingDate_code_userActivitySelector
  , defaultPartySizeSelector
  , defaultBookingDateSelector
  , maximumPartySizeSelector
  , setMaximumPartySizeSelector
  , minimumPartySizeSelector
  , setMinimumPartySizeSelector
  , providerImageSelector
  , setProviderImageSelector
  , codeSelector

  -- * Enum types
  , INGetAvailableRestaurantReservationBookingDefaultsIntentResponseCode(INGetAvailableRestaurantReservationBookingDefaultsIntentResponseCode)
  , pattern INGetAvailableRestaurantReservationBookingDefaultsIntentResponseCodeSuccess
  , pattern INGetAvailableRestaurantReservationBookingDefaultsIntentResponseCodeFailure
  , pattern INGetAvailableRestaurantReservationBookingDefaultsIntentResponseCodeUnspecified

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
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithDefaultPartySize:defaultBookingDate:code:userActivity:@
initWithDefaultPartySize_defaultBookingDate_code_userActivity :: (IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse inGetAvailableRestaurantReservationBookingDefaultsIntentResponse, IsNSDate defaultBookingDate, IsNSUserActivity userActivity) => inGetAvailableRestaurantReservationBookingDefaultsIntentResponse -> CULong -> defaultBookingDate -> INGetAvailableRestaurantReservationBookingDefaultsIntentResponseCode -> userActivity -> IO (Id INGetAvailableRestaurantReservationBookingDefaultsIntentResponse)
initWithDefaultPartySize_defaultBookingDate_code_userActivity inGetAvailableRestaurantReservationBookingDefaultsIntentResponse  defaultPartySize defaultBookingDate code userActivity =
withObjCPtr defaultBookingDate $ \raw_defaultBookingDate ->
  withObjCPtr userActivity $ \raw_userActivity ->
      sendMsg inGetAvailableRestaurantReservationBookingDefaultsIntentResponse (mkSelector "initWithDefaultPartySize:defaultBookingDate:code:userActivity:") (retPtr retVoid) [argCULong (fromIntegral defaultPartySize), argPtr (castPtr raw_defaultBookingDate :: Ptr ()), argCLong (coerce code), argPtr (castPtr raw_userActivity :: Ptr ())] >>= ownedObject . castPtr

-- | @- defaultPartySize@
defaultPartySize :: IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse inGetAvailableRestaurantReservationBookingDefaultsIntentResponse => inGetAvailableRestaurantReservationBookingDefaultsIntentResponse -> IO CULong
defaultPartySize inGetAvailableRestaurantReservationBookingDefaultsIntentResponse  =
  sendMsg inGetAvailableRestaurantReservationBookingDefaultsIntentResponse (mkSelector "defaultPartySize") retCULong []

-- | @- defaultBookingDate@
defaultBookingDate :: IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse inGetAvailableRestaurantReservationBookingDefaultsIntentResponse => inGetAvailableRestaurantReservationBookingDefaultsIntentResponse -> IO (Id NSDate)
defaultBookingDate inGetAvailableRestaurantReservationBookingDefaultsIntentResponse  =
  sendMsg inGetAvailableRestaurantReservationBookingDefaultsIntentResponse (mkSelector "defaultBookingDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- maximumPartySize@
maximumPartySize :: IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse inGetAvailableRestaurantReservationBookingDefaultsIntentResponse => inGetAvailableRestaurantReservationBookingDefaultsIntentResponse -> IO (Id NSNumber)
maximumPartySize inGetAvailableRestaurantReservationBookingDefaultsIntentResponse  =
  sendMsg inGetAvailableRestaurantReservationBookingDefaultsIntentResponse (mkSelector "maximumPartySize") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMaximumPartySize:@
setMaximumPartySize :: (IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse inGetAvailableRestaurantReservationBookingDefaultsIntentResponse, IsNSNumber value) => inGetAvailableRestaurantReservationBookingDefaultsIntentResponse -> value -> IO ()
setMaximumPartySize inGetAvailableRestaurantReservationBookingDefaultsIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inGetAvailableRestaurantReservationBookingDefaultsIntentResponse (mkSelector "setMaximumPartySize:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minimumPartySize@
minimumPartySize :: IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse inGetAvailableRestaurantReservationBookingDefaultsIntentResponse => inGetAvailableRestaurantReservationBookingDefaultsIntentResponse -> IO (Id NSNumber)
minimumPartySize inGetAvailableRestaurantReservationBookingDefaultsIntentResponse  =
  sendMsg inGetAvailableRestaurantReservationBookingDefaultsIntentResponse (mkSelector "minimumPartySize") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinimumPartySize:@
setMinimumPartySize :: (IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse inGetAvailableRestaurantReservationBookingDefaultsIntentResponse, IsNSNumber value) => inGetAvailableRestaurantReservationBookingDefaultsIntentResponse -> value -> IO ()
setMinimumPartySize inGetAvailableRestaurantReservationBookingDefaultsIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inGetAvailableRestaurantReservationBookingDefaultsIntentResponse (mkSelector "setMinimumPartySize:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- providerImage@
providerImage :: IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse inGetAvailableRestaurantReservationBookingDefaultsIntentResponse => inGetAvailableRestaurantReservationBookingDefaultsIntentResponse -> IO (Id INImage)
providerImage inGetAvailableRestaurantReservationBookingDefaultsIntentResponse  =
  sendMsg inGetAvailableRestaurantReservationBookingDefaultsIntentResponse (mkSelector "providerImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProviderImage:@
setProviderImage :: (IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse inGetAvailableRestaurantReservationBookingDefaultsIntentResponse, IsINImage value) => inGetAvailableRestaurantReservationBookingDefaultsIntentResponse -> value -> IO ()
setProviderImage inGetAvailableRestaurantReservationBookingDefaultsIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inGetAvailableRestaurantReservationBookingDefaultsIntentResponse (mkSelector "setProviderImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- code@
code :: IsINGetAvailableRestaurantReservationBookingDefaultsIntentResponse inGetAvailableRestaurantReservationBookingDefaultsIntentResponse => inGetAvailableRestaurantReservationBookingDefaultsIntentResponse -> IO INGetAvailableRestaurantReservationBookingDefaultsIntentResponseCode
code inGetAvailableRestaurantReservationBookingDefaultsIntentResponse  =
  fmap (coerce :: CLong -> INGetAvailableRestaurantReservationBookingDefaultsIntentResponseCode) $ sendMsg inGetAvailableRestaurantReservationBookingDefaultsIntentResponse (mkSelector "code") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDefaultPartySize:defaultBookingDate:code:userActivity:@
initWithDefaultPartySize_defaultBookingDate_code_userActivitySelector :: Selector
initWithDefaultPartySize_defaultBookingDate_code_userActivitySelector = mkSelector "initWithDefaultPartySize:defaultBookingDate:code:userActivity:"

-- | @Selector@ for @defaultPartySize@
defaultPartySizeSelector :: Selector
defaultPartySizeSelector = mkSelector "defaultPartySize"

-- | @Selector@ for @defaultBookingDate@
defaultBookingDateSelector :: Selector
defaultBookingDateSelector = mkSelector "defaultBookingDate"

-- | @Selector@ for @maximumPartySize@
maximumPartySizeSelector :: Selector
maximumPartySizeSelector = mkSelector "maximumPartySize"

-- | @Selector@ for @setMaximumPartySize:@
setMaximumPartySizeSelector :: Selector
setMaximumPartySizeSelector = mkSelector "setMaximumPartySize:"

-- | @Selector@ for @minimumPartySize@
minimumPartySizeSelector :: Selector
minimumPartySizeSelector = mkSelector "minimumPartySize"

-- | @Selector@ for @setMinimumPartySize:@
setMinimumPartySizeSelector :: Selector
setMinimumPartySizeSelector = mkSelector "setMinimumPartySize:"

-- | @Selector@ for @providerImage@
providerImageSelector :: Selector
providerImageSelector = mkSelector "providerImage"

-- | @Selector@ for @setProviderImage:@
setProviderImageSelector :: Selector
setProviderImageSelector = mkSelector "setProviderImage:"

-- | @Selector@ for @code@
codeSelector :: Selector
codeSelector = mkSelector "code"

