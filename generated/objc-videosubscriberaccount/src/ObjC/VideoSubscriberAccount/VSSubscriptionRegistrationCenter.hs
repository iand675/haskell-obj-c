{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VSSubscriptionRegistrationCenter stores subscription information.
--
-- Generated bindings for @VSSubscriptionRegistrationCenter@.
module ObjC.VideoSubscriberAccount.VSSubscriptionRegistrationCenter
  ( VSSubscriptionRegistrationCenter
  , IsVSSubscriptionRegistrationCenter(..)
  , defaultSubscriptionRegistrationCenter
  , setCurrentSubscription
  , defaultSubscriptionRegistrationCenterSelector
  , setCurrentSubscriptionSelector


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

import ObjC.VideoSubscriberAccount.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Use the default subscription registration center to tell the system about the customer's ability to access content within your app.
--
-- ObjC selector: @+ defaultSubscriptionRegistrationCenter@
defaultSubscriptionRegistrationCenter :: IO (Id VSSubscriptionRegistrationCenter)
defaultSubscriptionRegistrationCenter  =
  do
    cls' <- getRequiredClass "VSSubscriptionRegistrationCenter"
    sendClassMsg cls' (mkSelector "defaultSubscriptionRegistrationCenter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provide a subscription when the subscriber first authenticates, and when the subscription changes.
--
-- When the subscriber signs out or otherwise loses access to subscription content, invoke this method with nil.
--
-- You might also want to call this method opportunistically, if you happen to have just confirmed the validity of the subscription, or in response to app lifecycle events, e.g. when your app becomes active.  The system may use this activity as a hint that the user is actively using the subscription.
--
-- It is an error to provide a current subscription with an unknown access level; you should not provide a subscription if the user only has access to content that is offered for free without any account requirements.
--
-- ObjC selector: @- setCurrentSubscription:@
setCurrentSubscription :: (IsVSSubscriptionRegistrationCenter vsSubscriptionRegistrationCenter, IsVSSubscription currentSubscription) => vsSubscriptionRegistrationCenter -> currentSubscription -> IO ()
setCurrentSubscription vsSubscriptionRegistrationCenter  currentSubscription =
withObjCPtr currentSubscription $ \raw_currentSubscription ->
    sendMsg vsSubscriptionRegistrationCenter (mkSelector "setCurrentSubscription:") retVoid [argPtr (castPtr raw_currentSubscription :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultSubscriptionRegistrationCenter@
defaultSubscriptionRegistrationCenterSelector :: Selector
defaultSubscriptionRegistrationCenterSelector = mkSelector "defaultSubscriptionRegistrationCenter"

-- | @Selector@ for @setCurrentSubscription:@
setCurrentSubscriptionSelector :: Selector
setCurrentSubscriptionSelector = mkSelector "setCurrentSubscription:"

