{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' defaultSubscriptionRegistrationCenterSelector

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
setCurrentSubscription vsSubscriptionRegistrationCenter currentSubscription =
  sendMessage vsSubscriptionRegistrationCenter setCurrentSubscriptionSelector (toVSSubscription currentSubscription)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultSubscriptionRegistrationCenter@
defaultSubscriptionRegistrationCenterSelector :: Selector '[] (Id VSSubscriptionRegistrationCenter)
defaultSubscriptionRegistrationCenterSelector = mkSelector "defaultSubscriptionRegistrationCenter"

-- | @Selector@ for @setCurrentSubscription:@
setCurrentSubscriptionSelector :: Selector '[Id VSSubscription] ()
setCurrentSubscriptionSelector = mkSelector "setCurrentSubscription:"

