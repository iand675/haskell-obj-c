{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @UNLocationNotificationTrigger@.
module ObjC.UserNotifications.UNLocationNotificationTrigger
  ( UNLocationNotificationTrigger
  , IsUNLocationNotificationTrigger(..)
  , triggerWithRegion_repeats
  , region
  , regionSelector
  , triggerWithRegion_repeatsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.UserNotifications.Internal.Classes
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ triggerWithRegion:repeats:@
triggerWithRegion_repeats :: IsCLRegion region => region -> Bool -> IO (Id UNLocationNotificationTrigger)
triggerWithRegion_repeats region repeats =
  do
    cls' <- getRequiredClass "UNLocationNotificationTrigger"
    sendClassMessage cls' triggerWithRegion_repeatsSelector (toCLRegion region) repeats

-- | @- region@
region :: IsUNLocationNotificationTrigger unLocationNotificationTrigger => unLocationNotificationTrigger -> IO (Id CLRegion)
region unLocationNotificationTrigger =
  sendMessage unLocationNotificationTrigger regionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @triggerWithRegion:repeats:@
triggerWithRegion_repeatsSelector :: Selector '[Id CLRegion, Bool] (Id UNLocationNotificationTrigger)
triggerWithRegion_repeatsSelector = mkSelector "triggerWithRegion:repeats:"

-- | @Selector@ for @region@
regionSelector :: Selector '[] (Id CLRegion)
regionSelector = mkSelector "region"

