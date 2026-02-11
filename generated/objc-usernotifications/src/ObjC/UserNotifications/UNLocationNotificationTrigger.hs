{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @UNLocationNotificationTrigger@.
module ObjC.UserNotifications.UNLocationNotificationTrigger
  ( UNLocationNotificationTrigger
  , IsUNLocationNotificationTrigger(..)
  , triggerWithRegion_repeats
  , region
  , triggerWithRegion_repeatsSelector
  , regionSelector


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

import ObjC.UserNotifications.Internal.Classes
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ triggerWithRegion:repeats:@
triggerWithRegion_repeats :: IsCLRegion region => region -> Bool -> IO (Id UNLocationNotificationTrigger)
triggerWithRegion_repeats region repeats =
  do
    cls' <- getRequiredClass "UNLocationNotificationTrigger"
    withObjCPtr region $ \raw_region ->
      sendClassMsg cls' (mkSelector "triggerWithRegion:repeats:") (retPtr retVoid) [argPtr (castPtr raw_region :: Ptr ()), argCULong (if repeats then 1 else 0)] >>= retainedObject . castPtr

-- | @- region@
region :: IsUNLocationNotificationTrigger unLocationNotificationTrigger => unLocationNotificationTrigger -> IO (Id CLRegion)
region unLocationNotificationTrigger  =
  sendMsg unLocationNotificationTrigger (mkSelector "region") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @triggerWithRegion:repeats:@
triggerWithRegion_repeatsSelector :: Selector
triggerWithRegion_repeatsSelector = mkSelector "triggerWithRegion:repeats:"

-- | @Selector@ for @region@
regionSelector :: Selector
regionSelector = mkSelector "region"

