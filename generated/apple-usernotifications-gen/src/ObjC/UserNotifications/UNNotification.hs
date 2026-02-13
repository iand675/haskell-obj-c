{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @UNNotification@.
module ObjC.UserNotifications.UNNotification
  ( UNNotification
  , IsUNNotification(..)
  , init_
  , date
  , request
  , dateSelector
  , initSelector
  , requestSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.UserNotifications.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsUNNotification unNotification => unNotification -> IO (Id UNNotification)
init_ unNotification =
  sendOwnedMessage unNotification initSelector

-- | @- date@
date :: IsUNNotification unNotification => unNotification -> IO (Id NSDate)
date unNotification =
  sendMessage unNotification dateSelector

-- | @- request@
request :: IsUNNotification unNotification => unNotification -> IO (Id UNNotificationRequest)
request unNotification =
  sendMessage unNotification requestSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id UNNotification)
initSelector = mkSelector "init"

-- | @Selector@ for @date@
dateSelector :: Selector '[] (Id NSDate)
dateSelector = mkSelector "date"

-- | @Selector@ for @request@
requestSelector :: Selector '[] (Id UNNotificationRequest)
requestSelector = mkSelector "request"

