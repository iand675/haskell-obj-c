{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @UNNotificationTrigger@.
module ObjC.UserNotifications.UNNotificationTrigger
  ( UNNotificationTrigger
  , IsUNNotificationTrigger(..)
  , init_
  , repeats
  , initSelector
  , repeatsSelector


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
init_ :: IsUNNotificationTrigger unNotificationTrigger => unNotificationTrigger -> IO (Id UNNotificationTrigger)
init_ unNotificationTrigger =
  sendOwnedMessage unNotificationTrigger initSelector

-- | @- repeats@
repeats :: IsUNNotificationTrigger unNotificationTrigger => unNotificationTrigger -> IO Bool
repeats unNotificationTrigger =
  sendMessage unNotificationTrigger repeatsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id UNNotificationTrigger)
initSelector = mkSelector "init"

-- | @Selector@ for @repeats@
repeatsSelector :: Selector '[] Bool
repeatsSelector = mkSelector "repeats"

