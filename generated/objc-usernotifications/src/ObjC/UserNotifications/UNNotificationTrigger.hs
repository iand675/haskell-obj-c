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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsUNNotificationTrigger unNotificationTrigger => unNotificationTrigger -> IO (Id UNNotificationTrigger)
init_ unNotificationTrigger  =
  sendMsg unNotificationTrigger (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- repeats@
repeats :: IsUNNotificationTrigger unNotificationTrigger => unNotificationTrigger -> IO Bool
repeats unNotificationTrigger  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg unNotificationTrigger (mkSelector "repeats") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @repeats@
repeatsSelector :: Selector
repeatsSelector = mkSelector "repeats"

