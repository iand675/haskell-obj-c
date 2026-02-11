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
  , initSelector
  , dateSelector
  , requestSelector


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
init_ :: IsUNNotification unNotification => unNotification -> IO (Id UNNotification)
init_ unNotification  =
  sendMsg unNotification (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- date@
date :: IsUNNotification unNotification => unNotification -> IO (Id NSDate)
date unNotification  =
  sendMsg unNotification (mkSelector "date") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- request@
request :: IsUNNotification unNotification => unNotification -> IO (Id UNNotificationRequest)
request unNotification  =
  sendMsg unNotification (mkSelector "request") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @date@
dateSelector :: Selector
dateSelector = mkSelector "date"

-- | @Selector@ for @request@
requestSelector :: Selector
requestSelector = mkSelector "request"

