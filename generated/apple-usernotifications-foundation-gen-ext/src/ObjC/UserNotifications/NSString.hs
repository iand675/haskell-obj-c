{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSString@.
module ObjC.UserNotifications.NSString
  ( NSString
  , IsNSString(..)
  , localizedUserNotificationStringForKey_arguments
  , localizedUserNotificationStringForKey_argumentsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.UserNotifications.Internal.Classes
import ObjC.Foundation.Internal.Classes
import Data.String (IsString(..))
import ObjC.Runtime.NSString (pureNSString)

-- | @+ localizedUserNotificationStringForKey:arguments:@
localizedUserNotificationStringForKey_arguments :: (IsNSString key, IsNSArray arguments) => key -> arguments -> IO (Id NSString)
localizedUserNotificationStringForKey_arguments key arguments =
  do
    cls' <- getRequiredClass "NSString"
    sendClassMessage cls' localizedUserNotificationStringForKey_argumentsSelector (toNSString key) (toNSArray arguments)


-- | Allows using @OverloadedStrings@ for @Id NSString@.
--
-- >>> :set -XOverloadedStrings
-- >>> let s = "hello" :: Id NSString
instance IsString (Id NSString) where
  fromString = pureNSString
-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @localizedUserNotificationStringForKey:arguments:@
localizedUserNotificationStringForKey_argumentsSelector :: Selector '[Id NSString, Id NSArray] (Id NSString)
localizedUserNotificationStringForKey_argumentsSelector = mkSelector "localizedUserNotificationStringForKey:arguments:"

