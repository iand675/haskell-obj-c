{-# LANGUAGE FlexibleInstances #-}
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
import Data.String (IsString(..))
import ObjC.Runtime.NSString (pureNSString)

-- | @+ localizedUserNotificationStringForKey:arguments:@
localizedUserNotificationStringForKey_arguments :: (IsNSString key, IsNSArray arguments) => key -> arguments -> IO (Id NSString)
localizedUserNotificationStringForKey_arguments key arguments =
  do
    cls' <- getRequiredClass "NSString"
    withObjCPtr key $ \raw_key ->
      withObjCPtr arguments $ \raw_arguments ->
        sendClassMsg cls' (mkSelector "localizedUserNotificationStringForKey:arguments:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_arguments :: Ptr ())] >>= retainedObject . castPtr


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
localizedUserNotificationStringForKey_argumentsSelector :: Selector
localizedUserNotificationStringForKey_argumentsSelector = mkSelector "localizedUserNotificationStringForKey:arguments:"

