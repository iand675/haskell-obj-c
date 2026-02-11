{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPreferences@.
module ObjC.Intents.INPreferences
  ( INPreferences
  , IsINPreferences(..)
  , siriAuthorizationStatus
  , requestSiriAuthorization
  , siriLanguageCode
  , siriAuthorizationStatusSelector
  , requestSiriAuthorizationSelector
  , siriLanguageCodeSelector

  -- * Enum types
  , INSiriAuthorizationStatus(INSiriAuthorizationStatus)
  , pattern INSiriAuthorizationStatusNotDetermined
  , pattern INSiriAuthorizationStatusRestricted
  , pattern INSiriAuthorizationStatusDenied
  , pattern INSiriAuthorizationStatusAuthorized

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

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ siriAuthorizationStatus@
siriAuthorizationStatus :: IO INSiriAuthorizationStatus
siriAuthorizationStatus  =
  do
    cls' <- getRequiredClass "INPreferences"
    fmap (coerce :: CLong -> INSiriAuthorizationStatus) $ sendClassMsg cls' (mkSelector "siriAuthorizationStatus") retCLong []

-- | @+ requestSiriAuthorization:@
requestSiriAuthorization :: Ptr () -> IO ()
requestSiriAuthorization handler =
  do
    cls' <- getRequiredClass "INPreferences"
    sendClassMsg cls' (mkSelector "requestSiriAuthorization:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | @+ siriLanguageCode@
siriLanguageCode :: IO (Id NSString)
siriLanguageCode  =
  do
    cls' <- getRequiredClass "INPreferences"
    sendClassMsg cls' (mkSelector "siriLanguageCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @siriAuthorizationStatus@
siriAuthorizationStatusSelector :: Selector
siriAuthorizationStatusSelector = mkSelector "siriAuthorizationStatus"

-- | @Selector@ for @requestSiriAuthorization:@
requestSiriAuthorizationSelector :: Selector
requestSiriAuthorizationSelector = mkSelector "requestSiriAuthorization:"

-- | @Selector@ for @siriLanguageCode@
siriLanguageCodeSelector :: Selector
siriLanguageCodeSelector = mkSelector "siriLanguageCode"

