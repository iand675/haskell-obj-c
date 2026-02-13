{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , requestSiriAuthorizationSelector
  , siriAuthorizationStatusSelector
  , siriLanguageCodeSelector

  -- * Enum types
  , INSiriAuthorizationStatus(INSiriAuthorizationStatus)
  , pattern INSiriAuthorizationStatusNotDetermined
  , pattern INSiriAuthorizationStatusRestricted
  , pattern INSiriAuthorizationStatusDenied
  , pattern INSiriAuthorizationStatusAuthorized

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' siriAuthorizationStatusSelector

-- | @+ requestSiriAuthorization:@
requestSiriAuthorization :: Ptr () -> IO ()
requestSiriAuthorization handler =
  do
    cls' <- getRequiredClass "INPreferences"
    sendClassMessage cls' requestSiriAuthorizationSelector handler

-- | @+ siriLanguageCode@
siriLanguageCode :: IO (Id NSString)
siriLanguageCode  =
  do
    cls' <- getRequiredClass "INPreferences"
    sendClassMessage cls' siriLanguageCodeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @siriAuthorizationStatus@
siriAuthorizationStatusSelector :: Selector '[] INSiriAuthorizationStatus
siriAuthorizationStatusSelector = mkSelector "siriAuthorizationStatus"

-- | @Selector@ for @requestSiriAuthorization:@
requestSiriAuthorizationSelector :: Selector '[Ptr ()] ()
requestSiriAuthorizationSelector = mkSelector "requestSiriAuthorization:"

-- | @Selector@ for @siriLanguageCode@
siriLanguageCodeSelector :: Selector '[] (Id NSString)
siriLanguageCodeSelector = mkSelector "siriLanguageCode"

