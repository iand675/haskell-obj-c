{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLServiceSession@.
module ObjC.CoreLocation.CLServiceSession
  ( CLServiceSession
  , IsCLServiceSession(..)
  , init_
  , new
  , sessionRequiringAuthorization
  , sessionRequiringAuthorization_queue_handler
  , sessionRequiringAuthorization_fullAccuracyPurposeKey
  , sessionRequiringAuthorization_fullAccuracyPurposeKey_queue_handler
  , invalidate
  , initSelector
  , invalidateSelector
  , newSelector
  , sessionRequiringAuthorizationSelector
  , sessionRequiringAuthorization_fullAccuracyPurposeKeySelector
  , sessionRequiringAuthorization_fullAccuracyPurposeKey_queue_handlerSelector
  , sessionRequiringAuthorization_queue_handlerSelector

  -- * Enum types
  , CLServiceSessionAuthorizationRequirement(CLServiceSessionAuthorizationRequirement)
  , pattern CLServiceSessionAuthorizationRequirementNone
  , pattern CLServiceSessionAuthorizationRequirementWhenInUse
  , pattern CLServiceSessionAuthorizationRequirementAlways

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreLocation.Internal.Classes
import ObjC.CoreLocation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCLServiceSession clServiceSession => clServiceSession -> IO (Id CLServiceSession)
init_ clServiceSession =
  sendOwnedMessage clServiceSession initSelector

-- | @+ new@
new :: IO (Id CLServiceSession)
new  =
  do
    cls' <- getRequiredClass "CLServiceSession"
    sendOwnedClassMessage cls' newSelector

-- | @+ sessionRequiringAuthorization:@
sessionRequiringAuthorization :: CLServiceSessionAuthorizationRequirement -> IO (Id CLServiceSession)
sessionRequiringAuthorization authorizationRequirement =
  do
    cls' <- getRequiredClass "CLServiceSession"
    sendClassMessage cls' sessionRequiringAuthorizationSelector authorizationRequirement

-- | @+ sessionRequiringAuthorization:queue:handler:@
sessionRequiringAuthorization_queue_handler :: IsNSObject queue => CLServiceSessionAuthorizationRequirement -> queue -> Ptr () -> IO (Id CLServiceSession)
sessionRequiringAuthorization_queue_handler authorizationRequirement queue handler =
  do
    cls' <- getRequiredClass "CLServiceSession"
    sendClassMessage cls' sessionRequiringAuthorization_queue_handlerSelector authorizationRequirement (toNSObject queue) handler

-- | @+ sessionRequiringAuthorization:fullAccuracyPurposeKey:@
sessionRequiringAuthorization_fullAccuracyPurposeKey :: IsNSString purposeKey => CLServiceSessionAuthorizationRequirement -> purposeKey -> IO (Id CLServiceSession)
sessionRequiringAuthorization_fullAccuracyPurposeKey authorizationRequirement purposeKey =
  do
    cls' <- getRequiredClass "CLServiceSession"
    sendClassMessage cls' sessionRequiringAuthorization_fullAccuracyPurposeKeySelector authorizationRequirement (toNSString purposeKey)

-- | @+ sessionRequiringAuthorization:fullAccuracyPurposeKey:queue:handler:@
sessionRequiringAuthorization_fullAccuracyPurposeKey_queue_handler :: (IsNSString purposeKey, IsNSObject queue) => CLServiceSessionAuthorizationRequirement -> purposeKey -> queue -> Ptr () -> IO (Id CLServiceSession)
sessionRequiringAuthorization_fullAccuracyPurposeKey_queue_handler authorizationRequirement purposeKey queue handler =
  do
    cls' <- getRequiredClass "CLServiceSession"
    sendClassMessage cls' sessionRequiringAuthorization_fullAccuracyPurposeKey_queue_handlerSelector authorizationRequirement (toNSString purposeKey) (toNSObject queue) handler

-- | @- invalidate@
invalidate :: IsCLServiceSession clServiceSession => clServiceSession -> IO ()
invalidate clServiceSession =
  sendMessage clServiceSession invalidateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CLServiceSession)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CLServiceSession)
newSelector = mkSelector "new"

-- | @Selector@ for @sessionRequiringAuthorization:@
sessionRequiringAuthorizationSelector :: Selector '[CLServiceSessionAuthorizationRequirement] (Id CLServiceSession)
sessionRequiringAuthorizationSelector = mkSelector "sessionRequiringAuthorization:"

-- | @Selector@ for @sessionRequiringAuthorization:queue:handler:@
sessionRequiringAuthorization_queue_handlerSelector :: Selector '[CLServiceSessionAuthorizationRequirement, Id NSObject, Ptr ()] (Id CLServiceSession)
sessionRequiringAuthorization_queue_handlerSelector = mkSelector "sessionRequiringAuthorization:queue:handler:"

-- | @Selector@ for @sessionRequiringAuthorization:fullAccuracyPurposeKey:@
sessionRequiringAuthorization_fullAccuracyPurposeKeySelector :: Selector '[CLServiceSessionAuthorizationRequirement, Id NSString] (Id CLServiceSession)
sessionRequiringAuthorization_fullAccuracyPurposeKeySelector = mkSelector "sessionRequiringAuthorization:fullAccuracyPurposeKey:"

-- | @Selector@ for @sessionRequiringAuthorization:fullAccuracyPurposeKey:queue:handler:@
sessionRequiringAuthorization_fullAccuracyPurposeKey_queue_handlerSelector :: Selector '[CLServiceSessionAuthorizationRequirement, Id NSString, Id NSObject, Ptr ()] (Id CLServiceSession)
sessionRequiringAuthorization_fullAccuracyPurposeKey_queue_handlerSelector = mkSelector "sessionRequiringAuthorization:fullAccuracyPurposeKey:queue:handler:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector '[] ()
invalidateSelector = mkSelector "invalidate"

