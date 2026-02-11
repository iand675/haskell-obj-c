{-# LANGUAGE PatternSynonyms #-}
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
  , newSelector
  , sessionRequiringAuthorizationSelector
  , sessionRequiringAuthorization_queue_handlerSelector
  , sessionRequiringAuthorization_fullAccuracyPurposeKeySelector
  , sessionRequiringAuthorization_fullAccuracyPurposeKey_queue_handlerSelector
  , invalidateSelector

  -- * Enum types
  , CLServiceSessionAuthorizationRequirement(CLServiceSessionAuthorizationRequirement)
  , pattern CLServiceSessionAuthorizationRequirementNone
  , pattern CLServiceSessionAuthorizationRequirementWhenInUse
  , pattern CLServiceSessionAuthorizationRequirementAlways

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

import ObjC.CoreLocation.Internal.Classes
import ObjC.CoreLocation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCLServiceSession clServiceSession => clServiceSession -> IO (Id CLServiceSession)
init_ clServiceSession  =
  sendMsg clServiceSession (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CLServiceSession)
new  =
  do
    cls' <- getRequiredClass "CLServiceSession"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ sessionRequiringAuthorization:@
sessionRequiringAuthorization :: CLServiceSessionAuthorizationRequirement -> IO (Id CLServiceSession)
sessionRequiringAuthorization authorizationRequirement =
  do
    cls' <- getRequiredClass "CLServiceSession"
    sendClassMsg cls' (mkSelector "sessionRequiringAuthorization:") (retPtr retVoid) [argCLong (coerce authorizationRequirement)] >>= retainedObject . castPtr

-- | @+ sessionRequiringAuthorization:queue:handler:@
sessionRequiringAuthorization_queue_handler :: IsNSObject queue => CLServiceSessionAuthorizationRequirement -> queue -> Ptr () -> IO (Id CLServiceSession)
sessionRequiringAuthorization_queue_handler authorizationRequirement queue handler =
  do
    cls' <- getRequiredClass "CLServiceSession"
    withObjCPtr queue $ \raw_queue ->
      sendClassMsg cls' (mkSelector "sessionRequiringAuthorization:queue:handler:") (retPtr retVoid) [argCLong (coerce authorizationRequirement), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr handler :: Ptr ())] >>= retainedObject . castPtr

-- | @+ sessionRequiringAuthorization:fullAccuracyPurposeKey:@
sessionRequiringAuthorization_fullAccuracyPurposeKey :: IsNSString purposeKey => CLServiceSessionAuthorizationRequirement -> purposeKey -> IO (Id CLServiceSession)
sessionRequiringAuthorization_fullAccuracyPurposeKey authorizationRequirement purposeKey =
  do
    cls' <- getRequiredClass "CLServiceSession"
    withObjCPtr purposeKey $ \raw_purposeKey ->
      sendClassMsg cls' (mkSelector "sessionRequiringAuthorization:fullAccuracyPurposeKey:") (retPtr retVoid) [argCLong (coerce authorizationRequirement), argPtr (castPtr raw_purposeKey :: Ptr ())] >>= retainedObject . castPtr

-- | @+ sessionRequiringAuthorization:fullAccuracyPurposeKey:queue:handler:@
sessionRequiringAuthorization_fullAccuracyPurposeKey_queue_handler :: (IsNSString purposeKey, IsNSObject queue) => CLServiceSessionAuthorizationRequirement -> purposeKey -> queue -> Ptr () -> IO (Id CLServiceSession)
sessionRequiringAuthorization_fullAccuracyPurposeKey_queue_handler authorizationRequirement purposeKey queue handler =
  do
    cls' <- getRequiredClass "CLServiceSession"
    withObjCPtr purposeKey $ \raw_purposeKey ->
      withObjCPtr queue $ \raw_queue ->
        sendClassMsg cls' (mkSelector "sessionRequiringAuthorization:fullAccuracyPurposeKey:queue:handler:") (retPtr retVoid) [argCLong (coerce authorizationRequirement), argPtr (castPtr raw_purposeKey :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr handler :: Ptr ())] >>= retainedObject . castPtr

-- | @- invalidate@
invalidate :: IsCLServiceSession clServiceSession => clServiceSession -> IO ()
invalidate clServiceSession  =
  sendMsg clServiceSession (mkSelector "invalidate") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @sessionRequiringAuthorization:@
sessionRequiringAuthorizationSelector :: Selector
sessionRequiringAuthorizationSelector = mkSelector "sessionRequiringAuthorization:"

-- | @Selector@ for @sessionRequiringAuthorization:queue:handler:@
sessionRequiringAuthorization_queue_handlerSelector :: Selector
sessionRequiringAuthorization_queue_handlerSelector = mkSelector "sessionRequiringAuthorization:queue:handler:"

-- | @Selector@ for @sessionRequiringAuthorization:fullAccuracyPurposeKey:@
sessionRequiringAuthorization_fullAccuracyPurposeKeySelector :: Selector
sessionRequiringAuthorization_fullAccuracyPurposeKeySelector = mkSelector "sessionRequiringAuthorization:fullAccuracyPurposeKey:"

-- | @Selector@ for @sessionRequiringAuthorization:fullAccuracyPurposeKey:queue:handler:@
sessionRequiringAuthorization_fullAccuracyPurposeKey_queue_handlerSelector :: Selector
sessionRequiringAuthorization_fullAccuracyPurposeKey_queue_handlerSelector = mkSelector "sessionRequiringAuthorization:fullAccuracyPurposeKey:queue:handler:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector
invalidateSelector = mkSelector "invalidate"

