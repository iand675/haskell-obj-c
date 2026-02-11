{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AEAssessmentApplication@.
module ObjC.AutomaticAssessmentConfiguration.AEAssessmentApplication
  ( AEAssessmentApplication
  , IsAEAssessmentApplication(..)
  , initWithBundleIdentifier
  , initWithBundleIdentifier_teamIdentifier
  , init_
  , new
  , bundleIdentifier
  , requiresSignatureValidation
  , setRequiresSignatureValidation
  , initWithBundleIdentifierSelector
  , initWithBundleIdentifier_teamIdentifierSelector
  , initSelector
  , newSelector
  , bundleIdentifierSelector
  , requiresSignatureValidationSelector
  , setRequiresSignatureValidationSelector


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

import ObjC.AutomaticAssessmentConfiguration.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithBundleIdentifier:@
initWithBundleIdentifier :: (IsAEAssessmentApplication aeAssessmentApplication, IsNSString bundleIdentifier) => aeAssessmentApplication -> bundleIdentifier -> IO (Id AEAssessmentApplication)
initWithBundleIdentifier aeAssessmentApplication  bundleIdentifier =
withObjCPtr bundleIdentifier $ \raw_bundleIdentifier ->
    sendMsg aeAssessmentApplication (mkSelector "initWithBundleIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_bundleIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithBundleIdentifier:teamIdentifier:@
initWithBundleIdentifier_teamIdentifier :: (IsAEAssessmentApplication aeAssessmentApplication, IsNSString bundleIdentifier, IsNSString teamIdentifier) => aeAssessmentApplication -> bundleIdentifier -> teamIdentifier -> IO (Id AEAssessmentApplication)
initWithBundleIdentifier_teamIdentifier aeAssessmentApplication  bundleIdentifier teamIdentifier =
withObjCPtr bundleIdentifier $ \raw_bundleIdentifier ->
  withObjCPtr teamIdentifier $ \raw_teamIdentifier ->
      sendMsg aeAssessmentApplication (mkSelector "initWithBundleIdentifier:teamIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_bundleIdentifier :: Ptr ()), argPtr (castPtr raw_teamIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsAEAssessmentApplication aeAssessmentApplication => aeAssessmentApplication -> IO (Id AEAssessmentApplication)
init_ aeAssessmentApplication  =
  sendMsg aeAssessmentApplication (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AEAssessmentApplication)
new  =
  do
    cls' <- getRequiredClass "AEAssessmentApplication"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- bundleIdentifier@
bundleIdentifier :: IsAEAssessmentApplication aeAssessmentApplication => aeAssessmentApplication -> IO (Id NSString)
bundleIdentifier aeAssessmentApplication  =
  sendMsg aeAssessmentApplication (mkSelector "bundleIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- requiresSignatureValidation@
requiresSignatureValidation :: IsAEAssessmentApplication aeAssessmentApplication => aeAssessmentApplication -> IO Bool
requiresSignatureValidation aeAssessmentApplication  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg aeAssessmentApplication (mkSelector "requiresSignatureValidation") retCULong []

-- | @- setRequiresSignatureValidation:@
setRequiresSignatureValidation :: IsAEAssessmentApplication aeAssessmentApplication => aeAssessmentApplication -> Bool -> IO ()
setRequiresSignatureValidation aeAssessmentApplication  value =
  sendMsg aeAssessmentApplication (mkSelector "setRequiresSignatureValidation:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBundleIdentifier:@
initWithBundleIdentifierSelector :: Selector
initWithBundleIdentifierSelector = mkSelector "initWithBundleIdentifier:"

-- | @Selector@ for @initWithBundleIdentifier:teamIdentifier:@
initWithBundleIdentifier_teamIdentifierSelector :: Selector
initWithBundleIdentifier_teamIdentifierSelector = mkSelector "initWithBundleIdentifier:teamIdentifier:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @bundleIdentifier@
bundleIdentifierSelector :: Selector
bundleIdentifierSelector = mkSelector "bundleIdentifier"

-- | @Selector@ for @requiresSignatureValidation@
requiresSignatureValidationSelector :: Selector
requiresSignatureValidationSelector = mkSelector "requiresSignatureValidation"

-- | @Selector@ for @setRequiresSignatureValidation:@
setRequiresSignatureValidationSelector :: Selector
setRequiresSignatureValidationSelector = mkSelector "setRequiresSignatureValidation:"

