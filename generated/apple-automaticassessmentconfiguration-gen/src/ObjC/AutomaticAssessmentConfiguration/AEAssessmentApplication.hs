{-# LANGUAGE DataKinds #-}
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
  , teamIdentifier
  , requiresSignatureValidation
  , setRequiresSignatureValidation
  , bundleIdentifierSelector
  , initSelector
  , initWithBundleIdentifierSelector
  , initWithBundleIdentifier_teamIdentifierSelector
  , newSelector
  , requiresSignatureValidationSelector
  , setRequiresSignatureValidationSelector
  , teamIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AutomaticAssessmentConfiguration.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithBundleIdentifier:@
initWithBundleIdentifier :: (IsAEAssessmentApplication aeAssessmentApplication, IsNSString bundleIdentifier) => aeAssessmentApplication -> bundleIdentifier -> IO (Id AEAssessmentApplication)
initWithBundleIdentifier aeAssessmentApplication bundleIdentifier =
  sendOwnedMessage aeAssessmentApplication initWithBundleIdentifierSelector (toNSString bundleIdentifier)

-- | @- initWithBundleIdentifier:teamIdentifier:@
initWithBundleIdentifier_teamIdentifier :: (IsAEAssessmentApplication aeAssessmentApplication, IsNSString bundleIdentifier, IsNSString teamIdentifier) => aeAssessmentApplication -> bundleIdentifier -> teamIdentifier -> IO (Id AEAssessmentApplication)
initWithBundleIdentifier_teamIdentifier aeAssessmentApplication bundleIdentifier teamIdentifier =
  sendOwnedMessage aeAssessmentApplication initWithBundleIdentifier_teamIdentifierSelector (toNSString bundleIdentifier) (toNSString teamIdentifier)

-- | @- init@
init_ :: IsAEAssessmentApplication aeAssessmentApplication => aeAssessmentApplication -> IO (Id AEAssessmentApplication)
init_ aeAssessmentApplication =
  sendOwnedMessage aeAssessmentApplication initSelector

-- | @+ new@
new :: IO (Id AEAssessmentApplication)
new  =
  do
    cls' <- getRequiredClass "AEAssessmentApplication"
    sendOwnedClassMessage cls' newSelector

-- | @- bundleIdentifier@
bundleIdentifier :: IsAEAssessmentApplication aeAssessmentApplication => aeAssessmentApplication -> IO (Id NSString)
bundleIdentifier aeAssessmentApplication =
  sendMessage aeAssessmentApplication bundleIdentifierSelector

-- | @- teamIdentifier@
teamIdentifier :: IsAEAssessmentApplication aeAssessmentApplication => aeAssessmentApplication -> IO (Id NSString)
teamIdentifier aeAssessmentApplication =
  sendMessage aeAssessmentApplication teamIdentifierSelector

-- | @- requiresSignatureValidation@
requiresSignatureValidation :: IsAEAssessmentApplication aeAssessmentApplication => aeAssessmentApplication -> IO Bool
requiresSignatureValidation aeAssessmentApplication =
  sendMessage aeAssessmentApplication requiresSignatureValidationSelector

-- | @- setRequiresSignatureValidation:@
setRequiresSignatureValidation :: IsAEAssessmentApplication aeAssessmentApplication => aeAssessmentApplication -> Bool -> IO ()
setRequiresSignatureValidation aeAssessmentApplication value =
  sendMessage aeAssessmentApplication setRequiresSignatureValidationSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBundleIdentifier:@
initWithBundleIdentifierSelector :: Selector '[Id NSString] (Id AEAssessmentApplication)
initWithBundleIdentifierSelector = mkSelector "initWithBundleIdentifier:"

-- | @Selector@ for @initWithBundleIdentifier:teamIdentifier:@
initWithBundleIdentifier_teamIdentifierSelector :: Selector '[Id NSString, Id NSString] (Id AEAssessmentApplication)
initWithBundleIdentifier_teamIdentifierSelector = mkSelector "initWithBundleIdentifier:teamIdentifier:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AEAssessmentApplication)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AEAssessmentApplication)
newSelector = mkSelector "new"

-- | @Selector@ for @bundleIdentifier@
bundleIdentifierSelector :: Selector '[] (Id NSString)
bundleIdentifierSelector = mkSelector "bundleIdentifier"

-- | @Selector@ for @teamIdentifier@
teamIdentifierSelector :: Selector '[] (Id NSString)
teamIdentifierSelector = mkSelector "teamIdentifier"

-- | @Selector@ for @requiresSignatureValidation@
requiresSignatureValidationSelector :: Selector '[] Bool
requiresSignatureValidationSelector = mkSelector "requiresSignatureValidation"

-- | @Selector@ for @setRequiresSignatureValidation:@
setRequiresSignatureValidationSelector :: Selector '[Bool] ()
setRequiresSignatureValidationSelector = mkSelector "setRequiresSignatureValidation:"

