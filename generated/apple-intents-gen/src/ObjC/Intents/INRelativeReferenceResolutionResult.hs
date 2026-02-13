{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRelativeReferenceResolutionResult@.
module ObjC.Intents.INRelativeReferenceResolutionResult
  ( INRelativeReferenceResolutionResult
  , IsINRelativeReferenceResolutionResult(..)
  , successWithResolvedRelativeReference
  , successWithResolvedValue
  , confirmationRequiredWithRelativeReferenceToConfirm
  , confirmationRequiredWithValueToConfirm
  , confirmationRequiredWithRelativeReferenceToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector
  , successWithResolvedRelativeReferenceSelector
  , successWithResolvedValueSelector

  -- * Enum types
  , INRelativeReference(INRelativeReference)
  , pattern INRelativeReferenceUnknown
  , pattern INRelativeReferenceNext
  , pattern INRelativeReferencePrevious

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

-- | @+ successWithResolvedRelativeReference:@
successWithResolvedRelativeReference :: INRelativeReference -> IO (Id INRelativeReferenceResolutionResult)
successWithResolvedRelativeReference resolvedRelativeReference =
  do
    cls' <- getRequiredClass "INRelativeReferenceResolutionResult"
    sendClassMessage cls' successWithResolvedRelativeReferenceSelector resolvedRelativeReference

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INRelativeReference -> IO (Id INRelativeReferenceResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INRelativeReferenceResolutionResult"
    sendClassMessage cls' successWithResolvedValueSelector resolvedValue

-- | @+ confirmationRequiredWithRelativeReferenceToConfirm:@
confirmationRequiredWithRelativeReferenceToConfirm :: INRelativeReference -> IO (Id INRelativeReferenceResolutionResult)
confirmationRequiredWithRelativeReferenceToConfirm relativeReferenceToConfirm =
  do
    cls' <- getRequiredClass "INRelativeReferenceResolutionResult"
    sendClassMessage cls' confirmationRequiredWithRelativeReferenceToConfirmSelector relativeReferenceToConfirm

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INRelativeReference -> IO (Id INRelativeReferenceResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INRelativeReferenceResolutionResult"
    sendClassMessage cls' confirmationRequiredWithValueToConfirmSelector valueToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedRelativeReference:@
successWithResolvedRelativeReferenceSelector :: Selector '[INRelativeReference] (Id INRelativeReferenceResolutionResult)
successWithResolvedRelativeReferenceSelector = mkSelector "successWithResolvedRelativeReference:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector '[INRelativeReference] (Id INRelativeReferenceResolutionResult)
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithRelativeReferenceToConfirm:@
confirmationRequiredWithRelativeReferenceToConfirmSelector :: Selector '[INRelativeReference] (Id INRelativeReferenceResolutionResult)
confirmationRequiredWithRelativeReferenceToConfirmSelector = mkSelector "confirmationRequiredWithRelativeReferenceToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector '[INRelativeReference] (Id INRelativeReferenceResolutionResult)
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

