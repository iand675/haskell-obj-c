{-# LANGUAGE PatternSynonyms #-}
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
  , successWithResolvedRelativeReferenceSelector
  , successWithResolvedValueSelector
  , confirmationRequiredWithRelativeReferenceToConfirmSelector
  , confirmationRequiredWithValueToConfirmSelector

  -- * Enum types
  , INRelativeReference(INRelativeReference)
  , pattern INRelativeReferenceUnknown
  , pattern INRelativeReferenceNext
  , pattern INRelativeReferencePrevious

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

-- | @+ successWithResolvedRelativeReference:@
successWithResolvedRelativeReference :: INRelativeReference -> IO (Id INRelativeReferenceResolutionResult)
successWithResolvedRelativeReference resolvedRelativeReference =
  do
    cls' <- getRequiredClass "INRelativeReferenceResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedRelativeReference:") (retPtr retVoid) [argCLong (coerce resolvedRelativeReference)] >>= retainedObject . castPtr

-- | @+ successWithResolvedValue:@
successWithResolvedValue :: INRelativeReference -> IO (Id INRelativeReferenceResolutionResult)
successWithResolvedValue resolvedValue =
  do
    cls' <- getRequiredClass "INRelativeReferenceResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedValue:") (retPtr retVoid) [argCLong (coerce resolvedValue)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithRelativeReferenceToConfirm:@
confirmationRequiredWithRelativeReferenceToConfirm :: INRelativeReference -> IO (Id INRelativeReferenceResolutionResult)
confirmationRequiredWithRelativeReferenceToConfirm relativeReferenceToConfirm =
  do
    cls' <- getRequiredClass "INRelativeReferenceResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithRelativeReferenceToConfirm:") (retPtr retVoid) [argCLong (coerce relativeReferenceToConfirm)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirm :: INRelativeReference -> IO (Id INRelativeReferenceResolutionResult)
confirmationRequiredWithValueToConfirm valueToConfirm =
  do
    cls' <- getRequiredClass "INRelativeReferenceResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithValueToConfirm:") (retPtr retVoid) [argCLong (coerce valueToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedRelativeReference:@
successWithResolvedRelativeReferenceSelector :: Selector
successWithResolvedRelativeReferenceSelector = mkSelector "successWithResolvedRelativeReference:"

-- | @Selector@ for @successWithResolvedValue:@
successWithResolvedValueSelector :: Selector
successWithResolvedValueSelector = mkSelector "successWithResolvedValue:"

-- | @Selector@ for @confirmationRequiredWithRelativeReferenceToConfirm:@
confirmationRequiredWithRelativeReferenceToConfirmSelector :: Selector
confirmationRequiredWithRelativeReferenceToConfirmSelector = mkSelector "confirmationRequiredWithRelativeReferenceToConfirm:"

-- | @Selector@ for @confirmationRequiredWithValueToConfirm:@
confirmationRequiredWithValueToConfirmSelector :: Selector
confirmationRequiredWithValueToConfirmSelector = mkSelector "confirmationRequiredWithValueToConfirm:"

