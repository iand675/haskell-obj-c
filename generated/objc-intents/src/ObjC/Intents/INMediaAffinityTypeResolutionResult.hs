{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INMediaAffinityTypeResolutionResult@.
module ObjC.Intents.INMediaAffinityTypeResolutionResult
  ( INMediaAffinityTypeResolutionResult
  , IsINMediaAffinityTypeResolutionResult(..)
  , successWithResolvedMediaAffinityType
  , confirmationRequiredWithMediaAffinityTypeToConfirm
  , successWithResolvedMediaAffinityTypeSelector
  , confirmationRequiredWithMediaAffinityTypeToConfirmSelector

  -- * Enum types
  , INMediaAffinityType(INMediaAffinityType)
  , pattern INMediaAffinityTypeUnknown
  , pattern INMediaAffinityTypeLike
  , pattern INMediaAffinityTypeDislike

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

-- | @+ successWithResolvedMediaAffinityType:@
successWithResolvedMediaAffinityType :: INMediaAffinityType -> IO (Id INMediaAffinityTypeResolutionResult)
successWithResolvedMediaAffinityType resolvedMediaAffinityType =
  do
    cls' <- getRequiredClass "INMediaAffinityTypeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedMediaAffinityType:") (retPtr retVoid) [argCLong (coerce resolvedMediaAffinityType)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithMediaAffinityTypeToConfirm:@
confirmationRequiredWithMediaAffinityTypeToConfirm :: INMediaAffinityType -> IO (Id INMediaAffinityTypeResolutionResult)
confirmationRequiredWithMediaAffinityTypeToConfirm mediaAffinityTypeToConfirm =
  do
    cls' <- getRequiredClass "INMediaAffinityTypeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithMediaAffinityTypeToConfirm:") (retPtr retVoid) [argCLong (coerce mediaAffinityTypeToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedMediaAffinityType:@
successWithResolvedMediaAffinityTypeSelector :: Selector
successWithResolvedMediaAffinityTypeSelector = mkSelector "successWithResolvedMediaAffinityType:"

-- | @Selector@ for @confirmationRequiredWithMediaAffinityTypeToConfirm:@
confirmationRequiredWithMediaAffinityTypeToConfirmSelector :: Selector
confirmationRequiredWithMediaAffinityTypeToConfirmSelector = mkSelector "confirmationRequiredWithMediaAffinityTypeToConfirm:"

