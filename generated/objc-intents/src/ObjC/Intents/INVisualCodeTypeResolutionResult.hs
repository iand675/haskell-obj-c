{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INVisualCodeTypeResolutionResult@.
module ObjC.Intents.INVisualCodeTypeResolutionResult
  ( INVisualCodeTypeResolutionResult
  , IsINVisualCodeTypeResolutionResult(..)
  , successWithResolvedVisualCodeType
  , confirmationRequiredWithVisualCodeTypeToConfirm
  , successWithResolvedVisualCodeTypeSelector
  , confirmationRequiredWithVisualCodeTypeToConfirmSelector

  -- * Enum types
  , INVisualCodeType(INVisualCodeType)
  , pattern INVisualCodeTypeUnknown
  , pattern INVisualCodeTypeContact
  , pattern INVisualCodeTypeRequestPayment
  , pattern INVisualCodeTypeSendPayment
  , pattern INVisualCodeTypeTransit
  , pattern INVisualCodeTypeBus
  , pattern INVisualCodeTypeSubway

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

-- | @+ successWithResolvedVisualCodeType:@
successWithResolvedVisualCodeType :: INVisualCodeType -> IO (Id INVisualCodeTypeResolutionResult)
successWithResolvedVisualCodeType resolvedVisualCodeType =
  do
    cls' <- getRequiredClass "INVisualCodeTypeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedVisualCodeType:") (retPtr retVoid) [argCLong (coerce resolvedVisualCodeType)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithVisualCodeTypeToConfirm:@
confirmationRequiredWithVisualCodeTypeToConfirm :: INVisualCodeType -> IO (Id INVisualCodeTypeResolutionResult)
confirmationRequiredWithVisualCodeTypeToConfirm visualCodeTypeToConfirm =
  do
    cls' <- getRequiredClass "INVisualCodeTypeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithVisualCodeTypeToConfirm:") (retPtr retVoid) [argCLong (coerce visualCodeTypeToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedVisualCodeType:@
successWithResolvedVisualCodeTypeSelector :: Selector
successWithResolvedVisualCodeTypeSelector = mkSelector "successWithResolvedVisualCodeType:"

-- | @Selector@ for @confirmationRequiredWithVisualCodeTypeToConfirm:@
confirmationRequiredWithVisualCodeTypeToConfirmSelector :: Selector
confirmationRequiredWithVisualCodeTypeToConfirmSelector = mkSelector "confirmationRequiredWithVisualCodeTypeToConfirm:"

