{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INBalanceTypeResolutionResult@.
module ObjC.Intents.INBalanceTypeResolutionResult
  ( INBalanceTypeResolutionResult
  , IsINBalanceTypeResolutionResult(..)
  , successWithResolvedBalanceType
  , confirmationRequiredWithBalanceTypeToConfirm
  , successWithResolvedBalanceTypeSelector
  , confirmationRequiredWithBalanceTypeToConfirmSelector

  -- * Enum types
  , INBalanceType(INBalanceType)
  , pattern INBalanceTypeUnknown
  , pattern INBalanceTypeMoney
  , pattern INBalanceTypePoints
  , pattern INBalanceTypeMiles

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

-- | @+ successWithResolvedBalanceType:@
successWithResolvedBalanceType :: INBalanceType -> IO (Id INBalanceTypeResolutionResult)
successWithResolvedBalanceType resolvedBalanceType =
  do
    cls' <- getRequiredClass "INBalanceTypeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedBalanceType:") (retPtr retVoid) [argCLong (coerce resolvedBalanceType)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithBalanceTypeToConfirm:@
confirmationRequiredWithBalanceTypeToConfirm :: INBalanceType -> IO (Id INBalanceTypeResolutionResult)
confirmationRequiredWithBalanceTypeToConfirm balanceTypeToConfirm =
  do
    cls' <- getRequiredClass "INBalanceTypeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithBalanceTypeToConfirm:") (retPtr retVoid) [argCLong (coerce balanceTypeToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedBalanceType:@
successWithResolvedBalanceTypeSelector :: Selector
successWithResolvedBalanceTypeSelector = mkSelector "successWithResolvedBalanceType:"

-- | @Selector@ for @confirmationRequiredWithBalanceTypeToConfirm:@
confirmationRequiredWithBalanceTypeToConfirmSelector :: Selector
confirmationRequiredWithBalanceTypeToConfirmSelector = mkSelector "confirmationRequiredWithBalanceTypeToConfirm:"

