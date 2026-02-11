{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INLocationSearchTypeResolutionResult@.
module ObjC.Intents.INLocationSearchTypeResolutionResult
  ( INLocationSearchTypeResolutionResult
  , IsINLocationSearchTypeResolutionResult(..)
  , successWithResolvedLocationSearchType
  , confirmationRequiredWithLocationSearchTypeToConfirm
  , successWithResolvedLocationSearchTypeSelector
  , confirmationRequiredWithLocationSearchTypeToConfirmSelector

  -- * Enum types
  , INLocationSearchType(INLocationSearchType)
  , pattern INLocationSearchTypeUnknown
  , pattern INLocationSearchTypeByLocationTrigger

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

-- | @+ successWithResolvedLocationSearchType:@
successWithResolvedLocationSearchType :: INLocationSearchType -> IO (Id INLocationSearchTypeResolutionResult)
successWithResolvedLocationSearchType resolvedLocationSearchType =
  do
    cls' <- getRequiredClass "INLocationSearchTypeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedLocationSearchType:") (retPtr retVoid) [argCLong (coerce resolvedLocationSearchType)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithLocationSearchTypeToConfirm:@
confirmationRequiredWithLocationSearchTypeToConfirm :: INLocationSearchType -> IO (Id INLocationSearchTypeResolutionResult)
confirmationRequiredWithLocationSearchTypeToConfirm locationSearchTypeToConfirm =
  do
    cls' <- getRequiredClass "INLocationSearchTypeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithLocationSearchTypeToConfirm:") (retPtr retVoid) [argCLong (coerce locationSearchTypeToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedLocationSearchType:@
successWithResolvedLocationSearchTypeSelector :: Selector
successWithResolvedLocationSearchTypeSelector = mkSelector "successWithResolvedLocationSearchType:"

-- | @Selector@ for @confirmationRequiredWithLocationSearchTypeToConfirm:@
confirmationRequiredWithLocationSearchTypeToConfirmSelector :: Selector
confirmationRequiredWithLocationSearchTypeToConfirmSelector = mkSelector "confirmationRequiredWithLocationSearchTypeToConfirm:"

