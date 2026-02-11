{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INDateSearchTypeResolutionResult@.
module ObjC.Intents.INDateSearchTypeResolutionResult
  ( INDateSearchTypeResolutionResult
  , IsINDateSearchTypeResolutionResult(..)
  , successWithResolvedDateSearchType
  , confirmationRequiredWithDateSearchTypeToConfirm
  , successWithResolvedDateSearchTypeSelector
  , confirmationRequiredWithDateSearchTypeToConfirmSelector

  -- * Enum types
  , INDateSearchType(INDateSearchType)
  , pattern INDateSearchTypeUnknown
  , pattern INDateSearchTypeByDueDate
  , pattern INDateSearchTypeByModifiedDate
  , pattern INDateSearchTypeByCreatedDate

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

-- | @+ successWithResolvedDateSearchType:@
successWithResolvedDateSearchType :: INDateSearchType -> IO (Id INDateSearchTypeResolutionResult)
successWithResolvedDateSearchType resolvedDateSearchType =
  do
    cls' <- getRequiredClass "INDateSearchTypeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedDateSearchType:") (retPtr retVoid) [argCLong (coerce resolvedDateSearchType)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithDateSearchTypeToConfirm:@
confirmationRequiredWithDateSearchTypeToConfirm :: INDateSearchType -> IO (Id INDateSearchTypeResolutionResult)
confirmationRequiredWithDateSearchTypeToConfirm dateSearchTypeToConfirm =
  do
    cls' <- getRequiredClass "INDateSearchTypeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithDateSearchTypeToConfirm:") (retPtr retVoid) [argCLong (coerce dateSearchTypeToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedDateSearchType:@
successWithResolvedDateSearchTypeSelector :: Selector
successWithResolvedDateSearchTypeSelector = mkSelector "successWithResolvedDateSearchType:"

-- | @Selector@ for @confirmationRequiredWithDateSearchTypeToConfirm:@
confirmationRequiredWithDateSearchTypeToConfirmSelector :: Selector
confirmationRequiredWithDateSearchTypeToConfirmSelector = mkSelector "confirmationRequiredWithDateSearchTypeToConfirm:"

