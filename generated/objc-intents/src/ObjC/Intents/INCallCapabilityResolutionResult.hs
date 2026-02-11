{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCallCapabilityResolutionResult@.
module ObjC.Intents.INCallCapabilityResolutionResult
  ( INCallCapabilityResolutionResult
  , IsINCallCapabilityResolutionResult(..)
  , successWithResolvedCallCapability
  , confirmationRequiredWithCallCapabilityToConfirm
  , successWithResolvedCallCapabilitySelector
  , confirmationRequiredWithCallCapabilityToConfirmSelector

  -- * Enum types
  , INCallCapability(INCallCapability)
  , pattern INCallCapabilityUnknown
  , pattern INCallCapabilityAudioCall
  , pattern INCallCapabilityVideoCall

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

-- | @+ successWithResolvedCallCapability:@
successWithResolvedCallCapability :: INCallCapability -> IO (Id INCallCapabilityResolutionResult)
successWithResolvedCallCapability resolvedCallCapability =
  do
    cls' <- getRequiredClass "INCallCapabilityResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedCallCapability:") (retPtr retVoid) [argCLong (coerce resolvedCallCapability)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithCallCapabilityToConfirm:@
confirmationRequiredWithCallCapabilityToConfirm :: INCallCapability -> IO (Id INCallCapabilityResolutionResult)
confirmationRequiredWithCallCapabilityToConfirm callCapabilityToConfirm =
  do
    cls' <- getRequiredClass "INCallCapabilityResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithCallCapabilityToConfirm:") (retPtr retVoid) [argCLong (coerce callCapabilityToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedCallCapability:@
successWithResolvedCallCapabilitySelector :: Selector
successWithResolvedCallCapabilitySelector = mkSelector "successWithResolvedCallCapability:"

-- | @Selector@ for @confirmationRequiredWithCallCapabilityToConfirm:@
confirmationRequiredWithCallCapabilityToConfirmSelector :: Selector
confirmationRequiredWithCallCapabilityToConfirmSelector = mkSelector "confirmationRequiredWithCallCapabilityToConfirm:"

