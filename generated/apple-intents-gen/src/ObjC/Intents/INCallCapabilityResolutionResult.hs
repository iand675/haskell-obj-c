{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCallCapabilityResolutionResult@.
module ObjC.Intents.INCallCapabilityResolutionResult
  ( INCallCapabilityResolutionResult
  , IsINCallCapabilityResolutionResult(..)
  , successWithResolvedCallCapability
  , confirmationRequiredWithCallCapabilityToConfirm
  , confirmationRequiredWithCallCapabilityToConfirmSelector
  , successWithResolvedCallCapabilitySelector

  -- * Enum types
  , INCallCapability(INCallCapability)
  , pattern INCallCapabilityUnknown
  , pattern INCallCapabilityAudioCall
  , pattern INCallCapabilityVideoCall

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

-- | @+ successWithResolvedCallCapability:@
successWithResolvedCallCapability :: INCallCapability -> IO (Id INCallCapabilityResolutionResult)
successWithResolvedCallCapability resolvedCallCapability =
  do
    cls' <- getRequiredClass "INCallCapabilityResolutionResult"
    sendClassMessage cls' successWithResolvedCallCapabilitySelector resolvedCallCapability

-- | @+ confirmationRequiredWithCallCapabilityToConfirm:@
confirmationRequiredWithCallCapabilityToConfirm :: INCallCapability -> IO (Id INCallCapabilityResolutionResult)
confirmationRequiredWithCallCapabilityToConfirm callCapabilityToConfirm =
  do
    cls' <- getRequiredClass "INCallCapabilityResolutionResult"
    sendClassMessage cls' confirmationRequiredWithCallCapabilityToConfirmSelector callCapabilityToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedCallCapability:@
successWithResolvedCallCapabilitySelector :: Selector '[INCallCapability] (Id INCallCapabilityResolutionResult)
successWithResolvedCallCapabilitySelector = mkSelector "successWithResolvedCallCapability:"

-- | @Selector@ for @confirmationRequiredWithCallCapabilityToConfirm:@
confirmationRequiredWithCallCapabilityToConfirmSelector :: Selector '[INCallCapability] (Id INCallCapabilityResolutionResult)
confirmationRequiredWithCallCapabilityToConfirmSelector = mkSelector "confirmationRequiredWithCallCapabilityToConfirm:"

