{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStartCallCallCapabilityResolutionResult@.
module ObjC.Intents.INStartCallCallCapabilityResolutionResult
  ( INStartCallCallCapabilityResolutionResult
  , IsINStartCallCallCapabilityResolutionResult(..)
  , unsupportedForReason
  , initWithCallCapabilityResolutionResult
  , initWithCallCapabilityResolutionResultSelector
  , unsupportedForReasonSelector

  -- * Enum types
  , INStartCallCallCapabilityUnsupportedReason(INStartCallCallCapabilityUnsupportedReason)
  , pattern INStartCallCallCapabilityUnsupportedReasonVideoCallUnsupported
  , pattern INStartCallCallCapabilityUnsupportedReasonMicrophoneNotAccessible
  , pattern INStartCallCallCapabilityUnsupportedReasonCameraNotAccessible

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

-- | @+ unsupportedForReason:@
unsupportedForReason :: INStartCallCallCapabilityUnsupportedReason -> IO (Id INStartCallCallCapabilityResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INStartCallCallCapabilityResolutionResult"
    sendClassMessage cls' unsupportedForReasonSelector reason

-- | @- initWithCallCapabilityResolutionResult:@
initWithCallCapabilityResolutionResult :: (IsINStartCallCallCapabilityResolutionResult inStartCallCallCapabilityResolutionResult, IsINCallCapabilityResolutionResult callCapabilityResolutionResult) => inStartCallCallCapabilityResolutionResult -> callCapabilityResolutionResult -> IO (Id INStartCallCallCapabilityResolutionResult)
initWithCallCapabilityResolutionResult inStartCallCallCapabilityResolutionResult callCapabilityResolutionResult =
  sendOwnedMessage inStartCallCallCapabilityResolutionResult initWithCallCapabilityResolutionResultSelector (toINCallCapabilityResolutionResult callCapabilityResolutionResult)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector '[INStartCallCallCapabilityUnsupportedReason] (Id INStartCallCallCapabilityResolutionResult)
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithCallCapabilityResolutionResult:@
initWithCallCapabilityResolutionResultSelector :: Selector '[Id INCallCapabilityResolutionResult] (Id INStartCallCallCapabilityResolutionResult)
initWithCallCapabilityResolutionResultSelector = mkSelector "initWithCallCapabilityResolutionResult:"

