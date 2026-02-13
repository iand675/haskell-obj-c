{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPlayMediaPlaybackSpeedResolutionResult@.
module ObjC.Intents.INPlayMediaPlaybackSpeedResolutionResult
  ( INPlayMediaPlaybackSpeedResolutionResult
  , IsINPlayMediaPlaybackSpeedResolutionResult(..)
  , unsupportedForReason
  , initWithDoubleResolutionResult
  , initWithDoubleResolutionResultSelector
  , unsupportedForReasonSelector

  -- * Enum types
  , INPlayMediaPlaybackSpeedUnsupportedReason(INPlayMediaPlaybackSpeedUnsupportedReason)
  , pattern INPlayMediaPlaybackSpeedUnsupportedReasonBelowMinimum
  , pattern INPlayMediaPlaybackSpeedUnsupportedReasonAboveMaximum

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
unsupportedForReason :: INPlayMediaPlaybackSpeedUnsupportedReason -> IO (Id INPlayMediaPlaybackSpeedResolutionResult)
unsupportedForReason reason =
  do
    cls' <- getRequiredClass "INPlayMediaPlaybackSpeedResolutionResult"
    sendClassMessage cls' unsupportedForReasonSelector reason

-- | @- initWithDoubleResolutionResult:@
initWithDoubleResolutionResult :: (IsINPlayMediaPlaybackSpeedResolutionResult inPlayMediaPlaybackSpeedResolutionResult, IsINDoubleResolutionResult doubleResolutionResult) => inPlayMediaPlaybackSpeedResolutionResult -> doubleResolutionResult -> IO (Id INPlayMediaPlaybackSpeedResolutionResult)
initWithDoubleResolutionResult inPlayMediaPlaybackSpeedResolutionResult doubleResolutionResult =
  sendOwnedMessage inPlayMediaPlaybackSpeedResolutionResult initWithDoubleResolutionResultSelector (toINDoubleResolutionResult doubleResolutionResult)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unsupportedForReason:@
unsupportedForReasonSelector :: Selector '[INPlayMediaPlaybackSpeedUnsupportedReason] (Id INPlayMediaPlaybackSpeedResolutionResult)
unsupportedForReasonSelector = mkSelector "unsupportedForReason:"

-- | @Selector@ for @initWithDoubleResolutionResult:@
initWithDoubleResolutionResultSelector :: Selector '[Id INDoubleResolutionResult] (Id INPlayMediaPlaybackSpeedResolutionResult)
initWithDoubleResolutionResultSelector = mkSelector "initWithDoubleResolutionResult:"

