{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPlaybackQueueLocationResolutionResult@.
module ObjC.Intents.INPlaybackQueueLocationResolutionResult
  ( INPlaybackQueueLocationResolutionResult
  , IsINPlaybackQueueLocationResolutionResult(..)
  , successWithResolvedPlaybackQueueLocation
  , confirmationRequiredWithPlaybackQueueLocationToConfirm
  , confirmationRequiredWithPlaybackQueueLocationToConfirmSelector
  , successWithResolvedPlaybackQueueLocationSelector

  -- * Enum types
  , INPlaybackQueueLocation(INPlaybackQueueLocation)
  , pattern INPlaybackQueueLocationUnknown
  , pattern INPlaybackQueueLocationNow
  , pattern INPlaybackQueueLocationNext
  , pattern INPlaybackQueueLocationLater

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

-- | @+ successWithResolvedPlaybackQueueLocation:@
successWithResolvedPlaybackQueueLocation :: INPlaybackQueueLocation -> IO (Id INPlaybackQueueLocationResolutionResult)
successWithResolvedPlaybackQueueLocation resolvedPlaybackQueueLocation =
  do
    cls' <- getRequiredClass "INPlaybackQueueLocationResolutionResult"
    sendClassMessage cls' successWithResolvedPlaybackQueueLocationSelector resolvedPlaybackQueueLocation

-- | @+ confirmationRequiredWithPlaybackQueueLocationToConfirm:@
confirmationRequiredWithPlaybackQueueLocationToConfirm :: INPlaybackQueueLocation -> IO (Id INPlaybackQueueLocationResolutionResult)
confirmationRequiredWithPlaybackQueueLocationToConfirm playbackQueueLocationToConfirm =
  do
    cls' <- getRequiredClass "INPlaybackQueueLocationResolutionResult"
    sendClassMessage cls' confirmationRequiredWithPlaybackQueueLocationToConfirmSelector playbackQueueLocationToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedPlaybackQueueLocation:@
successWithResolvedPlaybackQueueLocationSelector :: Selector '[INPlaybackQueueLocation] (Id INPlaybackQueueLocationResolutionResult)
successWithResolvedPlaybackQueueLocationSelector = mkSelector "successWithResolvedPlaybackQueueLocation:"

-- | @Selector@ for @confirmationRequiredWithPlaybackQueueLocationToConfirm:@
confirmationRequiredWithPlaybackQueueLocationToConfirmSelector :: Selector '[INPlaybackQueueLocation] (Id INPlaybackQueueLocationResolutionResult)
confirmationRequiredWithPlaybackQueueLocationToConfirmSelector = mkSelector "confirmationRequiredWithPlaybackQueueLocationToConfirm:"

