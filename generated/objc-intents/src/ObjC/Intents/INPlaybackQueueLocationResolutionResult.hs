{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPlaybackQueueLocationResolutionResult@.
module ObjC.Intents.INPlaybackQueueLocationResolutionResult
  ( INPlaybackQueueLocationResolutionResult
  , IsINPlaybackQueueLocationResolutionResult(..)
  , successWithResolvedPlaybackQueueLocation
  , confirmationRequiredWithPlaybackQueueLocationToConfirm
  , successWithResolvedPlaybackQueueLocationSelector
  , confirmationRequiredWithPlaybackQueueLocationToConfirmSelector

  -- * Enum types
  , INPlaybackQueueLocation(INPlaybackQueueLocation)
  , pattern INPlaybackQueueLocationUnknown
  , pattern INPlaybackQueueLocationNow
  , pattern INPlaybackQueueLocationNext
  , pattern INPlaybackQueueLocationLater

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

-- | @+ successWithResolvedPlaybackQueueLocation:@
successWithResolvedPlaybackQueueLocation :: INPlaybackQueueLocation -> IO (Id INPlaybackQueueLocationResolutionResult)
successWithResolvedPlaybackQueueLocation resolvedPlaybackQueueLocation =
  do
    cls' <- getRequiredClass "INPlaybackQueueLocationResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedPlaybackQueueLocation:") (retPtr retVoid) [argCLong (coerce resolvedPlaybackQueueLocation)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithPlaybackQueueLocationToConfirm:@
confirmationRequiredWithPlaybackQueueLocationToConfirm :: INPlaybackQueueLocation -> IO (Id INPlaybackQueueLocationResolutionResult)
confirmationRequiredWithPlaybackQueueLocationToConfirm playbackQueueLocationToConfirm =
  do
    cls' <- getRequiredClass "INPlaybackQueueLocationResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithPlaybackQueueLocationToConfirm:") (retPtr retVoid) [argCLong (coerce playbackQueueLocationToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedPlaybackQueueLocation:@
successWithResolvedPlaybackQueueLocationSelector :: Selector
successWithResolvedPlaybackQueueLocationSelector = mkSelector "successWithResolvedPlaybackQueueLocation:"

-- | @Selector@ for @confirmationRequiredWithPlaybackQueueLocationToConfirm:@
confirmationRequiredWithPlaybackQueueLocationToConfirmSelector :: Selector
confirmationRequiredWithPlaybackQueueLocationToConfirmSelector = mkSelector "confirmationRequiredWithPlaybackQueueLocationToConfirm:"

