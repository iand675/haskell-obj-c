{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPlaybackRepeatModeResolutionResult@.
module ObjC.Intents.INPlaybackRepeatModeResolutionResult
  ( INPlaybackRepeatModeResolutionResult
  , IsINPlaybackRepeatModeResolutionResult(..)
  , successWithResolvedPlaybackRepeatMode
  , confirmationRequiredWithPlaybackRepeatModeToConfirm
  , confirmationRequiredWithPlaybackRepeatModeToConfirmSelector
  , successWithResolvedPlaybackRepeatModeSelector

  -- * Enum types
  , INPlaybackRepeatMode(INPlaybackRepeatMode)
  , pattern INPlaybackRepeatModeUnknown
  , pattern INPlaybackRepeatModeNone
  , pattern INPlaybackRepeatModeAll
  , pattern INPlaybackRepeatModeOne

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

-- | @+ successWithResolvedPlaybackRepeatMode:@
successWithResolvedPlaybackRepeatMode :: INPlaybackRepeatMode -> IO (Id INPlaybackRepeatModeResolutionResult)
successWithResolvedPlaybackRepeatMode resolvedPlaybackRepeatMode =
  do
    cls' <- getRequiredClass "INPlaybackRepeatModeResolutionResult"
    sendClassMessage cls' successWithResolvedPlaybackRepeatModeSelector resolvedPlaybackRepeatMode

-- | @+ confirmationRequiredWithPlaybackRepeatModeToConfirm:@
confirmationRequiredWithPlaybackRepeatModeToConfirm :: INPlaybackRepeatMode -> IO (Id INPlaybackRepeatModeResolutionResult)
confirmationRequiredWithPlaybackRepeatModeToConfirm playbackRepeatModeToConfirm =
  do
    cls' <- getRequiredClass "INPlaybackRepeatModeResolutionResult"
    sendClassMessage cls' confirmationRequiredWithPlaybackRepeatModeToConfirmSelector playbackRepeatModeToConfirm

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedPlaybackRepeatMode:@
successWithResolvedPlaybackRepeatModeSelector :: Selector '[INPlaybackRepeatMode] (Id INPlaybackRepeatModeResolutionResult)
successWithResolvedPlaybackRepeatModeSelector = mkSelector "successWithResolvedPlaybackRepeatMode:"

-- | @Selector@ for @confirmationRequiredWithPlaybackRepeatModeToConfirm:@
confirmationRequiredWithPlaybackRepeatModeToConfirmSelector :: Selector '[INPlaybackRepeatMode] (Id INPlaybackRepeatModeResolutionResult)
confirmationRequiredWithPlaybackRepeatModeToConfirmSelector = mkSelector "confirmationRequiredWithPlaybackRepeatModeToConfirm:"

