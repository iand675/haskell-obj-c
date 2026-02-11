{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPlaybackRepeatModeResolutionResult@.
module ObjC.Intents.INPlaybackRepeatModeResolutionResult
  ( INPlaybackRepeatModeResolutionResult
  , IsINPlaybackRepeatModeResolutionResult(..)
  , successWithResolvedPlaybackRepeatMode
  , confirmationRequiredWithPlaybackRepeatModeToConfirm
  , successWithResolvedPlaybackRepeatModeSelector
  , confirmationRequiredWithPlaybackRepeatModeToConfirmSelector

  -- * Enum types
  , INPlaybackRepeatMode(INPlaybackRepeatMode)
  , pattern INPlaybackRepeatModeUnknown
  , pattern INPlaybackRepeatModeNone
  , pattern INPlaybackRepeatModeAll
  , pattern INPlaybackRepeatModeOne

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

-- | @+ successWithResolvedPlaybackRepeatMode:@
successWithResolvedPlaybackRepeatMode :: INPlaybackRepeatMode -> IO (Id INPlaybackRepeatModeResolutionResult)
successWithResolvedPlaybackRepeatMode resolvedPlaybackRepeatMode =
  do
    cls' <- getRequiredClass "INPlaybackRepeatModeResolutionResult"
    sendClassMsg cls' (mkSelector "successWithResolvedPlaybackRepeatMode:") (retPtr retVoid) [argCLong (coerce resolvedPlaybackRepeatMode)] >>= retainedObject . castPtr

-- | @+ confirmationRequiredWithPlaybackRepeatModeToConfirm:@
confirmationRequiredWithPlaybackRepeatModeToConfirm :: INPlaybackRepeatMode -> IO (Id INPlaybackRepeatModeResolutionResult)
confirmationRequiredWithPlaybackRepeatModeToConfirm playbackRepeatModeToConfirm =
  do
    cls' <- getRequiredClass "INPlaybackRepeatModeResolutionResult"
    sendClassMsg cls' (mkSelector "confirmationRequiredWithPlaybackRepeatModeToConfirm:") (retPtr retVoid) [argCLong (coerce playbackRepeatModeToConfirm)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @successWithResolvedPlaybackRepeatMode:@
successWithResolvedPlaybackRepeatModeSelector :: Selector
successWithResolvedPlaybackRepeatModeSelector = mkSelector "successWithResolvedPlaybackRepeatMode:"

-- | @Selector@ for @confirmationRequiredWithPlaybackRepeatModeToConfirm:@
confirmationRequiredWithPlaybackRepeatModeToConfirmSelector :: Selector
confirmationRequiredWithPlaybackRepeatModeToConfirmSelector = mkSelector "confirmationRequiredWithPlaybackRepeatModeToConfirm:"

