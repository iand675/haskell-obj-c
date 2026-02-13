{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureReactionEffectState
--
-- Reports the state of a reaction performed on an AVCaptureDevice.
--
-- AVCaptureReactionEffectState may be obtained by calling -[AVCaptureDevice reactionEffectsInProgress].  When -[AVCaptureDevice canPerformReactionEffects] returns YES, new entries are added either by calling -[AVCaptureDevice performEffectForReaction:], or by gesture detection in the capture stream when AVCaptureDevice.reactionEffectGesturesEnabled.  The effect rendering is done before frames are given to the capture client, and these status objects let you know when these effects are performed.
--
-- Generated bindings for @AVCaptureReactionEffectState@.
module ObjC.AVFoundation.AVCaptureReactionEffectState
  ( AVCaptureReactionEffectState
  , IsAVCaptureReactionEffectState(..)
  , reactionType
  , reactionTypeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | reactionType
--
-- Indicates the reaction which is running.
--
-- There may be multiple reactions of the same type at a given time.  Some may come from gesture detection, some may come from calls to -[AVCaptureDevice performReactionEffect:]
--
-- ObjC selector: @- reactionType@
reactionType :: IsAVCaptureReactionEffectState avCaptureReactionEffectState => avCaptureReactionEffectState -> IO (Id NSString)
reactionType avCaptureReactionEffectState =
  sendMessage avCaptureReactionEffectState reactionTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reactionType@
reactionTypeSelector :: Selector '[] (Id NSString)
reactionTypeSelector = mkSelector "reactionType"

