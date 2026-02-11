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
reactionType avCaptureReactionEffectState  =
  sendMsg avCaptureReactionEffectState (mkSelector "reactionType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reactionType@
reactionTypeSelector :: Selector
reactionTypeSelector = mkSelector "reactionType"

