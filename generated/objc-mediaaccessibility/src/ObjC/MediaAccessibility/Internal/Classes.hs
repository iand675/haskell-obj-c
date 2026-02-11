{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.MediaAccessibility.Internal.Classes (
    module ObjC.MediaAccessibility.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- MAFlashingLightsProcessor ----------

-- | Phantom type for @MAFlashingLightsProcessor@.
data MAFlashingLightsProcessor

instance IsObjCObject (Id MAFlashingLightsProcessor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MAFlashingLightsProcessor"

class IsNSObject a => IsMAFlashingLightsProcessor a where
  toMAFlashingLightsProcessor :: a -> Id MAFlashingLightsProcessor

instance IsMAFlashingLightsProcessor (Id MAFlashingLightsProcessor) where
  toMAFlashingLightsProcessor = unsafeCastId

instance IsNSObject (Id MAFlashingLightsProcessor) where
  toNSObject = unsafeCastId

-- ---------- MAFlashingLightsProcessorResult ----------

-- | Phantom type for @MAFlashingLightsProcessorResult@.
data MAFlashingLightsProcessorResult

instance IsObjCObject (Id MAFlashingLightsProcessorResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MAFlashingLightsProcessorResult"

class IsNSObject a => IsMAFlashingLightsProcessorResult a where
  toMAFlashingLightsProcessorResult :: a -> Id MAFlashingLightsProcessorResult

instance IsMAFlashingLightsProcessorResult (Id MAFlashingLightsProcessorResult) where
  toMAFlashingLightsProcessorResult = unsafeCastId

instance IsNSObject (Id MAFlashingLightsProcessorResult) where
  toNSObject = unsafeCastId

-- ---------- MAMusicHapticsManager ----------

-- | Phantom type for @MAMusicHapticsManager@.
data MAMusicHapticsManager

instance IsObjCObject (Id MAMusicHapticsManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MAMusicHapticsManager"

class IsNSObject a => IsMAMusicHapticsManager a where
  toMAMusicHapticsManager :: a -> Id MAMusicHapticsManager

instance IsMAMusicHapticsManager (Id MAMusicHapticsManager) where
  toMAMusicHapticsManager = unsafeCastId

instance IsNSObject (Id MAMusicHapticsManager) where
  toNSObject = unsafeCastId
