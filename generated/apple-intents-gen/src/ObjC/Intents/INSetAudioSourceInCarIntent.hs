{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetAudioSourceInCarIntent@.
module ObjC.Intents.INSetAudioSourceInCarIntent
  ( INSetAudioSourceInCarIntent
  , IsINSetAudioSourceInCarIntent(..)
  , initWithAudioSource_relativeAudioSourceReference
  , audioSource
  , relativeAudioSourceReference
  , audioSourceSelector
  , initWithAudioSource_relativeAudioSourceReferenceSelector
  , relativeAudioSourceReferenceSelector

  -- * Enum types
  , INCarAudioSource(INCarAudioSource)
  , pattern INCarAudioSourceUnknown
  , pattern INCarAudioSourceCarPlay
  , pattern INCarAudioSourceiPod
  , pattern INCarAudioSourceRadio
  , pattern INCarAudioSourceBluetooth
  , pattern INCarAudioSourceAUX
  , pattern INCarAudioSourceUSB
  , pattern INCarAudioSourceMemoryCard
  , pattern INCarAudioSourceOpticalDrive
  , pattern INCarAudioSourceHardDrive
  , INRelativeReference(INRelativeReference)
  , pattern INRelativeReferenceUnknown
  , pattern INRelativeReferenceNext
  , pattern INRelativeReferencePrevious

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

-- | @- initWithAudioSource:relativeAudioSourceReference:@
initWithAudioSource_relativeAudioSourceReference :: IsINSetAudioSourceInCarIntent inSetAudioSourceInCarIntent => inSetAudioSourceInCarIntent -> INCarAudioSource -> INRelativeReference -> IO (Id INSetAudioSourceInCarIntent)
initWithAudioSource_relativeAudioSourceReference inSetAudioSourceInCarIntent audioSource relativeAudioSourceReference =
  sendOwnedMessage inSetAudioSourceInCarIntent initWithAudioSource_relativeAudioSourceReferenceSelector audioSource relativeAudioSourceReference

-- | @- audioSource@
audioSource :: IsINSetAudioSourceInCarIntent inSetAudioSourceInCarIntent => inSetAudioSourceInCarIntent -> IO INCarAudioSource
audioSource inSetAudioSourceInCarIntent =
  sendMessage inSetAudioSourceInCarIntent audioSourceSelector

-- | @- relativeAudioSourceReference@
relativeAudioSourceReference :: IsINSetAudioSourceInCarIntent inSetAudioSourceInCarIntent => inSetAudioSourceInCarIntent -> IO INRelativeReference
relativeAudioSourceReference inSetAudioSourceInCarIntent =
  sendMessage inSetAudioSourceInCarIntent relativeAudioSourceReferenceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAudioSource:relativeAudioSourceReference:@
initWithAudioSource_relativeAudioSourceReferenceSelector :: Selector '[INCarAudioSource, INRelativeReference] (Id INSetAudioSourceInCarIntent)
initWithAudioSource_relativeAudioSourceReferenceSelector = mkSelector "initWithAudioSource:relativeAudioSourceReference:"

-- | @Selector@ for @audioSource@
audioSourceSelector :: Selector '[] INCarAudioSource
audioSourceSelector = mkSelector "audioSource"

-- | @Selector@ for @relativeAudioSourceReference@
relativeAudioSourceReferenceSelector :: Selector '[] INRelativeReference
relativeAudioSourceReferenceSelector = mkSelector "relativeAudioSourceReference"

