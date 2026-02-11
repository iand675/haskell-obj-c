{-# LANGUAGE PatternSynonyms #-}
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
  , initWithAudioSource_relativeAudioSourceReferenceSelector
  , audioSourceSelector
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

-- | @- initWithAudioSource:relativeAudioSourceReference:@
initWithAudioSource_relativeAudioSourceReference :: IsINSetAudioSourceInCarIntent inSetAudioSourceInCarIntent => inSetAudioSourceInCarIntent -> INCarAudioSource -> INRelativeReference -> IO (Id INSetAudioSourceInCarIntent)
initWithAudioSource_relativeAudioSourceReference inSetAudioSourceInCarIntent  audioSource relativeAudioSourceReference =
  sendMsg inSetAudioSourceInCarIntent (mkSelector "initWithAudioSource:relativeAudioSourceReference:") (retPtr retVoid) [argCLong (coerce audioSource), argCLong (coerce relativeAudioSourceReference)] >>= ownedObject . castPtr

-- | @- audioSource@
audioSource :: IsINSetAudioSourceInCarIntent inSetAudioSourceInCarIntent => inSetAudioSourceInCarIntent -> IO INCarAudioSource
audioSource inSetAudioSourceInCarIntent  =
  fmap (coerce :: CLong -> INCarAudioSource) $ sendMsg inSetAudioSourceInCarIntent (mkSelector "audioSource") retCLong []

-- | @- relativeAudioSourceReference@
relativeAudioSourceReference :: IsINSetAudioSourceInCarIntent inSetAudioSourceInCarIntent => inSetAudioSourceInCarIntent -> IO INRelativeReference
relativeAudioSourceReference inSetAudioSourceInCarIntent  =
  fmap (coerce :: CLong -> INRelativeReference) $ sendMsg inSetAudioSourceInCarIntent (mkSelector "relativeAudioSourceReference") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAudioSource:relativeAudioSourceReference:@
initWithAudioSource_relativeAudioSourceReferenceSelector :: Selector
initWithAudioSource_relativeAudioSourceReferenceSelector = mkSelector "initWithAudioSource:relativeAudioSourceReference:"

-- | @Selector@ for @audioSource@
audioSourceSelector :: Selector
audioSourceSelector = mkSelector "audioSource"

-- | @Selector@ for @relativeAudioSourceReference@
relativeAudioSourceReferenceSelector :: Selector
relativeAudioSourceReferenceSelector = mkSelector "relativeAudioSourceReference"

