{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUserActivity@.
module ObjC.Intents.NSUserActivity
  ( NSUserActivity
  , IsNSUserActivity(..)
  , interaction
  , shortcutAvailability
  , setShortcutAvailability
  , interactionSelector
  , shortcutAvailabilitySelector
  , setShortcutAvailabilitySelector

  -- * Enum types
  , INShortcutAvailabilityOptions(INShortcutAvailabilityOptions)
  , pattern INShortcutAvailabilityOptionSleepMindfulness
  , pattern INShortcutAvailabilityOptionSleepJournaling
  , pattern INShortcutAvailabilityOptionSleepMusic
  , pattern INShortcutAvailabilityOptionSleepPodcasts
  , pattern INShortcutAvailabilityOptionSleepReading
  , pattern INShortcutAvailabilityOptionSleepWrapUpYourDay
  , pattern INShortcutAvailabilityOptionSleepYogaAndStretching

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

-- | @- interaction@
interaction :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id INInteraction)
interaction nsUserActivity  =
  sendMsg nsUserActivity (mkSelector "interaction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- shortcutAvailability@
shortcutAvailability :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO INShortcutAvailabilityOptions
shortcutAvailability nsUserActivity  =
  fmap (coerce :: CULong -> INShortcutAvailabilityOptions) $ sendMsg nsUserActivity (mkSelector "shortcutAvailability") retCULong []

-- | @- setShortcutAvailability:@
setShortcutAvailability :: IsNSUserActivity nsUserActivity => nsUserActivity -> INShortcutAvailabilityOptions -> IO ()
setShortcutAvailability nsUserActivity  value =
  sendMsg nsUserActivity (mkSelector "setShortcutAvailability:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @interaction@
interactionSelector :: Selector
interactionSelector = mkSelector "interaction"

-- | @Selector@ for @shortcutAvailability@
shortcutAvailabilitySelector :: Selector
shortcutAvailabilitySelector = mkSelector "shortcutAvailability"

-- | @Selector@ for @setShortcutAvailability:@
setShortcutAvailabilitySelector :: Selector
setShortcutAvailabilitySelector = mkSelector "setShortcutAvailability:"

