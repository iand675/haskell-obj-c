{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUserActivity@.
module ObjC.Intents.NSUserActivity
  ( NSUserActivity
  , IsNSUserActivity(..)
  , interaction
  , suggestedInvocationPhrase
  , setSuggestedInvocationPhrase
  , shortcutAvailability
  , setShortcutAvailability
  , interactionSelector
  , setShortcutAvailabilitySelector
  , setSuggestedInvocationPhraseSelector
  , shortcutAvailabilitySelector
  , suggestedInvocationPhraseSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- interaction@
interaction :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id INInteraction)
interaction nsUserActivity =
  sendMessage nsUserActivity interactionSelector

-- | @- suggestedInvocationPhrase@
suggestedInvocationPhrase :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSString)
suggestedInvocationPhrase nsUserActivity =
  sendMessage nsUserActivity suggestedInvocationPhraseSelector

-- | @- setSuggestedInvocationPhrase:@
setSuggestedInvocationPhrase :: (IsNSUserActivity nsUserActivity, IsNSString value) => nsUserActivity -> value -> IO ()
setSuggestedInvocationPhrase nsUserActivity value =
  sendMessage nsUserActivity setSuggestedInvocationPhraseSelector (toNSString value)

-- | @- shortcutAvailability@
shortcutAvailability :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO INShortcutAvailabilityOptions
shortcutAvailability nsUserActivity =
  sendMessage nsUserActivity shortcutAvailabilitySelector

-- | @- setShortcutAvailability:@
setShortcutAvailability :: IsNSUserActivity nsUserActivity => nsUserActivity -> INShortcutAvailabilityOptions -> IO ()
setShortcutAvailability nsUserActivity value =
  sendMessage nsUserActivity setShortcutAvailabilitySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @interaction@
interactionSelector :: Selector '[] (Id INInteraction)
interactionSelector = mkSelector "interaction"

-- | @Selector@ for @suggestedInvocationPhrase@
suggestedInvocationPhraseSelector :: Selector '[] (Id NSString)
suggestedInvocationPhraseSelector = mkSelector "suggestedInvocationPhrase"

-- | @Selector@ for @setSuggestedInvocationPhrase:@
setSuggestedInvocationPhraseSelector :: Selector '[Id NSString] ()
setSuggestedInvocationPhraseSelector = mkSelector "setSuggestedInvocationPhrase:"

-- | @Selector@ for @shortcutAvailability@
shortcutAvailabilitySelector :: Selector '[] INShortcutAvailabilityOptions
shortcutAvailabilitySelector = mkSelector "shortcutAvailability"

-- | @Selector@ for @setShortcutAvailability:@
setShortcutAvailabilitySelector :: Selector '[INShortcutAvailabilityOptions] ()
setShortcutAvailabilitySelector = mkSelector "setShortcutAvailability:"

