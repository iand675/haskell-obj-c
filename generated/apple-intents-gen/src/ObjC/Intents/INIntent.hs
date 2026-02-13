{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INIntent@.
module ObjC.Intents.INIntent
  ( INIntent
  , IsINIntent(..)
  , setImage_forParameterNamed
  , imageForParameterNamed
  , keyImage
  , identifier
  , intentDescription
  , suggestedInvocationPhrase
  , setSuggestedInvocationPhrase
  , shortcutAvailability
  , setShortcutAvailability
  , donationMetadata
  , setDonationMetadata
  , donationMetadataSelector
  , identifierSelector
  , imageForParameterNamedSelector
  , intentDescriptionSelector
  , keyImageSelector
  , setDonationMetadataSelector
  , setImage_forParameterNamedSelector
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

-- | @- setImage:forParameterNamed:@
setImage_forParameterNamed :: (IsINIntent inIntent, IsINImage image, IsNSString parameterName) => inIntent -> image -> parameterName -> IO ()
setImage_forParameterNamed inIntent image parameterName =
  sendMessage inIntent setImage_forParameterNamedSelector (toINImage image) (toNSString parameterName)

-- | @- imageForParameterNamed:@
imageForParameterNamed :: (IsINIntent inIntent, IsNSString parameterName) => inIntent -> parameterName -> IO (Id INImage)
imageForParameterNamed inIntent parameterName =
  sendMessage inIntent imageForParameterNamedSelector (toNSString parameterName)

-- | @- keyImage@
keyImage :: IsINIntent inIntent => inIntent -> IO (Id INImage)
keyImage inIntent =
  sendMessage inIntent keyImageSelector

-- | @- identifier@
identifier :: IsINIntent inIntent => inIntent -> IO (Id NSString)
identifier inIntent =
  sendMessage inIntent identifierSelector

-- | @- intentDescription@
intentDescription :: IsINIntent inIntent => inIntent -> IO (Id NSString)
intentDescription inIntent =
  sendMessage inIntent intentDescriptionSelector

-- | @- suggestedInvocationPhrase@
suggestedInvocationPhrase :: IsINIntent inIntent => inIntent -> IO (Id NSString)
suggestedInvocationPhrase inIntent =
  sendMessage inIntent suggestedInvocationPhraseSelector

-- | @- setSuggestedInvocationPhrase:@
setSuggestedInvocationPhrase :: (IsINIntent inIntent, IsNSString value) => inIntent -> value -> IO ()
setSuggestedInvocationPhrase inIntent value =
  sendMessage inIntent setSuggestedInvocationPhraseSelector (toNSString value)

-- | @- shortcutAvailability@
shortcutAvailability :: IsINIntent inIntent => inIntent -> IO INShortcutAvailabilityOptions
shortcutAvailability inIntent =
  sendMessage inIntent shortcutAvailabilitySelector

-- | @- setShortcutAvailability:@
setShortcutAvailability :: IsINIntent inIntent => inIntent -> INShortcutAvailabilityOptions -> IO ()
setShortcutAvailability inIntent value =
  sendMessage inIntent setShortcutAvailabilitySelector value

-- | @- donationMetadata@
donationMetadata :: IsINIntent inIntent => inIntent -> IO (Id INIntentDonationMetadata)
donationMetadata inIntent =
  sendMessage inIntent donationMetadataSelector

-- | @- setDonationMetadata:@
setDonationMetadata :: (IsINIntent inIntent, IsINIntentDonationMetadata value) => inIntent -> value -> IO ()
setDonationMetadata inIntent value =
  sendMessage inIntent setDonationMetadataSelector (toINIntentDonationMetadata value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setImage:forParameterNamed:@
setImage_forParameterNamedSelector :: Selector '[Id INImage, Id NSString] ()
setImage_forParameterNamedSelector = mkSelector "setImage:forParameterNamed:"

-- | @Selector@ for @imageForParameterNamed:@
imageForParameterNamedSelector :: Selector '[Id NSString] (Id INImage)
imageForParameterNamedSelector = mkSelector "imageForParameterNamed:"

-- | @Selector@ for @keyImage@
keyImageSelector :: Selector '[] (Id INImage)
keyImageSelector = mkSelector "keyImage"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @intentDescription@
intentDescriptionSelector :: Selector '[] (Id NSString)
intentDescriptionSelector = mkSelector "intentDescription"

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

-- | @Selector@ for @donationMetadata@
donationMetadataSelector :: Selector '[] (Id INIntentDonationMetadata)
donationMetadataSelector = mkSelector "donationMetadata"

-- | @Selector@ for @setDonationMetadata:@
setDonationMetadataSelector :: Selector '[Id INIntentDonationMetadata] ()
setDonationMetadataSelector = mkSelector "setDonationMetadata:"

