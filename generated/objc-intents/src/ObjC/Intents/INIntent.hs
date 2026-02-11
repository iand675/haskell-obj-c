{-# LANGUAGE PatternSynonyms #-}
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
  , setImage_forParameterNamedSelector
  , imageForParameterNamedSelector
  , keyImageSelector
  , identifierSelector
  , intentDescriptionSelector
  , suggestedInvocationPhraseSelector
  , setSuggestedInvocationPhraseSelector
  , shortcutAvailabilitySelector
  , setShortcutAvailabilitySelector
  , donationMetadataSelector
  , setDonationMetadataSelector

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

-- | @- setImage:forParameterNamed:@
setImage_forParameterNamed :: (IsINIntent inIntent, IsINImage image, IsNSString parameterName) => inIntent -> image -> parameterName -> IO ()
setImage_forParameterNamed inIntent  image parameterName =
withObjCPtr image $ \raw_image ->
  withObjCPtr parameterName $ \raw_parameterName ->
      sendMsg inIntent (mkSelector "setImage:forParameterNamed:") retVoid [argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_parameterName :: Ptr ())]

-- | @- imageForParameterNamed:@
imageForParameterNamed :: (IsINIntent inIntent, IsNSString parameterName) => inIntent -> parameterName -> IO (Id INImage)
imageForParameterNamed inIntent  parameterName =
withObjCPtr parameterName $ \raw_parameterName ->
    sendMsg inIntent (mkSelector "imageForParameterNamed:") (retPtr retVoid) [argPtr (castPtr raw_parameterName :: Ptr ())] >>= retainedObject . castPtr

-- | @- keyImage@
keyImage :: IsINIntent inIntent => inIntent -> IO (Id INImage)
keyImage inIntent  =
  sendMsg inIntent (mkSelector "keyImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- identifier@
identifier :: IsINIntent inIntent => inIntent -> IO (Id NSString)
identifier inIntent  =
  sendMsg inIntent (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- intentDescription@
intentDescription :: IsINIntent inIntent => inIntent -> IO (Id NSString)
intentDescription inIntent  =
  sendMsg inIntent (mkSelector "intentDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- suggestedInvocationPhrase@
suggestedInvocationPhrase :: IsINIntent inIntent => inIntent -> IO (Id NSString)
suggestedInvocationPhrase inIntent  =
  sendMsg inIntent (mkSelector "suggestedInvocationPhrase") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSuggestedInvocationPhrase:@
setSuggestedInvocationPhrase :: (IsINIntent inIntent, IsNSString value) => inIntent -> value -> IO ()
setSuggestedInvocationPhrase inIntent  value =
withObjCPtr value $ \raw_value ->
    sendMsg inIntent (mkSelector "setSuggestedInvocationPhrase:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- shortcutAvailability@
shortcutAvailability :: IsINIntent inIntent => inIntent -> IO INShortcutAvailabilityOptions
shortcutAvailability inIntent  =
  fmap (coerce :: CULong -> INShortcutAvailabilityOptions) $ sendMsg inIntent (mkSelector "shortcutAvailability") retCULong []

-- | @- setShortcutAvailability:@
setShortcutAvailability :: IsINIntent inIntent => inIntent -> INShortcutAvailabilityOptions -> IO ()
setShortcutAvailability inIntent  value =
  sendMsg inIntent (mkSelector "setShortcutAvailability:") retVoid [argCULong (coerce value)]

-- | @- donationMetadata@
donationMetadata :: IsINIntent inIntent => inIntent -> IO (Id INIntentDonationMetadata)
donationMetadata inIntent  =
  sendMsg inIntent (mkSelector "donationMetadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDonationMetadata:@
setDonationMetadata :: (IsINIntent inIntent, IsINIntentDonationMetadata value) => inIntent -> value -> IO ()
setDonationMetadata inIntent  value =
withObjCPtr value $ \raw_value ->
    sendMsg inIntent (mkSelector "setDonationMetadata:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setImage:forParameterNamed:@
setImage_forParameterNamedSelector :: Selector
setImage_forParameterNamedSelector = mkSelector "setImage:forParameterNamed:"

-- | @Selector@ for @imageForParameterNamed:@
imageForParameterNamedSelector :: Selector
imageForParameterNamedSelector = mkSelector "imageForParameterNamed:"

-- | @Selector@ for @keyImage@
keyImageSelector :: Selector
keyImageSelector = mkSelector "keyImage"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @intentDescription@
intentDescriptionSelector :: Selector
intentDescriptionSelector = mkSelector "intentDescription"

-- | @Selector@ for @suggestedInvocationPhrase@
suggestedInvocationPhraseSelector :: Selector
suggestedInvocationPhraseSelector = mkSelector "suggestedInvocationPhrase"

-- | @Selector@ for @setSuggestedInvocationPhrase:@
setSuggestedInvocationPhraseSelector :: Selector
setSuggestedInvocationPhraseSelector = mkSelector "setSuggestedInvocationPhrase:"

-- | @Selector@ for @shortcutAvailability@
shortcutAvailabilitySelector :: Selector
shortcutAvailabilitySelector = mkSelector "shortcutAvailability"

-- | @Selector@ for @setShortcutAvailability:@
setShortcutAvailabilitySelector :: Selector
setShortcutAvailabilitySelector = mkSelector "setShortcutAvailability:"

-- | @Selector@ for @donationMetadata@
donationMetadataSelector :: Selector
donationMetadataSelector = mkSelector "donationMetadata"

-- | @Selector@ for @setDonationMetadata:@
setDonationMetadataSelector :: Selector
setDonationMetadataSelector = mkSelector "setDonationMetadata:"

