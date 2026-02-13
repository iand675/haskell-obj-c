{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStartAudioCallIntent@.
module ObjC.Intents.INStartAudioCallIntent
  ( INStartAudioCallIntent
  , IsINStartAudioCallIntent(..)
  , initWithDestinationType_contacts
  , initWithContacts
  , destinationType
  , contacts
  , contactsSelector
  , destinationTypeSelector
  , initWithContactsSelector
  , initWithDestinationType_contactsSelector

  -- * Enum types
  , INCallDestinationType(INCallDestinationType)
  , pattern INCallDestinationTypeUnknown
  , pattern INCallDestinationTypeNormal
  , pattern INCallDestinationTypeEmergency
  , pattern INCallDestinationTypeVoicemail
  , pattern INCallDestinationTypeRedial
  , pattern INCallDestinationTypeCallBack
  , pattern INCallDestinationTypeNormalDestination
  , pattern INCallDestinationTypeEmergencyDestination
  , pattern INCallDestinationTypeVoicemailDestination
  , pattern INCallDestinationTypeRedialDestination

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

-- | @- initWithDestinationType:contacts:@
initWithDestinationType_contacts :: (IsINStartAudioCallIntent inStartAudioCallIntent, IsNSArray contacts) => inStartAudioCallIntent -> INCallDestinationType -> contacts -> IO (Id INStartAudioCallIntent)
initWithDestinationType_contacts inStartAudioCallIntent destinationType contacts =
  sendOwnedMessage inStartAudioCallIntent initWithDestinationType_contactsSelector destinationType (toNSArray contacts)

-- | @- initWithContacts:@
initWithContacts :: (IsINStartAudioCallIntent inStartAudioCallIntent, IsNSArray contacts) => inStartAudioCallIntent -> contacts -> IO (Id INStartAudioCallIntent)
initWithContacts inStartAudioCallIntent contacts =
  sendOwnedMessage inStartAudioCallIntent initWithContactsSelector (toNSArray contacts)

-- | @- destinationType@
destinationType :: IsINStartAudioCallIntent inStartAudioCallIntent => inStartAudioCallIntent -> IO INCallDestinationType
destinationType inStartAudioCallIntent =
  sendMessage inStartAudioCallIntent destinationTypeSelector

-- | @- contacts@
contacts :: IsINStartAudioCallIntent inStartAudioCallIntent => inStartAudioCallIntent -> IO (Id NSArray)
contacts inStartAudioCallIntent =
  sendMessage inStartAudioCallIntent contactsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDestinationType:contacts:@
initWithDestinationType_contactsSelector :: Selector '[INCallDestinationType, Id NSArray] (Id INStartAudioCallIntent)
initWithDestinationType_contactsSelector = mkSelector "initWithDestinationType:contacts:"

-- | @Selector@ for @initWithContacts:@
initWithContactsSelector :: Selector '[Id NSArray] (Id INStartAudioCallIntent)
initWithContactsSelector = mkSelector "initWithContacts:"

-- | @Selector@ for @destinationType@
destinationTypeSelector :: Selector '[] INCallDestinationType
destinationTypeSelector = mkSelector "destinationType"

-- | @Selector@ for @contacts@
contactsSelector :: Selector '[] (Id NSArray)
contactsSelector = mkSelector "contacts"

