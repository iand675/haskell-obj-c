{-# LANGUAGE PatternSynonyms #-}
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
  , initWithDestinationType_contactsSelector
  , initWithContactsSelector
  , destinationTypeSelector
  , contactsSelector

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

-- | @- initWithDestinationType:contacts:@
initWithDestinationType_contacts :: (IsINStartAudioCallIntent inStartAudioCallIntent, IsNSArray contacts) => inStartAudioCallIntent -> INCallDestinationType -> contacts -> IO (Id INStartAudioCallIntent)
initWithDestinationType_contacts inStartAudioCallIntent  destinationType contacts =
withObjCPtr contacts $ \raw_contacts ->
    sendMsg inStartAudioCallIntent (mkSelector "initWithDestinationType:contacts:") (retPtr retVoid) [argCLong (coerce destinationType), argPtr (castPtr raw_contacts :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContacts:@
initWithContacts :: (IsINStartAudioCallIntent inStartAudioCallIntent, IsNSArray contacts) => inStartAudioCallIntent -> contacts -> IO (Id INStartAudioCallIntent)
initWithContacts inStartAudioCallIntent  contacts =
withObjCPtr contacts $ \raw_contacts ->
    sendMsg inStartAudioCallIntent (mkSelector "initWithContacts:") (retPtr retVoid) [argPtr (castPtr raw_contacts :: Ptr ())] >>= ownedObject . castPtr

-- | @- destinationType@
destinationType :: IsINStartAudioCallIntent inStartAudioCallIntent => inStartAudioCallIntent -> IO INCallDestinationType
destinationType inStartAudioCallIntent  =
  fmap (coerce :: CLong -> INCallDestinationType) $ sendMsg inStartAudioCallIntent (mkSelector "destinationType") retCLong []

-- | @- contacts@
contacts :: IsINStartAudioCallIntent inStartAudioCallIntent => inStartAudioCallIntent -> IO (Id NSArray)
contacts inStartAudioCallIntent  =
  sendMsg inStartAudioCallIntent (mkSelector "contacts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDestinationType:contacts:@
initWithDestinationType_contactsSelector :: Selector
initWithDestinationType_contactsSelector = mkSelector "initWithDestinationType:contacts:"

-- | @Selector@ for @initWithContacts:@
initWithContactsSelector :: Selector
initWithContactsSelector = mkSelector "initWithContacts:"

-- | @Selector@ for @destinationType@
destinationTypeSelector :: Selector
destinationTypeSelector = mkSelector "destinationType"

-- | @Selector@ for @contacts@
contactsSelector :: Selector
contactsSelector = mkSelector "contacts"

