{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStartVideoCallIntent@.
module ObjC.Intents.INStartVideoCallIntent
  ( INStartVideoCallIntent
  , IsINStartVideoCallIntent(..)
  , initWithContacts
  , contacts
  , contactsSelector
  , initWithContactsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithContacts:@
initWithContacts :: (IsINStartVideoCallIntent inStartVideoCallIntent, IsNSArray contacts) => inStartVideoCallIntent -> contacts -> IO (Id INStartVideoCallIntent)
initWithContacts inStartVideoCallIntent contacts =
  sendOwnedMessage inStartVideoCallIntent initWithContactsSelector (toNSArray contacts)

-- | @- contacts@
contacts :: IsINStartVideoCallIntent inStartVideoCallIntent => inStartVideoCallIntent -> IO (Id NSArray)
contacts inStartVideoCallIntent =
  sendMessage inStartVideoCallIntent contactsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContacts:@
initWithContactsSelector :: Selector '[Id NSArray] (Id INStartVideoCallIntent)
initWithContactsSelector = mkSelector "initWithContacts:"

-- | @Selector@ for @contacts@
contactsSelector :: Selector '[] (Id NSArray)
contactsSelector = mkSelector "contacts"

