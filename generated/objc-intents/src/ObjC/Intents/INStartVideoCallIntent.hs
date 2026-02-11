{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStartVideoCallIntent@.
module ObjC.Intents.INStartVideoCallIntent
  ( INStartVideoCallIntent
  , IsINStartVideoCallIntent(..)
  , initWithContacts
  , contacts
  , initWithContactsSelector
  , contactsSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- initWithContacts:@
initWithContacts :: (IsINStartVideoCallIntent inStartVideoCallIntent, IsNSArray contacts) => inStartVideoCallIntent -> contacts -> IO (Id INStartVideoCallIntent)
initWithContacts inStartVideoCallIntent  contacts =
withObjCPtr contacts $ \raw_contacts ->
    sendMsg inStartVideoCallIntent (mkSelector "initWithContacts:") (retPtr retVoid) [argPtr (castPtr raw_contacts :: Ptr ())] >>= ownedObject . castPtr

-- | @- contacts@
contacts :: IsINStartVideoCallIntent inStartVideoCallIntent => inStartVideoCallIntent -> IO (Id NSArray)
contacts inStartVideoCallIntent  =
  sendMsg inStartVideoCallIntent (mkSelector "contacts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContacts:@
initWithContactsSelector :: Selector
initWithContactsSelector = mkSelector "initWithContacts:"

-- | @Selector@ for @contacts@
contactsSelector :: Selector
contactsSelector = mkSelector "contacts"

