{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A view controller to display and edit a @CNContact.@
--
-- Generated bindings for @CNContactViewController@.
module ObjC.ContactsUI.CNContactViewController
  ( CNContactViewController
  , IsCNContactViewController(..)
  , descriptorForRequiredKeys
  , descriptorForRequiredKeysSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ContactsUI.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Descriptor for all keys that must be fetched on a contact before setting it on the view controller.
--
-- Pass this descriptor to the keysToFetch of the @CNContactFetchRequest@ if you want to display the contact in a @CNContactViewController.@
--
-- ObjC selector: @+ descriptorForRequiredKeys@
descriptorForRequiredKeys :: IO RawId
descriptorForRequiredKeys  =
  do
    cls' <- getRequiredClass "CNContactViewController"
    sendClassMessage cls' descriptorForRequiredKeysSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorForRequiredKeys@
descriptorForRequiredKeysSelector :: Selector '[] RawId
descriptorForRequiredKeysSelector = mkSelector "descriptorForRequiredKeys"

