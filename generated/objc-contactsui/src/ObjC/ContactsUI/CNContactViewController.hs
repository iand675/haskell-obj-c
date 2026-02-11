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
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "descriptorForRequiredKeys") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptorForRequiredKeys@
descriptorForRequiredKeysSelector :: Selector
descriptorForRequiredKeysSelector = mkSelector "descriptorForRequiredKeys"

