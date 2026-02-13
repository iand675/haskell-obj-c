{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEVPNIKEv2PPKConfiguration
--
-- The NEVPNIKEv2PPKConfiguration class declares the programmatic interface of an object that manages parameters for a Post-quantum Pre-shared Key (PPK)
--
-- Instances of this class conform to RFC 8784. Instances of this class are thread safe.
--
-- Generated bindings for @NEVPNIKEv2PPKConfiguration@.
module ObjC.NetworkExtension.NEVPNIKEv2PPKConfiguration
  ( NEVPNIKEv2PPKConfiguration
  , IsNEVPNIKEv2PPKConfiguration(..)
  , initWithIdentifier_keychainReference
  , identifier
  , keychainReference
  , isMandatory
  , setIsMandatory
  , identifierSelector
  , initWithIdentifier_keychainReferenceSelector
  , isMandatorySelector
  , keychainReferenceSelector
  , setIsMandatorySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithIdentifier:keychainReference:
--
-- Initialize a newly-allocated NEVPNIKEv2PPKConfiguration object.
--
-- @identifier@ — The identifier for the PPK.
--
-- @keychainReference@ — A persistent reference to a keychain item of class kSecClassGenericPassword containing the PPK.
--
-- ObjC selector: @- initWithIdentifier:keychainReference:@
initWithIdentifier_keychainReference :: (IsNEVPNIKEv2PPKConfiguration nevpnikEv2PPKConfiguration, IsNSString identifier, IsNSData keychainReference) => nevpnikEv2PPKConfiguration -> identifier -> keychainReference -> IO (Id NEVPNIKEv2PPKConfiguration)
initWithIdentifier_keychainReference nevpnikEv2PPKConfiguration identifier keychainReference =
  sendOwnedMessage nevpnikEv2PPKConfiguration initWithIdentifier_keychainReferenceSelector (toNSString identifier) (toNSData keychainReference)

-- | identifier
--
-- The identifer for the PPK.
--
-- ObjC selector: @- identifier@
identifier :: IsNEVPNIKEv2PPKConfiguration nevpnikEv2PPKConfiguration => nevpnikEv2PPKConfiguration -> IO (Id NSString)
identifier nevpnikEv2PPKConfiguration =
  sendMessage nevpnikEv2PPKConfiguration identifierSelector

-- | keychainReference
--
-- A persistent reference to a keychain item of class kSecClassGenericPassword containing the PPK.
--
-- ObjC selector: @- keychainReference@
keychainReference :: IsNEVPNIKEv2PPKConfiguration nevpnikEv2PPKConfiguration => nevpnikEv2PPKConfiguration -> IO (Id NSData)
keychainReference nevpnikEv2PPKConfiguration =
  sendMessage nevpnikEv2PPKConfiguration keychainReferenceSelector

-- | isMandatory
--
-- Boolean indicating whether use of the PPK is mandatory or not. Default is YES.
--
-- ObjC selector: @- isMandatory@
isMandatory :: IsNEVPNIKEv2PPKConfiguration nevpnikEv2PPKConfiguration => nevpnikEv2PPKConfiguration -> IO Bool
isMandatory nevpnikEv2PPKConfiguration =
  sendMessage nevpnikEv2PPKConfiguration isMandatorySelector

-- | isMandatory
--
-- Boolean indicating whether use of the PPK is mandatory or not. Default is YES.
--
-- ObjC selector: @- setIsMandatory:@
setIsMandatory :: IsNEVPNIKEv2PPKConfiguration nevpnikEv2PPKConfiguration => nevpnikEv2PPKConfiguration -> Bool -> IO ()
setIsMandatory nevpnikEv2PPKConfiguration value =
  sendMessage nevpnikEv2PPKConfiguration setIsMandatorySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:keychainReference:@
initWithIdentifier_keychainReferenceSelector :: Selector '[Id NSString, Id NSData] (Id NEVPNIKEv2PPKConfiguration)
initWithIdentifier_keychainReferenceSelector = mkSelector "initWithIdentifier:keychainReference:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @keychainReference@
keychainReferenceSelector :: Selector '[] (Id NSData)
keychainReferenceSelector = mkSelector "keychainReference"

-- | @Selector@ for @isMandatory@
isMandatorySelector :: Selector '[] Bool
isMandatorySelector = mkSelector "isMandatory"

-- | @Selector@ for @setIsMandatory:@
setIsMandatorySelector :: Selector '[Bool] ()
setIsMandatorySelector = mkSelector "setIsMandatory:"

