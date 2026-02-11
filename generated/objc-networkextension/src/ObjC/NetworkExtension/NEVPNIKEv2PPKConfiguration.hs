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
  , initWithIdentifier_keychainReferenceSelector
  , identifierSelector
  , keychainReferenceSelector
  , isMandatorySelector
  , setIsMandatorySelector


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
initWithIdentifier_keychainReference nevpnikEv2PPKConfiguration  identifier keychainReference =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr keychainReference $ \raw_keychainReference ->
      sendMsg nevpnikEv2PPKConfiguration (mkSelector "initWithIdentifier:keychainReference:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_keychainReference :: Ptr ())] >>= ownedObject . castPtr

-- | identifier
--
-- The identifer for the PPK.
--
-- ObjC selector: @- identifier@
identifier :: IsNEVPNIKEv2PPKConfiguration nevpnikEv2PPKConfiguration => nevpnikEv2PPKConfiguration -> IO (Id NSString)
identifier nevpnikEv2PPKConfiguration  =
  sendMsg nevpnikEv2PPKConfiguration (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | keychainReference
--
-- A persistent reference to a keychain item of class kSecClassGenericPassword containing the PPK.
--
-- ObjC selector: @- keychainReference@
keychainReference :: IsNEVPNIKEv2PPKConfiguration nevpnikEv2PPKConfiguration => nevpnikEv2PPKConfiguration -> IO (Id NSData)
keychainReference nevpnikEv2PPKConfiguration  =
  sendMsg nevpnikEv2PPKConfiguration (mkSelector "keychainReference") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | isMandatory
--
-- Boolean indicating whether use of the PPK is mandatory or not. Default is YES.
--
-- ObjC selector: @- isMandatory@
isMandatory :: IsNEVPNIKEv2PPKConfiguration nevpnikEv2PPKConfiguration => nevpnikEv2PPKConfiguration -> IO Bool
isMandatory nevpnikEv2PPKConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nevpnikEv2PPKConfiguration (mkSelector "isMandatory") retCULong []

-- | isMandatory
--
-- Boolean indicating whether use of the PPK is mandatory or not. Default is YES.
--
-- ObjC selector: @- setIsMandatory:@
setIsMandatory :: IsNEVPNIKEv2PPKConfiguration nevpnikEv2PPKConfiguration => nevpnikEv2PPKConfiguration -> Bool -> IO ()
setIsMandatory nevpnikEv2PPKConfiguration  value =
  sendMsg nevpnikEv2PPKConfiguration (mkSelector "setIsMandatory:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:keychainReference:@
initWithIdentifier_keychainReferenceSelector :: Selector
initWithIdentifier_keychainReferenceSelector = mkSelector "initWithIdentifier:keychainReference:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @keychainReference@
keychainReferenceSelector :: Selector
keychainReferenceSelector = mkSelector "keychainReference"

-- | @Selector@ for @isMandatory@
isMandatorySelector :: Selector
isMandatorySelector = mkSelector "isMandatory"

-- | @Selector@ for @setIsMandatory:@
setIsMandatorySelector :: Selector
setIsMandatorySelector = mkSelector "setIsMandatory:"

