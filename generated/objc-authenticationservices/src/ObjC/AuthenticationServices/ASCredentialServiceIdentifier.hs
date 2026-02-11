{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASCredentialServiceIdentifier@.
module ObjC.AuthenticationServices.ASCredentialServiceIdentifier
  ( ASCredentialServiceIdentifier
  , IsASCredentialServiceIdentifier(..)
  , initWithIdentifier_type
  , initWithIdentifier_type_displayName
  , displayName
  , identifier
  , type_
  , initWithIdentifier_typeSelector
  , initWithIdentifier_type_displayNameSelector
  , displayNameSelector
  , identifierSelector
  , typeSelector

  -- * Enum types
  , ASCredentialServiceIdentifierType(ASCredentialServiceIdentifierType)
  , pattern ASCredentialServiceIdentifierTypeDomain
  , pattern ASCredentialServiceIdentifierTypeURL
  , pattern ASCredentialServiceIdentifierTypeApp

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

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.AuthenticationServices.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initializes an ASCredentialServiceIdentifier object.
--
-- @identifier@ — string value for the service identifier.
--
-- @type@ — the type that the service identifier string represents.
--
-- ObjC selector: @- initWithIdentifier:type:@
initWithIdentifier_type :: (IsASCredentialServiceIdentifier asCredentialServiceIdentifier, IsNSString identifier) => asCredentialServiceIdentifier -> identifier -> ASCredentialServiceIdentifierType -> IO (Id ASCredentialServiceIdentifier)
initWithIdentifier_type asCredentialServiceIdentifier  identifier type_ =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg asCredentialServiceIdentifier (mkSelector "initWithIdentifier:type:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argCLong (coerce type_)] >>= ownedObject . castPtr

-- | Initializes an ASCredentialServiceIdentifier object.
--
-- - Parameters:   - identifier: The string value for the service identifier.   - type: The type that the service identifier string represents.   - displayName: A user visible name that describes the service.
--
-- ObjC selector: @- initWithIdentifier:type:displayName:@
initWithIdentifier_type_displayName :: (IsASCredentialServiceIdentifier asCredentialServiceIdentifier, IsNSString identifier, IsNSString displayName) => asCredentialServiceIdentifier -> identifier -> ASCredentialServiceIdentifierType -> displayName -> IO (Id ASCredentialServiceIdentifier)
initWithIdentifier_type_displayName asCredentialServiceIdentifier  identifier type_ displayName =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr displayName $ \raw_displayName ->
      sendMsg asCredentialServiceIdentifier (mkSelector "initWithIdentifier:type:displayName:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argCLong (coerce type_), argPtr (castPtr raw_displayName :: Ptr ())] >>= ownedObject . castPtr

-- | A user visible name for the identifier. For @app@ types it will contain the localized name of the app. For @URL@ types it will contain the host name of the URL if it contains a valid host. For @URL@ type identifiers that do not contain a valid host and for @domain@ type identifiers, this will be equal to @identifier@. This property is meant only as a best effort suggestion for display purposes. It is not used by the system to identify the service or suggest a credential for AutoFill.
--
-- ObjC selector: @- displayName@
displayName :: IsASCredentialServiceIdentifier asCredentialServiceIdentifier => asCredentialServiceIdentifier -> IO (Id NSString)
displayName asCredentialServiceIdentifier  =
  sendMsg asCredentialServiceIdentifier (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get the identifier.
--
-- Returns: The service identifier.
--
-- ObjC selector: @- identifier@
identifier :: IsASCredentialServiceIdentifier asCredentialServiceIdentifier => asCredentialServiceIdentifier -> IO (Id NSString)
identifier asCredentialServiceIdentifier  =
  sendMsg asCredentialServiceIdentifier (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get the service identifier type.
--
-- Returns: The service identifier type.
--
-- ObjC selector: @- type@
type_ :: IsASCredentialServiceIdentifier asCredentialServiceIdentifier => asCredentialServiceIdentifier -> IO ASCredentialServiceIdentifierType
type_ asCredentialServiceIdentifier  =
  fmap (coerce :: CLong -> ASCredentialServiceIdentifierType) $ sendMsg asCredentialServiceIdentifier (mkSelector "type") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:type:@
initWithIdentifier_typeSelector :: Selector
initWithIdentifier_typeSelector = mkSelector "initWithIdentifier:type:"

-- | @Selector@ for @initWithIdentifier:type:displayName:@
initWithIdentifier_type_displayNameSelector :: Selector
initWithIdentifier_type_displayNameSelector = mkSelector "initWithIdentifier:type:displayName:"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

