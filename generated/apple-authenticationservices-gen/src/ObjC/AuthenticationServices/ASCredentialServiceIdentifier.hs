{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , displayNameSelector
  , identifierSelector
  , initWithIdentifier_typeSelector
  , initWithIdentifier_type_displayNameSelector
  , typeSelector

  -- * Enum types
  , ASCredentialServiceIdentifierType(ASCredentialServiceIdentifierType)
  , pattern ASCredentialServiceIdentifierTypeDomain
  , pattern ASCredentialServiceIdentifierTypeURL
  , pattern ASCredentialServiceIdentifierTypeApp

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithIdentifier_type asCredentialServiceIdentifier identifier type_ =
  sendOwnedMessage asCredentialServiceIdentifier initWithIdentifier_typeSelector (toNSString identifier) type_

-- | Initializes an ASCredentialServiceIdentifier object.
--
-- - Parameters:   - identifier: The string value for the service identifier.   - type: The type that the service identifier string represents.   - displayName: A user visible name that describes the service.
--
-- ObjC selector: @- initWithIdentifier:type:displayName:@
initWithIdentifier_type_displayName :: (IsASCredentialServiceIdentifier asCredentialServiceIdentifier, IsNSString identifier, IsNSString displayName) => asCredentialServiceIdentifier -> identifier -> ASCredentialServiceIdentifierType -> displayName -> IO (Id ASCredentialServiceIdentifier)
initWithIdentifier_type_displayName asCredentialServiceIdentifier identifier type_ displayName =
  sendOwnedMessage asCredentialServiceIdentifier initWithIdentifier_type_displayNameSelector (toNSString identifier) type_ (toNSString displayName)

-- | A user visible name for the identifier. For @app@ types it will contain the localized name of the app. For @URL@ types it will contain the host name of the URL if it contains a valid host. For @URL@ type identifiers that do not contain a valid host and for @domain@ type identifiers, this will be equal to @identifier@. This property is meant only as a best effort suggestion for display purposes. It is not used by the system to identify the service or suggest a credential for AutoFill.
--
-- ObjC selector: @- displayName@
displayName :: IsASCredentialServiceIdentifier asCredentialServiceIdentifier => asCredentialServiceIdentifier -> IO (Id NSString)
displayName asCredentialServiceIdentifier =
  sendMessage asCredentialServiceIdentifier displayNameSelector

-- | Get the identifier.
--
-- Returns: The service identifier.
--
-- ObjC selector: @- identifier@
identifier :: IsASCredentialServiceIdentifier asCredentialServiceIdentifier => asCredentialServiceIdentifier -> IO (Id NSString)
identifier asCredentialServiceIdentifier =
  sendMessage asCredentialServiceIdentifier identifierSelector

-- | Get the service identifier type.
--
-- Returns: The service identifier type.
--
-- ObjC selector: @- type@
type_ :: IsASCredentialServiceIdentifier asCredentialServiceIdentifier => asCredentialServiceIdentifier -> IO ASCredentialServiceIdentifierType
type_ asCredentialServiceIdentifier =
  sendMessage asCredentialServiceIdentifier typeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:type:@
initWithIdentifier_typeSelector :: Selector '[Id NSString, ASCredentialServiceIdentifierType] (Id ASCredentialServiceIdentifier)
initWithIdentifier_typeSelector = mkSelector "initWithIdentifier:type:"

-- | @Selector@ for @initWithIdentifier:type:displayName:@
initWithIdentifier_type_displayNameSelector :: Selector '[Id NSString, ASCredentialServiceIdentifierType, Id NSString] (Id ASCredentialServiceIdentifier)
initWithIdentifier_type_displayNameSelector = mkSelector "initWithIdentifier:type:displayName:"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector '[] (Id NSString)
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @type@
typeSelector :: Selector '[] ASCredentialServiceIdentifierType
typeSelector = mkSelector "type"

