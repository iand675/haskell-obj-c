{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A collection of information about a subscriber's account.
--
-- Generated bindings for @VSAccountMetadata@.
module ObjC.VideoSubscriberAccount.VSAccountMetadata
  ( VSAccountMetadata
  , IsVSAccountMetadata(..)
  , accountProviderIdentifier
  , authenticationExpirationDate
  , verificationData
  , samlAttributeQueryResponse
  , accountProviderResponse
  , accountProviderIdentifierSelector
  , accountProviderResponseSelector
  , authenticationExpirationDateSelector
  , samlAttributeQueryResponseSelector
  , verificationDataSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.VideoSubscriberAccount.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A value that uniquely identifies the account provider. You may use this value to brand your app.
--
-- ObjC selector: @- accountProviderIdentifier@
accountProviderIdentifier :: IsVSAccountMetadata vsAccountMetadata => vsAccountMetadata -> IO (Id NSString)
accountProviderIdentifier vsAccountMetadata =
  sendMessage vsAccountMetadata accountProviderIdentifierSelector

-- | Specifies when the user might need to re-authenticate with the account provider. The value might be nil if the user is not currently authenticated.
--
-- ObjC selector: @- authenticationExpirationDate@
authenticationExpirationDate :: IsVSAccountMetadata vsAccountMetadata => vsAccountMetadata -> IO (Id NSDate)
authenticationExpirationDate vsAccountMetadata =
  sendMessage vsAccountMetadata authenticationExpirationDateSelector

-- | An opaque blob of data that can be used to cryptographically verify that the SAML AttributeQuery response actually came from the account provider.
--
-- ObjC selector: @- verificationData@
verificationData :: IsVSAccountMetadata vsAccountMetadata => vsAccountMetadata -> IO (Id NSData)
verificationData vsAccountMetadata =
  sendMessage vsAccountMetadata verificationDataSelector

-- | The SAML AttributeQuery response received from the account provider. The value might be nil if your account metadata request did not specify any SAML attributes or if the user does not have a valid authentication.
--
-- ObjC selector: @- SAMLAttributeQueryResponse@
samlAttributeQueryResponse :: IsVSAccountMetadata vsAccountMetadata => vsAccountMetadata -> IO (Id NSString)
samlAttributeQueryResponse vsAccountMetadata =
  sendMessage vsAccountMetadata samlAttributeQueryResponseSelector

-- | The response received from the account provider. The value might be nil if your account metadata request did not specify any attributes, or if the user does not have a valid authentication.
--
-- ObjC selector: @- accountProviderResponse@
accountProviderResponse :: IsVSAccountMetadata vsAccountMetadata => vsAccountMetadata -> IO (Id VSAccountProviderResponse)
accountProviderResponse vsAccountMetadata =
  sendMessage vsAccountMetadata accountProviderResponseSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @accountProviderIdentifier@
accountProviderIdentifierSelector :: Selector '[] (Id NSString)
accountProviderIdentifierSelector = mkSelector "accountProviderIdentifier"

-- | @Selector@ for @authenticationExpirationDate@
authenticationExpirationDateSelector :: Selector '[] (Id NSDate)
authenticationExpirationDateSelector = mkSelector "authenticationExpirationDate"

-- | @Selector@ for @verificationData@
verificationDataSelector :: Selector '[] (Id NSData)
verificationDataSelector = mkSelector "verificationData"

-- | @Selector@ for @SAMLAttributeQueryResponse@
samlAttributeQueryResponseSelector :: Selector '[] (Id NSString)
samlAttributeQueryResponseSelector = mkSelector "SAMLAttributeQueryResponse"

-- | @Selector@ for @accountProviderResponse@
accountProviderResponseSelector :: Selector '[] (Id VSAccountProviderResponse)
accountProviderResponseSelector = mkSelector "accountProviderResponse"

