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
  , authenticationExpirationDateSelector
  , verificationDataSelector
  , samlAttributeQueryResponseSelector
  , accountProviderResponseSelector


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

import ObjC.VideoSubscriberAccount.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A value that uniquely identifies the account provider. You may use this value to brand your app.
--
-- ObjC selector: @- accountProviderIdentifier@
accountProviderIdentifier :: IsVSAccountMetadata vsAccountMetadata => vsAccountMetadata -> IO (Id NSString)
accountProviderIdentifier vsAccountMetadata  =
  sendMsg vsAccountMetadata (mkSelector "accountProviderIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Specifies when the user might need to re-authenticate with the account provider. The value might be nil if the user is not currently authenticated.
--
-- ObjC selector: @- authenticationExpirationDate@
authenticationExpirationDate :: IsVSAccountMetadata vsAccountMetadata => vsAccountMetadata -> IO (Id NSDate)
authenticationExpirationDate vsAccountMetadata  =
  sendMsg vsAccountMetadata (mkSelector "authenticationExpirationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An opaque blob of data that can be used to cryptographically verify that the SAML AttributeQuery response actually came from the account provider.
--
-- ObjC selector: @- verificationData@
verificationData :: IsVSAccountMetadata vsAccountMetadata => vsAccountMetadata -> IO (Id NSData)
verificationData vsAccountMetadata  =
  sendMsg vsAccountMetadata (mkSelector "verificationData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The SAML AttributeQuery response received from the account provider. The value might be nil if your account metadata request did not specify any SAML attributes or if the user does not have a valid authentication.
--
-- ObjC selector: @- SAMLAttributeQueryResponse@
samlAttributeQueryResponse :: IsVSAccountMetadata vsAccountMetadata => vsAccountMetadata -> IO (Id NSString)
samlAttributeQueryResponse vsAccountMetadata  =
  sendMsg vsAccountMetadata (mkSelector "SAMLAttributeQueryResponse") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The response received from the account provider. The value might be nil if your account metadata request did not specify any attributes, or if the user does not have a valid authentication.
--
-- ObjC selector: @- accountProviderResponse@
accountProviderResponse :: IsVSAccountMetadata vsAccountMetadata => vsAccountMetadata -> IO (Id VSAccountProviderResponse)
accountProviderResponse vsAccountMetadata  =
  sendMsg vsAccountMetadata (mkSelector "accountProviderResponse") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @accountProviderIdentifier@
accountProviderIdentifierSelector :: Selector
accountProviderIdentifierSelector = mkSelector "accountProviderIdentifier"

-- | @Selector@ for @authenticationExpirationDate@
authenticationExpirationDateSelector :: Selector
authenticationExpirationDateSelector = mkSelector "authenticationExpirationDate"

-- | @Selector@ for @verificationData@
verificationDataSelector :: Selector
verificationDataSelector = mkSelector "verificationData"

-- | @Selector@ for @SAMLAttributeQueryResponse@
samlAttributeQueryResponseSelector :: Selector
samlAttributeQueryResponseSelector = mkSelector "SAMLAttributeQueryResponse"

-- | @Selector@ for @accountProviderResponse@
accountProviderResponseSelector :: Selector
accountProviderResponseSelector = mkSelector "accountProviderResponse"

