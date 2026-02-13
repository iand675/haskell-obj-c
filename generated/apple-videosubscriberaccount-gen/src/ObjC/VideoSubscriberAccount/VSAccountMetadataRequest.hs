{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Specifies which information the app wants to obtain about the subscriber's account. You should only request the information you need to fulfill your contractual obligations.
--
-- Generated bindings for @VSAccountMetadataRequest@.
module ObjC.VideoSubscriberAccount.VSAccountMetadataRequest
  ( VSAccountMetadataRequest
  , IsVSAccountMetadataRequest(..)
  , channelIdentifier
  , setChannelIdentifier
  , supportedAccountProviderIdentifiers
  , setSupportedAccountProviderIdentifiers
  , featuredAccountProviderIdentifiers
  , setFeaturedAccountProviderIdentifiers
  , verificationToken
  , setVerificationToken
  , includeAccountProviderIdentifier
  , setIncludeAccountProviderIdentifier
  , includeAuthenticationExpirationDate
  , setIncludeAuthenticationExpirationDate
  , localizedVideoTitle
  , setLocalizedVideoTitle
  , interruptionAllowed
  , setInterruptionAllowed
  , forceAuthentication
  , setForceAuthentication
  , attributeNames
  , setAttributeNames
  , supportedAuthenticationSchemes
  , setSupportedAuthenticationSchemes
  , accountProviderAuthenticationToken
  , setAccountProviderAuthenticationToken
  , applicationAccountProviders
  , setApplicationAccountProviders
  , accountProviderAuthenticationTokenSelector
  , applicationAccountProvidersSelector
  , attributeNamesSelector
  , channelIdentifierSelector
  , featuredAccountProviderIdentifiersSelector
  , forceAuthenticationSelector
  , includeAccountProviderIdentifierSelector
  , includeAuthenticationExpirationDateSelector
  , interruptionAllowedSelector
  , localizedVideoTitleSelector
  , setAccountProviderAuthenticationTokenSelector
  , setApplicationAccountProvidersSelector
  , setAttributeNamesSelector
  , setChannelIdentifierSelector
  , setFeaturedAccountProviderIdentifiersSelector
  , setForceAuthenticationSelector
  , setIncludeAccountProviderIdentifierSelector
  , setIncludeAuthenticationExpirationDateSelector
  , setInterruptionAllowedSelector
  , setLocalizedVideoTitleSelector
  , setSupportedAccountProviderIdentifiersSelector
  , setSupportedAuthenticationSchemesSelector
  , setVerificationTokenSelector
  , supportedAccountProviderIdentifiersSelector
  , supportedAuthenticationSchemesSelector
  , verificationTokenSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.VideoSubscriberAccount.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Identifies who is making the request. For use by applications using the SAML authentication scheme only.
--
-- ObjC selector: @- channelIdentifier@
channelIdentifier :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO (Id NSString)
channelIdentifier vsAccountMetadataRequest =
  sendMessage vsAccountMetadataRequest channelIdentifierSelector

-- | Identifies who is making the request. For use by applications using the SAML authentication scheme only.
--
-- ObjC selector: @- setChannelIdentifier:@
setChannelIdentifier :: (IsVSAccountMetadataRequest vsAccountMetadataRequest, IsNSString value) => vsAccountMetadataRequest -> value -> IO ()
setChannelIdentifier vsAccountMetadataRequest value =
  sendMessage vsAccountMetadataRequest setChannelIdentifierSelector (toNSString value)

-- | If non-empty, limits which account providers can respond to the request.
--
-- ObjC selector: @- supportedAccountProviderIdentifiers@
supportedAccountProviderIdentifiers :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO (Id NSArray)
supportedAccountProviderIdentifiers vsAccountMetadataRequest =
  sendMessage vsAccountMetadataRequest supportedAccountProviderIdentifiersSelector

-- | If non-empty, limits which account providers can respond to the request.
--
-- ObjC selector: @- setSupportedAccountProviderIdentifiers:@
setSupportedAccountProviderIdentifiers :: (IsVSAccountMetadataRequest vsAccountMetadataRequest, IsNSArray value) => vsAccountMetadataRequest -> value -> IO ()
setSupportedAccountProviderIdentifiers vsAccountMetadataRequest value =
  sendMessage vsAccountMetadataRequest setSupportedAccountProviderIdentifiersSelector (toNSArray value)

-- | If non-empty, specifies providers which may be given more prominent placement when choosing an account provider during authentication.
--
-- ObjC selector: @- featuredAccountProviderIdentifiers@
featuredAccountProviderIdentifiers :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO (Id NSArray)
featuredAccountProviderIdentifiers vsAccountMetadataRequest =
  sendMessage vsAccountMetadataRequest featuredAccountProviderIdentifiersSelector

-- | If non-empty, specifies providers which may be given more prominent placement when choosing an account provider during authentication.
--
-- ObjC selector: @- setFeaturedAccountProviderIdentifiers:@
setFeaturedAccountProviderIdentifiers :: (IsVSAccountMetadataRequest vsAccountMetadataRequest, IsNSArray value) => vsAccountMetadataRequest -> value -> IO ()
setFeaturedAccountProviderIdentifiers vsAccountMetadataRequest value =
  sendMessage vsAccountMetadataRequest setFeaturedAccountProviderIdentifiersSelector (toNSArray value)

-- | A value that the account provider may use to verify the identity of the requesting app.
--
-- ObjC selector: @- verificationToken@
verificationToken :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO (Id NSString)
verificationToken vsAccountMetadataRequest =
  sendMessage vsAccountMetadataRequest verificationTokenSelector

-- | A value that the account provider may use to verify the identity of the requesting app.
--
-- ObjC selector: @- setVerificationToken:@
setVerificationToken :: (IsVSAccountMetadataRequest vsAccountMetadataRequest, IsNSString value) => vsAccountMetadataRequest -> value -> IO ()
setVerificationToken vsAccountMetadataRequest value =
  sendMessage vsAccountMetadataRequest setVerificationTokenSelector (toNSString value)

-- | Whether to request information that identifies the account provider.
--
-- ObjC selector: @- includeAccountProviderIdentifier@
includeAccountProviderIdentifier :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO Bool
includeAccountProviderIdentifier vsAccountMetadataRequest =
  sendMessage vsAccountMetadataRequest includeAccountProviderIdentifierSelector

-- | Whether to request information that identifies the account provider.
--
-- ObjC selector: @- setIncludeAccountProviderIdentifier:@
setIncludeAccountProviderIdentifier :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> Bool -> IO ()
setIncludeAccountProviderIdentifier vsAccountMetadataRequest value =
  sendMessage vsAccountMetadataRequest setIncludeAccountProviderIdentifierSelector value

-- | Whether to request the expiration date of the subscriber's current authentication.
--
-- ObjC selector: @- includeAuthenticationExpirationDate@
includeAuthenticationExpirationDate :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO Bool
includeAuthenticationExpirationDate vsAccountMetadataRequest =
  sendMessage vsAccountMetadataRequest includeAuthenticationExpirationDateSelector

-- | Whether to request the expiration date of the subscriber's current authentication.
--
-- ObjC selector: @- setIncludeAuthenticationExpirationDate:@
setIncludeAuthenticationExpirationDate :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> Bool -> IO ()
setIncludeAuthenticationExpirationDate vsAccountMetadataRequest value =
  sendMessage vsAccountMetadataRequest setIncludeAuthenticationExpirationDateSelector value

-- | A brief, user-presentable name for the video that the app will play if it receives a successful response. For example, "What's New in Swift" or "Office Space" Do not provide a value if the request will not be used to play a specific video.
--
-- ObjC selector: @- localizedVideoTitle@
localizedVideoTitle :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO (Id NSString)
localizedVideoTitle vsAccountMetadataRequest =
  sendMessage vsAccountMetadataRequest localizedVideoTitleSelector

-- | A brief, user-presentable name for the video that the app will play if it receives a successful response. For example, "What's New in Swift" or "Office Space" Do not provide a value if the request will not be used to play a specific video.
--
-- ObjC selector: @- setLocalizedVideoTitle:@
setLocalizedVideoTitle :: (IsVSAccountMetadataRequest vsAccountMetadataRequest, IsNSString value) => vsAccountMetadataRequest -> value -> IO ()
setLocalizedVideoTitle vsAccountMetadataRequest value =
  sendMessage vsAccountMetadataRequest setLocalizedVideoTitleSelector (toNSString value)

-- | Whether the user might expect to be prompted to authenticate in order to complete this request.
--
-- ObjC selector: @- interruptionAllowed@
interruptionAllowed :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO Bool
interruptionAllowed vsAccountMetadataRequest =
  sendMessage vsAccountMetadataRequest interruptionAllowedSelector

-- | Whether the user might expect to be prompted to authenticate in order to complete this request.
--
-- ObjC selector: @- setInterruptionAllowed:@
setInterruptionAllowed :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> Bool -> IO ()
setInterruptionAllowed vsAccountMetadataRequest value =
  sendMessage vsAccountMetadataRequest setInterruptionAllowedSelector value

-- | Requests that the TV Provider reauthenticate the user if they are already authenticated.
--
-- ObjC selector: @- forceAuthentication@
forceAuthentication :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO Bool
forceAuthentication vsAccountMetadataRequest =
  sendMessage vsAccountMetadataRequest forceAuthenticationSelector

-- | Requests that the TV Provider reauthenticate the user if they are already authenticated.
--
-- ObjC selector: @- setForceAuthentication:@
setForceAuthentication :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> Bool -> IO ()
setForceAuthentication vsAccountMetadataRequest value =
  sendMessage vsAccountMetadataRequest setForceAuthenticationSelector value

-- | Attributes to add to a SAML attributeQuery request and sent to the account provider.
--
-- ObjC selector: @- attributeNames@
attributeNames :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO (Id NSArray)
attributeNames vsAccountMetadataRequest =
  sendMessage vsAccountMetadataRequest attributeNamesSelector

-- | Attributes to add to a SAML attributeQuery request and sent to the account provider.
--
-- ObjC selector: @- setAttributeNames:@
setAttributeNames :: (IsVSAccountMetadataRequest vsAccountMetadataRequest, IsNSArray value) => vsAccountMetadataRequest -> value -> IO ()
setAttributeNames vsAccountMetadataRequest value =
  sendMessage vsAccountMetadataRequest setAttributeNamesSelector (toNSArray value)

-- | The collection of authentication schemes that the app supports for this request. This list may be used to determine compatibility of the app with providers. Defaults to SAML.
--
-- ObjC selector: @- supportedAuthenticationSchemes@
supportedAuthenticationSchemes :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO (Id NSArray)
supportedAuthenticationSchemes vsAccountMetadataRequest =
  sendMessage vsAccountMetadataRequest supportedAuthenticationSchemesSelector

-- | The collection of authentication schemes that the app supports for this request. This list may be used to determine compatibility of the app with providers. Defaults to SAML.
--
-- ObjC selector: @- setSupportedAuthenticationSchemes:@
setSupportedAuthenticationSchemes :: (IsVSAccountMetadataRequest vsAccountMetadataRequest, IsNSArray value) => vsAccountMetadataRequest -> value -> IO ()
setSupportedAuthenticationSchemes vsAccountMetadataRequest value =
  sendMessage vsAccountMetadataRequest setSupportedAuthenticationSchemesSelector (toNSArray value)

-- | A value that an account provider application may set to pass an existing authentication session. For use by TV Provider applications only.
--
-- ObjC selector: @- accountProviderAuthenticationToken@
accountProviderAuthenticationToken :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO (Id NSString)
accountProviderAuthenticationToken vsAccountMetadataRequest =
  sendMessage vsAccountMetadataRequest accountProviderAuthenticationTokenSelector

-- | A value that an account provider application may set to pass an existing authentication session. For use by TV Provider applications only.
--
-- ObjC selector: @- setAccountProviderAuthenticationToken:@
setAccountProviderAuthenticationToken :: (IsVSAccountMetadataRequest vsAccountMetadataRequest, IsNSString value) => vsAccountMetadataRequest -> value -> IO ()
setAccountProviderAuthenticationToken vsAccountMetadataRequest value =
  sendMessage vsAccountMetadataRequest setAccountProviderAuthenticationTokenSelector (toNSString value)

-- | Application-specific providers to be added to the list of account providers.
--
-- ObjC selector: @- applicationAccountProviders@
applicationAccountProviders :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO (Id NSArray)
applicationAccountProviders vsAccountMetadataRequest =
  sendMessage vsAccountMetadataRequest applicationAccountProvidersSelector

-- | Application-specific providers to be added to the list of account providers.
--
-- ObjC selector: @- setApplicationAccountProviders:@
setApplicationAccountProviders :: (IsVSAccountMetadataRequest vsAccountMetadataRequest, IsNSArray value) => vsAccountMetadataRequest -> value -> IO ()
setApplicationAccountProviders vsAccountMetadataRequest value =
  sendMessage vsAccountMetadataRequest setApplicationAccountProvidersSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @channelIdentifier@
channelIdentifierSelector :: Selector '[] (Id NSString)
channelIdentifierSelector = mkSelector "channelIdentifier"

-- | @Selector@ for @setChannelIdentifier:@
setChannelIdentifierSelector :: Selector '[Id NSString] ()
setChannelIdentifierSelector = mkSelector "setChannelIdentifier:"

-- | @Selector@ for @supportedAccountProviderIdentifiers@
supportedAccountProviderIdentifiersSelector :: Selector '[] (Id NSArray)
supportedAccountProviderIdentifiersSelector = mkSelector "supportedAccountProviderIdentifiers"

-- | @Selector@ for @setSupportedAccountProviderIdentifiers:@
setSupportedAccountProviderIdentifiersSelector :: Selector '[Id NSArray] ()
setSupportedAccountProviderIdentifiersSelector = mkSelector "setSupportedAccountProviderIdentifiers:"

-- | @Selector@ for @featuredAccountProviderIdentifiers@
featuredAccountProviderIdentifiersSelector :: Selector '[] (Id NSArray)
featuredAccountProviderIdentifiersSelector = mkSelector "featuredAccountProviderIdentifiers"

-- | @Selector@ for @setFeaturedAccountProviderIdentifiers:@
setFeaturedAccountProviderIdentifiersSelector :: Selector '[Id NSArray] ()
setFeaturedAccountProviderIdentifiersSelector = mkSelector "setFeaturedAccountProviderIdentifiers:"

-- | @Selector@ for @verificationToken@
verificationTokenSelector :: Selector '[] (Id NSString)
verificationTokenSelector = mkSelector "verificationToken"

-- | @Selector@ for @setVerificationToken:@
setVerificationTokenSelector :: Selector '[Id NSString] ()
setVerificationTokenSelector = mkSelector "setVerificationToken:"

-- | @Selector@ for @includeAccountProviderIdentifier@
includeAccountProviderIdentifierSelector :: Selector '[] Bool
includeAccountProviderIdentifierSelector = mkSelector "includeAccountProviderIdentifier"

-- | @Selector@ for @setIncludeAccountProviderIdentifier:@
setIncludeAccountProviderIdentifierSelector :: Selector '[Bool] ()
setIncludeAccountProviderIdentifierSelector = mkSelector "setIncludeAccountProviderIdentifier:"

-- | @Selector@ for @includeAuthenticationExpirationDate@
includeAuthenticationExpirationDateSelector :: Selector '[] Bool
includeAuthenticationExpirationDateSelector = mkSelector "includeAuthenticationExpirationDate"

-- | @Selector@ for @setIncludeAuthenticationExpirationDate:@
setIncludeAuthenticationExpirationDateSelector :: Selector '[Bool] ()
setIncludeAuthenticationExpirationDateSelector = mkSelector "setIncludeAuthenticationExpirationDate:"

-- | @Selector@ for @localizedVideoTitle@
localizedVideoTitleSelector :: Selector '[] (Id NSString)
localizedVideoTitleSelector = mkSelector "localizedVideoTitle"

-- | @Selector@ for @setLocalizedVideoTitle:@
setLocalizedVideoTitleSelector :: Selector '[Id NSString] ()
setLocalizedVideoTitleSelector = mkSelector "setLocalizedVideoTitle:"

-- | @Selector@ for @interruptionAllowed@
interruptionAllowedSelector :: Selector '[] Bool
interruptionAllowedSelector = mkSelector "interruptionAllowed"

-- | @Selector@ for @setInterruptionAllowed:@
setInterruptionAllowedSelector :: Selector '[Bool] ()
setInterruptionAllowedSelector = mkSelector "setInterruptionAllowed:"

-- | @Selector@ for @forceAuthentication@
forceAuthenticationSelector :: Selector '[] Bool
forceAuthenticationSelector = mkSelector "forceAuthentication"

-- | @Selector@ for @setForceAuthentication:@
setForceAuthenticationSelector :: Selector '[Bool] ()
setForceAuthenticationSelector = mkSelector "setForceAuthentication:"

-- | @Selector@ for @attributeNames@
attributeNamesSelector :: Selector '[] (Id NSArray)
attributeNamesSelector = mkSelector "attributeNames"

-- | @Selector@ for @setAttributeNames:@
setAttributeNamesSelector :: Selector '[Id NSArray] ()
setAttributeNamesSelector = mkSelector "setAttributeNames:"

-- | @Selector@ for @supportedAuthenticationSchemes@
supportedAuthenticationSchemesSelector :: Selector '[] (Id NSArray)
supportedAuthenticationSchemesSelector = mkSelector "supportedAuthenticationSchemes"

-- | @Selector@ for @setSupportedAuthenticationSchemes:@
setSupportedAuthenticationSchemesSelector :: Selector '[Id NSArray] ()
setSupportedAuthenticationSchemesSelector = mkSelector "setSupportedAuthenticationSchemes:"

-- | @Selector@ for @accountProviderAuthenticationToken@
accountProviderAuthenticationTokenSelector :: Selector '[] (Id NSString)
accountProviderAuthenticationTokenSelector = mkSelector "accountProviderAuthenticationToken"

-- | @Selector@ for @setAccountProviderAuthenticationToken:@
setAccountProviderAuthenticationTokenSelector :: Selector '[Id NSString] ()
setAccountProviderAuthenticationTokenSelector = mkSelector "setAccountProviderAuthenticationToken:"

-- | @Selector@ for @applicationAccountProviders@
applicationAccountProvidersSelector :: Selector '[] (Id NSArray)
applicationAccountProvidersSelector = mkSelector "applicationAccountProviders"

-- | @Selector@ for @setApplicationAccountProviders:@
setApplicationAccountProvidersSelector :: Selector '[Id NSArray] ()
setApplicationAccountProvidersSelector = mkSelector "setApplicationAccountProviders:"

