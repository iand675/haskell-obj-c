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
  , channelIdentifierSelector
  , setChannelIdentifierSelector
  , supportedAccountProviderIdentifiersSelector
  , setSupportedAccountProviderIdentifiersSelector
  , featuredAccountProviderIdentifiersSelector
  , setFeaturedAccountProviderIdentifiersSelector
  , verificationTokenSelector
  , setVerificationTokenSelector
  , includeAccountProviderIdentifierSelector
  , setIncludeAccountProviderIdentifierSelector
  , includeAuthenticationExpirationDateSelector
  , setIncludeAuthenticationExpirationDateSelector
  , localizedVideoTitleSelector
  , setLocalizedVideoTitleSelector
  , interruptionAllowedSelector
  , setInterruptionAllowedSelector
  , forceAuthenticationSelector
  , setForceAuthenticationSelector
  , attributeNamesSelector
  , setAttributeNamesSelector
  , supportedAuthenticationSchemesSelector
  , setSupportedAuthenticationSchemesSelector
  , accountProviderAuthenticationTokenSelector
  , setAccountProviderAuthenticationTokenSelector
  , applicationAccountProvidersSelector
  , setApplicationAccountProvidersSelector


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

-- | Identifies who is making the request. For use by applications using the SAML authentication scheme only.
--
-- ObjC selector: @- channelIdentifier@
channelIdentifier :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO (Id NSString)
channelIdentifier vsAccountMetadataRequest  =
  sendMsg vsAccountMetadataRequest (mkSelector "channelIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Identifies who is making the request. For use by applications using the SAML authentication scheme only.
--
-- ObjC selector: @- setChannelIdentifier:@
setChannelIdentifier :: (IsVSAccountMetadataRequest vsAccountMetadataRequest, IsNSString value) => vsAccountMetadataRequest -> value -> IO ()
setChannelIdentifier vsAccountMetadataRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg vsAccountMetadataRequest (mkSelector "setChannelIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | If non-empty, limits which account providers can respond to the request.
--
-- ObjC selector: @- supportedAccountProviderIdentifiers@
supportedAccountProviderIdentifiers :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO (Id NSArray)
supportedAccountProviderIdentifiers vsAccountMetadataRequest  =
  sendMsg vsAccountMetadataRequest (mkSelector "supportedAccountProviderIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If non-empty, limits which account providers can respond to the request.
--
-- ObjC selector: @- setSupportedAccountProviderIdentifiers:@
setSupportedAccountProviderIdentifiers :: (IsVSAccountMetadataRequest vsAccountMetadataRequest, IsNSArray value) => vsAccountMetadataRequest -> value -> IO ()
setSupportedAccountProviderIdentifiers vsAccountMetadataRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg vsAccountMetadataRequest (mkSelector "setSupportedAccountProviderIdentifiers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | If non-empty, specifies providers which may be given more prominent placement when choosing an account provider during authentication.
--
-- ObjC selector: @- featuredAccountProviderIdentifiers@
featuredAccountProviderIdentifiers :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO (Id NSArray)
featuredAccountProviderIdentifiers vsAccountMetadataRequest  =
  sendMsg vsAccountMetadataRequest (mkSelector "featuredAccountProviderIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If non-empty, specifies providers which may be given more prominent placement when choosing an account provider during authentication.
--
-- ObjC selector: @- setFeaturedAccountProviderIdentifiers:@
setFeaturedAccountProviderIdentifiers :: (IsVSAccountMetadataRequest vsAccountMetadataRequest, IsNSArray value) => vsAccountMetadataRequest -> value -> IO ()
setFeaturedAccountProviderIdentifiers vsAccountMetadataRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg vsAccountMetadataRequest (mkSelector "setFeaturedAccountProviderIdentifiers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A value that the account provider may use to verify the identity of the requesting app.
--
-- ObjC selector: @- verificationToken@
verificationToken :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO (Id NSString)
verificationToken vsAccountMetadataRequest  =
  sendMsg vsAccountMetadataRequest (mkSelector "verificationToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A value that the account provider may use to verify the identity of the requesting app.
--
-- ObjC selector: @- setVerificationToken:@
setVerificationToken :: (IsVSAccountMetadataRequest vsAccountMetadataRequest, IsNSString value) => vsAccountMetadataRequest -> value -> IO ()
setVerificationToken vsAccountMetadataRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg vsAccountMetadataRequest (mkSelector "setVerificationToken:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Whether to request information that identifies the account provider.
--
-- ObjC selector: @- includeAccountProviderIdentifier@
includeAccountProviderIdentifier :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO Bool
includeAccountProviderIdentifier vsAccountMetadataRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vsAccountMetadataRequest (mkSelector "includeAccountProviderIdentifier") retCULong []

-- | Whether to request information that identifies the account provider.
--
-- ObjC selector: @- setIncludeAccountProviderIdentifier:@
setIncludeAccountProviderIdentifier :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> Bool -> IO ()
setIncludeAccountProviderIdentifier vsAccountMetadataRequest  value =
  sendMsg vsAccountMetadataRequest (mkSelector "setIncludeAccountProviderIdentifier:") retVoid [argCULong (if value then 1 else 0)]

-- | Whether to request the expiration date of the subscriber's current authentication.
--
-- ObjC selector: @- includeAuthenticationExpirationDate@
includeAuthenticationExpirationDate :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO Bool
includeAuthenticationExpirationDate vsAccountMetadataRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vsAccountMetadataRequest (mkSelector "includeAuthenticationExpirationDate") retCULong []

-- | Whether to request the expiration date of the subscriber's current authentication.
--
-- ObjC selector: @- setIncludeAuthenticationExpirationDate:@
setIncludeAuthenticationExpirationDate :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> Bool -> IO ()
setIncludeAuthenticationExpirationDate vsAccountMetadataRequest  value =
  sendMsg vsAccountMetadataRequest (mkSelector "setIncludeAuthenticationExpirationDate:") retVoid [argCULong (if value then 1 else 0)]

-- | A brief, user-presentable name for the video that the app will play if it receives a successful response. For example, "What's New in Swift" or "Office Space" Do not provide a value if the request will not be used to play a specific video.
--
-- ObjC selector: @- localizedVideoTitle@
localizedVideoTitle :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO (Id NSString)
localizedVideoTitle vsAccountMetadataRequest  =
  sendMsg vsAccountMetadataRequest (mkSelector "localizedVideoTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A brief, user-presentable name for the video that the app will play if it receives a successful response. For example, "What's New in Swift" or "Office Space" Do not provide a value if the request will not be used to play a specific video.
--
-- ObjC selector: @- setLocalizedVideoTitle:@
setLocalizedVideoTitle :: (IsVSAccountMetadataRequest vsAccountMetadataRequest, IsNSString value) => vsAccountMetadataRequest -> value -> IO ()
setLocalizedVideoTitle vsAccountMetadataRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg vsAccountMetadataRequest (mkSelector "setLocalizedVideoTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Whether the user might expect to be prompted to authenticate in order to complete this request.
--
-- ObjC selector: @- interruptionAllowed@
interruptionAllowed :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO Bool
interruptionAllowed vsAccountMetadataRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vsAccountMetadataRequest (mkSelector "interruptionAllowed") retCULong []

-- | Whether the user might expect to be prompted to authenticate in order to complete this request.
--
-- ObjC selector: @- setInterruptionAllowed:@
setInterruptionAllowed :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> Bool -> IO ()
setInterruptionAllowed vsAccountMetadataRequest  value =
  sendMsg vsAccountMetadataRequest (mkSelector "setInterruptionAllowed:") retVoid [argCULong (if value then 1 else 0)]

-- | Requests that the TV Provider reauthenticate the user if they are already authenticated.
--
-- ObjC selector: @- forceAuthentication@
forceAuthentication :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO Bool
forceAuthentication vsAccountMetadataRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vsAccountMetadataRequest (mkSelector "forceAuthentication") retCULong []

-- | Requests that the TV Provider reauthenticate the user if they are already authenticated.
--
-- ObjC selector: @- setForceAuthentication:@
setForceAuthentication :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> Bool -> IO ()
setForceAuthentication vsAccountMetadataRequest  value =
  sendMsg vsAccountMetadataRequest (mkSelector "setForceAuthentication:") retVoid [argCULong (if value then 1 else 0)]

-- | Attributes to add to a SAML attributeQuery request and sent to the account provider.
--
-- ObjC selector: @- attributeNames@
attributeNames :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO (Id NSArray)
attributeNames vsAccountMetadataRequest  =
  sendMsg vsAccountMetadataRequest (mkSelector "attributeNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Attributes to add to a SAML attributeQuery request and sent to the account provider.
--
-- ObjC selector: @- setAttributeNames:@
setAttributeNames :: (IsVSAccountMetadataRequest vsAccountMetadataRequest, IsNSArray value) => vsAccountMetadataRequest -> value -> IO ()
setAttributeNames vsAccountMetadataRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg vsAccountMetadataRequest (mkSelector "setAttributeNames:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The collection of authentication schemes that the app supports for this request. This list may be used to determine compatibility of the app with providers. Defaults to SAML.
--
-- ObjC selector: @- supportedAuthenticationSchemes@
supportedAuthenticationSchemes :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO (Id NSArray)
supportedAuthenticationSchemes vsAccountMetadataRequest  =
  sendMsg vsAccountMetadataRequest (mkSelector "supportedAuthenticationSchemes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The collection of authentication schemes that the app supports for this request. This list may be used to determine compatibility of the app with providers. Defaults to SAML.
--
-- ObjC selector: @- setSupportedAuthenticationSchemes:@
setSupportedAuthenticationSchemes :: (IsVSAccountMetadataRequest vsAccountMetadataRequest, IsNSArray value) => vsAccountMetadataRequest -> value -> IO ()
setSupportedAuthenticationSchemes vsAccountMetadataRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg vsAccountMetadataRequest (mkSelector "setSupportedAuthenticationSchemes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A value that an account provider application may set to pass an existing authentication session. For use by TV Provider applications only.
--
-- ObjC selector: @- accountProviderAuthenticationToken@
accountProviderAuthenticationToken :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO (Id NSString)
accountProviderAuthenticationToken vsAccountMetadataRequest  =
  sendMsg vsAccountMetadataRequest (mkSelector "accountProviderAuthenticationToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A value that an account provider application may set to pass an existing authentication session. For use by TV Provider applications only.
--
-- ObjC selector: @- setAccountProviderAuthenticationToken:@
setAccountProviderAuthenticationToken :: (IsVSAccountMetadataRequest vsAccountMetadataRequest, IsNSString value) => vsAccountMetadataRequest -> value -> IO ()
setAccountProviderAuthenticationToken vsAccountMetadataRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg vsAccountMetadataRequest (mkSelector "setAccountProviderAuthenticationToken:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Application-specific providers to be added to the list of account providers.
--
-- ObjC selector: @- applicationAccountProviders@
applicationAccountProviders :: IsVSAccountMetadataRequest vsAccountMetadataRequest => vsAccountMetadataRequest -> IO (Id NSArray)
applicationAccountProviders vsAccountMetadataRequest  =
  sendMsg vsAccountMetadataRequest (mkSelector "applicationAccountProviders") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Application-specific providers to be added to the list of account providers.
--
-- ObjC selector: @- setApplicationAccountProviders:@
setApplicationAccountProviders :: (IsVSAccountMetadataRequest vsAccountMetadataRequest, IsNSArray value) => vsAccountMetadataRequest -> value -> IO ()
setApplicationAccountProviders vsAccountMetadataRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg vsAccountMetadataRequest (mkSelector "setApplicationAccountProviders:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @channelIdentifier@
channelIdentifierSelector :: Selector
channelIdentifierSelector = mkSelector "channelIdentifier"

-- | @Selector@ for @setChannelIdentifier:@
setChannelIdentifierSelector :: Selector
setChannelIdentifierSelector = mkSelector "setChannelIdentifier:"

-- | @Selector@ for @supportedAccountProviderIdentifiers@
supportedAccountProviderIdentifiersSelector :: Selector
supportedAccountProviderIdentifiersSelector = mkSelector "supportedAccountProviderIdentifiers"

-- | @Selector@ for @setSupportedAccountProviderIdentifiers:@
setSupportedAccountProviderIdentifiersSelector :: Selector
setSupportedAccountProviderIdentifiersSelector = mkSelector "setSupportedAccountProviderIdentifiers:"

-- | @Selector@ for @featuredAccountProviderIdentifiers@
featuredAccountProviderIdentifiersSelector :: Selector
featuredAccountProviderIdentifiersSelector = mkSelector "featuredAccountProviderIdentifiers"

-- | @Selector@ for @setFeaturedAccountProviderIdentifiers:@
setFeaturedAccountProviderIdentifiersSelector :: Selector
setFeaturedAccountProviderIdentifiersSelector = mkSelector "setFeaturedAccountProviderIdentifiers:"

-- | @Selector@ for @verificationToken@
verificationTokenSelector :: Selector
verificationTokenSelector = mkSelector "verificationToken"

-- | @Selector@ for @setVerificationToken:@
setVerificationTokenSelector :: Selector
setVerificationTokenSelector = mkSelector "setVerificationToken:"

-- | @Selector@ for @includeAccountProviderIdentifier@
includeAccountProviderIdentifierSelector :: Selector
includeAccountProviderIdentifierSelector = mkSelector "includeAccountProviderIdentifier"

-- | @Selector@ for @setIncludeAccountProviderIdentifier:@
setIncludeAccountProviderIdentifierSelector :: Selector
setIncludeAccountProviderIdentifierSelector = mkSelector "setIncludeAccountProviderIdentifier:"

-- | @Selector@ for @includeAuthenticationExpirationDate@
includeAuthenticationExpirationDateSelector :: Selector
includeAuthenticationExpirationDateSelector = mkSelector "includeAuthenticationExpirationDate"

-- | @Selector@ for @setIncludeAuthenticationExpirationDate:@
setIncludeAuthenticationExpirationDateSelector :: Selector
setIncludeAuthenticationExpirationDateSelector = mkSelector "setIncludeAuthenticationExpirationDate:"

-- | @Selector@ for @localizedVideoTitle@
localizedVideoTitleSelector :: Selector
localizedVideoTitleSelector = mkSelector "localizedVideoTitle"

-- | @Selector@ for @setLocalizedVideoTitle:@
setLocalizedVideoTitleSelector :: Selector
setLocalizedVideoTitleSelector = mkSelector "setLocalizedVideoTitle:"

-- | @Selector@ for @interruptionAllowed@
interruptionAllowedSelector :: Selector
interruptionAllowedSelector = mkSelector "interruptionAllowed"

-- | @Selector@ for @setInterruptionAllowed:@
setInterruptionAllowedSelector :: Selector
setInterruptionAllowedSelector = mkSelector "setInterruptionAllowed:"

-- | @Selector@ for @forceAuthentication@
forceAuthenticationSelector :: Selector
forceAuthenticationSelector = mkSelector "forceAuthentication"

-- | @Selector@ for @setForceAuthentication:@
setForceAuthenticationSelector :: Selector
setForceAuthenticationSelector = mkSelector "setForceAuthentication:"

-- | @Selector@ for @attributeNames@
attributeNamesSelector :: Selector
attributeNamesSelector = mkSelector "attributeNames"

-- | @Selector@ for @setAttributeNames:@
setAttributeNamesSelector :: Selector
setAttributeNamesSelector = mkSelector "setAttributeNames:"

-- | @Selector@ for @supportedAuthenticationSchemes@
supportedAuthenticationSchemesSelector :: Selector
supportedAuthenticationSchemesSelector = mkSelector "supportedAuthenticationSchemes"

-- | @Selector@ for @setSupportedAuthenticationSchemes:@
setSupportedAuthenticationSchemesSelector :: Selector
setSupportedAuthenticationSchemesSelector = mkSelector "setSupportedAuthenticationSchemes:"

-- | @Selector@ for @accountProviderAuthenticationToken@
accountProviderAuthenticationTokenSelector :: Selector
accountProviderAuthenticationTokenSelector = mkSelector "accountProviderAuthenticationToken"

-- | @Selector@ for @setAccountProviderAuthenticationToken:@
setAccountProviderAuthenticationTokenSelector :: Selector
setAccountProviderAuthenticationTokenSelector = mkSelector "setAccountProviderAuthenticationToken:"

-- | @Selector@ for @applicationAccountProviders@
applicationAccountProvidersSelector :: Selector
applicationAccountProvidersSelector = mkSelector "applicationAccountProviders"

-- | @Selector@ for @setApplicationAccountProviders:@
setApplicationAccountProvidersSelector :: Selector
setApplicationAccountProvidersSelector = mkSelector "setApplicationAccountProviders:"

