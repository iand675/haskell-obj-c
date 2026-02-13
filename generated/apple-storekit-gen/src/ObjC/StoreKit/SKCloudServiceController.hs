{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKCloudServiceController@.
module ObjC.StoreKit.SKCloudServiceController
  ( SKCloudServiceController
  , IsSKCloudServiceController(..)
  , authorizationStatus
  , requestAuthorization
  , requestCapabilitiesWithCompletionHandler
  , requestStorefrontCountryCodeWithCompletionHandler
  , requestStorefrontIdentifierWithCompletionHandler
  , requestUserTokenForDeveloperToken_completionHandler
  , requestPersonalizationTokenForClientToken_withCompletionHandler
  , authorizationStatusSelector
  , requestAuthorizationSelector
  , requestCapabilitiesWithCompletionHandlerSelector
  , requestPersonalizationTokenForClientToken_withCompletionHandlerSelector
  , requestStorefrontCountryCodeWithCompletionHandlerSelector
  , requestStorefrontIdentifierWithCompletionHandlerSelector
  , requestUserTokenForDeveloperToken_completionHandlerSelector

  -- * Enum types
  , SKCloudServiceAuthorizationStatus(SKCloudServiceAuthorizationStatus)
  , pattern SKCloudServiceAuthorizationStatusNotDetermined
  , pattern SKCloudServiceAuthorizationStatusDenied
  , pattern SKCloudServiceAuthorizationStatusRestricted
  , pattern SKCloudServiceAuthorizationStatusAuthorized

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.StoreKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ authorizationStatus@
authorizationStatus :: IO SKCloudServiceAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "SKCloudServiceController"
    sendClassMessage cls' authorizationStatusSelector

-- | @+ requestAuthorization:@
requestAuthorization :: Ptr () -> IO ()
requestAuthorization completionHandler =
  do
    cls' <- getRequiredClass "SKCloudServiceController"
    sendClassMessage cls' requestAuthorizationSelector completionHandler

-- | @- requestCapabilitiesWithCompletionHandler:@
requestCapabilitiesWithCompletionHandler :: IsSKCloudServiceController skCloudServiceController => skCloudServiceController -> Ptr () -> IO ()
requestCapabilitiesWithCompletionHandler skCloudServiceController completionHandler =
  sendMessage skCloudServiceController requestCapabilitiesWithCompletionHandlerSelector completionHandler

-- | @- requestStorefrontCountryCodeWithCompletionHandler:@
requestStorefrontCountryCodeWithCompletionHandler :: IsSKCloudServiceController skCloudServiceController => skCloudServiceController -> Ptr () -> IO ()
requestStorefrontCountryCodeWithCompletionHandler skCloudServiceController completionHandler =
  sendMessage skCloudServiceController requestStorefrontCountryCodeWithCompletionHandlerSelector completionHandler

-- | @- requestStorefrontIdentifierWithCompletionHandler:@
requestStorefrontIdentifierWithCompletionHandler :: IsSKCloudServiceController skCloudServiceController => skCloudServiceController -> Ptr () -> IO ()
requestStorefrontIdentifierWithCompletionHandler skCloudServiceController completionHandler =
  sendMessage skCloudServiceController requestStorefrontIdentifierWithCompletionHandlerSelector completionHandler

-- | @- requestUserTokenForDeveloperToken:completionHandler:@
requestUserTokenForDeveloperToken_completionHandler :: (IsSKCloudServiceController skCloudServiceController, IsNSString developerToken) => skCloudServiceController -> developerToken -> Ptr () -> IO ()
requestUserTokenForDeveloperToken_completionHandler skCloudServiceController developerToken completionHandler =
  sendMessage skCloudServiceController requestUserTokenForDeveloperToken_completionHandlerSelector (toNSString developerToken) completionHandler

-- | @- requestPersonalizationTokenForClientToken:withCompletionHandler:@
requestPersonalizationTokenForClientToken_withCompletionHandler :: (IsSKCloudServiceController skCloudServiceController, IsNSString clientToken) => skCloudServiceController -> clientToken -> Ptr () -> IO ()
requestPersonalizationTokenForClientToken_withCompletionHandler skCloudServiceController clientToken completionHandler =
  sendMessage skCloudServiceController requestPersonalizationTokenForClientToken_withCompletionHandlerSelector (toNSString clientToken) completionHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector '[] SKCloudServiceAuthorizationStatus
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @requestAuthorization:@
requestAuthorizationSelector :: Selector '[Ptr ()] ()
requestAuthorizationSelector = mkSelector "requestAuthorization:"

-- | @Selector@ for @requestCapabilitiesWithCompletionHandler:@
requestCapabilitiesWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
requestCapabilitiesWithCompletionHandlerSelector = mkSelector "requestCapabilitiesWithCompletionHandler:"

-- | @Selector@ for @requestStorefrontCountryCodeWithCompletionHandler:@
requestStorefrontCountryCodeWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
requestStorefrontCountryCodeWithCompletionHandlerSelector = mkSelector "requestStorefrontCountryCodeWithCompletionHandler:"

-- | @Selector@ for @requestStorefrontIdentifierWithCompletionHandler:@
requestStorefrontIdentifierWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
requestStorefrontIdentifierWithCompletionHandlerSelector = mkSelector "requestStorefrontIdentifierWithCompletionHandler:"

-- | @Selector@ for @requestUserTokenForDeveloperToken:completionHandler:@
requestUserTokenForDeveloperToken_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
requestUserTokenForDeveloperToken_completionHandlerSelector = mkSelector "requestUserTokenForDeveloperToken:completionHandler:"

-- | @Selector@ for @requestPersonalizationTokenForClientToken:withCompletionHandler:@
requestPersonalizationTokenForClientToken_withCompletionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
requestPersonalizationTokenForClientToken_withCompletionHandlerSelector = mkSelector "requestPersonalizationTokenForClientToken:withCompletionHandler:"

