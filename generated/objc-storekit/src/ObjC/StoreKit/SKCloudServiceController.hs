{-# LANGUAGE PatternSynonyms #-}
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
  , requestStorefrontCountryCodeWithCompletionHandlerSelector
  , requestStorefrontIdentifierWithCompletionHandlerSelector
  , requestUserTokenForDeveloperToken_completionHandlerSelector
  , requestPersonalizationTokenForClientToken_withCompletionHandlerSelector

  -- * Enum types
  , SKCloudServiceAuthorizationStatus(SKCloudServiceAuthorizationStatus)
  , pattern SKCloudServiceAuthorizationStatusNotDetermined
  , pattern SKCloudServiceAuthorizationStatusDenied
  , pattern SKCloudServiceAuthorizationStatusRestricted
  , pattern SKCloudServiceAuthorizationStatusAuthorized

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

import ObjC.StoreKit.Internal.Classes
import ObjC.StoreKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ authorizationStatus@
authorizationStatus :: IO SKCloudServiceAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "SKCloudServiceController"
    fmap (coerce :: CLong -> SKCloudServiceAuthorizationStatus) $ sendClassMsg cls' (mkSelector "authorizationStatus") retCLong []

-- | @+ requestAuthorization:@
requestAuthorization :: Ptr () -> IO ()
requestAuthorization completionHandler =
  do
    cls' <- getRequiredClass "SKCloudServiceController"
    sendClassMsg cls' (mkSelector "requestAuthorization:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- requestCapabilitiesWithCompletionHandler:@
requestCapabilitiesWithCompletionHandler :: IsSKCloudServiceController skCloudServiceController => skCloudServiceController -> Ptr () -> IO ()
requestCapabilitiesWithCompletionHandler skCloudServiceController  completionHandler =
  sendMsg skCloudServiceController (mkSelector "requestCapabilitiesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- requestStorefrontCountryCodeWithCompletionHandler:@
requestStorefrontCountryCodeWithCompletionHandler :: IsSKCloudServiceController skCloudServiceController => skCloudServiceController -> Ptr () -> IO ()
requestStorefrontCountryCodeWithCompletionHandler skCloudServiceController  completionHandler =
  sendMsg skCloudServiceController (mkSelector "requestStorefrontCountryCodeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- requestStorefrontIdentifierWithCompletionHandler:@
requestStorefrontIdentifierWithCompletionHandler :: IsSKCloudServiceController skCloudServiceController => skCloudServiceController -> Ptr () -> IO ()
requestStorefrontIdentifierWithCompletionHandler skCloudServiceController  completionHandler =
  sendMsg skCloudServiceController (mkSelector "requestStorefrontIdentifierWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- requestUserTokenForDeveloperToken:completionHandler:@
requestUserTokenForDeveloperToken_completionHandler :: (IsSKCloudServiceController skCloudServiceController, IsNSString developerToken) => skCloudServiceController -> developerToken -> Ptr () -> IO ()
requestUserTokenForDeveloperToken_completionHandler skCloudServiceController  developerToken completionHandler =
withObjCPtr developerToken $ \raw_developerToken ->
    sendMsg skCloudServiceController (mkSelector "requestUserTokenForDeveloperToken:completionHandler:") retVoid [argPtr (castPtr raw_developerToken :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- requestPersonalizationTokenForClientToken:withCompletionHandler:@
requestPersonalizationTokenForClientToken_withCompletionHandler :: (IsSKCloudServiceController skCloudServiceController, IsNSString clientToken) => skCloudServiceController -> clientToken -> Ptr () -> IO ()
requestPersonalizationTokenForClientToken_withCompletionHandler skCloudServiceController  clientToken completionHandler =
withObjCPtr clientToken $ \raw_clientToken ->
    sendMsg skCloudServiceController (mkSelector "requestPersonalizationTokenForClientToken:withCompletionHandler:") retVoid [argPtr (castPtr raw_clientToken :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @requestAuthorization:@
requestAuthorizationSelector :: Selector
requestAuthorizationSelector = mkSelector "requestAuthorization:"

-- | @Selector@ for @requestCapabilitiesWithCompletionHandler:@
requestCapabilitiesWithCompletionHandlerSelector :: Selector
requestCapabilitiesWithCompletionHandlerSelector = mkSelector "requestCapabilitiesWithCompletionHandler:"

-- | @Selector@ for @requestStorefrontCountryCodeWithCompletionHandler:@
requestStorefrontCountryCodeWithCompletionHandlerSelector :: Selector
requestStorefrontCountryCodeWithCompletionHandlerSelector = mkSelector "requestStorefrontCountryCodeWithCompletionHandler:"

-- | @Selector@ for @requestStorefrontIdentifierWithCompletionHandler:@
requestStorefrontIdentifierWithCompletionHandlerSelector :: Selector
requestStorefrontIdentifierWithCompletionHandlerSelector = mkSelector "requestStorefrontIdentifierWithCompletionHandler:"

-- | @Selector@ for @requestUserTokenForDeveloperToken:completionHandler:@
requestUserTokenForDeveloperToken_completionHandlerSelector :: Selector
requestUserTokenForDeveloperToken_completionHandlerSelector = mkSelector "requestUserTokenForDeveloperToken:completionHandler:"

-- | @Selector@ for @requestPersonalizationTokenForClientToken:withCompletionHandler:@
requestPersonalizationTokenForClientToken_withCompletionHandlerSelector :: Selector
requestPersonalizationTokenForClientToken_withCompletionHandlerSelector = mkSelector "requestPersonalizationTokenForClientToken:withCompletionHandler:"

