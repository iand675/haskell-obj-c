{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An immutable value object representing a social profile.
--
-- CNSocialProfile is thread safe.
--
-- Generated bindings for @CNSocialProfile@.
module ObjC.Contacts.CNSocialProfile
  ( CNSocialProfile
  , IsCNSocialProfile(..)
  , initWithUrlString_username_userIdentifier_service
  , localizedStringForKey
  , localizedStringForService
  , urlString
  , username
  , userIdentifier
  , service
  , initWithUrlString_username_userIdentifier_serviceSelector
  , localizedStringForKeySelector
  , localizedStringForServiceSelector
  , serviceSelector
  , urlStringSelector
  , userIdentifierSelector
  , usernameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithUrlString:username:userIdentifier:service:@
initWithUrlString_username_userIdentifier_service :: (IsCNSocialProfile cnSocialProfile, IsNSString urlString, IsNSString username, IsNSString userIdentifier, IsNSString service) => cnSocialProfile -> urlString -> username -> userIdentifier -> service -> IO (Id CNSocialProfile)
initWithUrlString_username_userIdentifier_service cnSocialProfile urlString username userIdentifier service =
  sendOwnedMessage cnSocialProfile initWithUrlString_username_userIdentifier_serviceSelector (toNSString urlString) (toNSString username) (toNSString userIdentifier) (toNSString service)

-- | Returns a user displayable property name.
--
-- ObjC selector: @+ localizedStringForKey:@
localizedStringForKey :: IsNSString key => key -> IO (Id NSString)
localizedStringForKey key =
  do
    cls' <- getRequiredClass "CNSocialProfile"
    sendClassMessage cls' localizedStringForKeySelector (toNSString key)

-- | Returns a user displayable service name.
--
-- ObjC selector: @+ localizedStringForService:@
localizedStringForService :: IsNSString service => service -> IO (Id NSString)
localizedStringForService service =
  do
    cls' <- getRequiredClass "CNSocialProfile"
    sendClassMessage cls' localizedStringForServiceSelector (toNSString service)

-- | @- urlString@
urlString :: IsCNSocialProfile cnSocialProfile => cnSocialProfile -> IO (Id NSString)
urlString cnSocialProfile =
  sendMessage cnSocialProfile urlStringSelector

-- | @- username@
username :: IsCNSocialProfile cnSocialProfile => cnSocialProfile -> IO (Id NSString)
username cnSocialProfile =
  sendMessage cnSocialProfile usernameSelector

-- | @- userIdentifier@
userIdentifier :: IsCNSocialProfile cnSocialProfile => cnSocialProfile -> IO (Id NSString)
userIdentifier cnSocialProfile =
  sendMessage cnSocialProfile userIdentifierSelector

-- | @- service@
service :: IsCNSocialProfile cnSocialProfile => cnSocialProfile -> IO (Id NSString)
service cnSocialProfile =
  sendMessage cnSocialProfile serviceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUrlString:username:userIdentifier:service:@
initWithUrlString_username_userIdentifier_serviceSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSString] (Id CNSocialProfile)
initWithUrlString_username_userIdentifier_serviceSelector = mkSelector "initWithUrlString:username:userIdentifier:service:"

-- | @Selector@ for @localizedStringForKey:@
localizedStringForKeySelector :: Selector '[Id NSString] (Id NSString)
localizedStringForKeySelector = mkSelector "localizedStringForKey:"

-- | @Selector@ for @localizedStringForService:@
localizedStringForServiceSelector :: Selector '[Id NSString] (Id NSString)
localizedStringForServiceSelector = mkSelector "localizedStringForService:"

-- | @Selector@ for @urlString@
urlStringSelector :: Selector '[] (Id NSString)
urlStringSelector = mkSelector "urlString"

-- | @Selector@ for @username@
usernameSelector :: Selector '[] (Id NSString)
usernameSelector = mkSelector "username"

-- | @Selector@ for @userIdentifier@
userIdentifierSelector :: Selector '[] (Id NSString)
userIdentifierSelector = mkSelector "userIdentifier"

-- | @Selector@ for @service@
serviceSelector :: Selector '[] (Id NSString)
serviceSelector = mkSelector "service"

