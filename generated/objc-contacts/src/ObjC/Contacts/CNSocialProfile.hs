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
  , urlStringSelector
  , usernameSelector
  , userIdentifierSelector
  , serviceSelector


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

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithUrlString:username:userIdentifier:service:@
initWithUrlString_username_userIdentifier_service :: (IsCNSocialProfile cnSocialProfile, IsNSString urlString, IsNSString username, IsNSString userIdentifier, IsNSString service) => cnSocialProfile -> urlString -> username -> userIdentifier -> service -> IO (Id CNSocialProfile)
initWithUrlString_username_userIdentifier_service cnSocialProfile  urlString username userIdentifier service =
withObjCPtr urlString $ \raw_urlString ->
  withObjCPtr username $ \raw_username ->
    withObjCPtr userIdentifier $ \raw_userIdentifier ->
      withObjCPtr service $ \raw_service ->
          sendMsg cnSocialProfile (mkSelector "initWithUrlString:username:userIdentifier:service:") (retPtr retVoid) [argPtr (castPtr raw_urlString :: Ptr ()), argPtr (castPtr raw_username :: Ptr ()), argPtr (castPtr raw_userIdentifier :: Ptr ()), argPtr (castPtr raw_service :: Ptr ())] >>= ownedObject . castPtr

-- | Returns a user displayable property name.
--
-- ObjC selector: @+ localizedStringForKey:@
localizedStringForKey :: IsNSString key => key -> IO (Id NSString)
localizedStringForKey key =
  do
    cls' <- getRequiredClass "CNSocialProfile"
    withObjCPtr key $ \raw_key ->
      sendClassMsg cls' (mkSelector "localizedStringForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a user displayable service name.
--
-- ObjC selector: @+ localizedStringForService:@
localizedStringForService :: IsNSString service => service -> IO (Id NSString)
localizedStringForService service =
  do
    cls' <- getRequiredClass "CNSocialProfile"
    withObjCPtr service $ \raw_service ->
      sendClassMsg cls' (mkSelector "localizedStringForService:") (retPtr retVoid) [argPtr (castPtr raw_service :: Ptr ())] >>= retainedObject . castPtr

-- | @- urlString@
urlString :: IsCNSocialProfile cnSocialProfile => cnSocialProfile -> IO (Id NSString)
urlString cnSocialProfile  =
  sendMsg cnSocialProfile (mkSelector "urlString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- username@
username :: IsCNSocialProfile cnSocialProfile => cnSocialProfile -> IO (Id NSString)
username cnSocialProfile  =
  sendMsg cnSocialProfile (mkSelector "username") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- userIdentifier@
userIdentifier :: IsCNSocialProfile cnSocialProfile => cnSocialProfile -> IO (Id NSString)
userIdentifier cnSocialProfile  =
  sendMsg cnSocialProfile (mkSelector "userIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- service@
service :: IsCNSocialProfile cnSocialProfile => cnSocialProfile -> IO (Id NSString)
service cnSocialProfile  =
  sendMsg cnSocialProfile (mkSelector "service") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUrlString:username:userIdentifier:service:@
initWithUrlString_username_userIdentifier_serviceSelector :: Selector
initWithUrlString_username_userIdentifier_serviceSelector = mkSelector "initWithUrlString:username:userIdentifier:service:"

-- | @Selector@ for @localizedStringForKey:@
localizedStringForKeySelector :: Selector
localizedStringForKeySelector = mkSelector "localizedStringForKey:"

-- | @Selector@ for @localizedStringForService:@
localizedStringForServiceSelector :: Selector
localizedStringForServiceSelector = mkSelector "localizedStringForService:"

-- | @Selector@ for @urlString@
urlStringSelector :: Selector
urlStringSelector = mkSelector "urlString"

-- | @Selector@ for @username@
usernameSelector :: Selector
usernameSelector = mkSelector "username"

-- | @Selector@ for @userIdentifier@
userIdentifierSelector :: Selector
userIdentifierSelector = mkSelector "userIdentifier"

-- | @Selector@ for @service@
serviceSelector :: Selector
serviceSelector = mkSelector "service"

