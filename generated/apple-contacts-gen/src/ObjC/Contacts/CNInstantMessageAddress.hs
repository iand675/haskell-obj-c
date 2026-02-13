{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An immutable value object representing an instant message address.
--
-- CNInstantMessageAddress is thread safe.
--
-- Generated bindings for @CNInstantMessageAddress@.
module ObjC.Contacts.CNInstantMessageAddress
  ( CNInstantMessageAddress
  , IsCNInstantMessageAddress(..)
  , initWithUsername_service
  , localizedStringForKey
  , localizedStringForService
  , username
  , service
  , initWithUsername_serviceSelector
  , localizedStringForKeySelector
  , localizedStringForServiceSelector
  , serviceSelector
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

-- | @- initWithUsername:service:@
initWithUsername_service :: (IsCNInstantMessageAddress cnInstantMessageAddress, IsNSString username, IsNSString service) => cnInstantMessageAddress -> username -> service -> IO (Id CNInstantMessageAddress)
initWithUsername_service cnInstantMessageAddress username service =
  sendOwnedMessage cnInstantMessageAddress initWithUsername_serviceSelector (toNSString username) (toNSString service)

-- | Returns a user displayable property name.
--
-- ObjC selector: @+ localizedStringForKey:@
localizedStringForKey :: IsNSString key => key -> IO (Id NSString)
localizedStringForKey key =
  do
    cls' <- getRequiredClass "CNInstantMessageAddress"
    sendClassMessage cls' localizedStringForKeySelector (toNSString key)

-- | Returns a user displayable service name.
--
-- ObjC selector: @+ localizedStringForService:@
localizedStringForService :: IsNSString service => service -> IO (Id NSString)
localizedStringForService service =
  do
    cls' <- getRequiredClass "CNInstantMessageAddress"
    sendClassMessage cls' localizedStringForServiceSelector (toNSString service)

-- | @- username@
username :: IsCNInstantMessageAddress cnInstantMessageAddress => cnInstantMessageAddress -> IO (Id NSString)
username cnInstantMessageAddress =
  sendMessage cnInstantMessageAddress usernameSelector

-- | @- service@
service :: IsCNInstantMessageAddress cnInstantMessageAddress => cnInstantMessageAddress -> IO (Id NSString)
service cnInstantMessageAddress =
  sendMessage cnInstantMessageAddress serviceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUsername:service:@
initWithUsername_serviceSelector :: Selector '[Id NSString, Id NSString] (Id CNInstantMessageAddress)
initWithUsername_serviceSelector = mkSelector "initWithUsername:service:"

-- | @Selector@ for @localizedStringForKey:@
localizedStringForKeySelector :: Selector '[Id NSString] (Id NSString)
localizedStringForKeySelector = mkSelector "localizedStringForKey:"

-- | @Selector@ for @localizedStringForService:@
localizedStringForServiceSelector :: Selector '[Id NSString] (Id NSString)
localizedStringForServiceSelector = mkSelector "localizedStringForService:"

-- | @Selector@ for @username@
usernameSelector :: Selector '[] (Id NSString)
usernameSelector = mkSelector "username"

-- | @Selector@ for @service@
serviceSelector :: Selector '[] (Id NSString)
serviceSelector = mkSelector "service"

