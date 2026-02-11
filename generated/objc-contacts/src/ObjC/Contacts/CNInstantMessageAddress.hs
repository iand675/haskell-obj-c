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
  , usernameSelector
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

-- | @- initWithUsername:service:@
initWithUsername_service :: (IsCNInstantMessageAddress cnInstantMessageAddress, IsNSString username, IsNSString service) => cnInstantMessageAddress -> username -> service -> IO (Id CNInstantMessageAddress)
initWithUsername_service cnInstantMessageAddress  username service =
withObjCPtr username $ \raw_username ->
  withObjCPtr service $ \raw_service ->
      sendMsg cnInstantMessageAddress (mkSelector "initWithUsername:service:") (retPtr retVoid) [argPtr (castPtr raw_username :: Ptr ()), argPtr (castPtr raw_service :: Ptr ())] >>= ownedObject . castPtr

-- | Returns a user displayable property name.
--
-- ObjC selector: @+ localizedStringForKey:@
localizedStringForKey :: IsNSString key => key -> IO (Id NSString)
localizedStringForKey key =
  do
    cls' <- getRequiredClass "CNInstantMessageAddress"
    withObjCPtr key $ \raw_key ->
      sendClassMsg cls' (mkSelector "localizedStringForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a user displayable service name.
--
-- ObjC selector: @+ localizedStringForService:@
localizedStringForService :: IsNSString service => service -> IO (Id NSString)
localizedStringForService service =
  do
    cls' <- getRequiredClass "CNInstantMessageAddress"
    withObjCPtr service $ \raw_service ->
      sendClassMsg cls' (mkSelector "localizedStringForService:") (retPtr retVoid) [argPtr (castPtr raw_service :: Ptr ())] >>= retainedObject . castPtr

-- | @- username@
username :: IsCNInstantMessageAddress cnInstantMessageAddress => cnInstantMessageAddress -> IO (Id NSString)
username cnInstantMessageAddress  =
  sendMsg cnInstantMessageAddress (mkSelector "username") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- service@
service :: IsCNInstantMessageAddress cnInstantMessageAddress => cnInstantMessageAddress -> IO (Id NSString)
service cnInstantMessageAddress  =
  sendMsg cnInstantMessageAddress (mkSelector "service") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUsername:service:@
initWithUsername_serviceSelector :: Selector
initWithUsername_serviceSelector = mkSelector "initWithUsername:service:"

-- | @Selector@ for @localizedStringForKey:@
localizedStringForKeySelector :: Selector
localizedStringForKeySelector = mkSelector "localizedStringForKey:"

-- | @Selector@ for @localizedStringForService:@
localizedStringForServiceSelector :: Selector
localizedStringForServiceSelector = mkSelector "localizedStringForService:"

-- | @Selector@ for @username@
usernameSelector :: Selector
usernameSelector = mkSelector "username"

-- | @Selector@ for @service@
serviceSelector :: Selector
serviceSelector = mkSelector "service"

