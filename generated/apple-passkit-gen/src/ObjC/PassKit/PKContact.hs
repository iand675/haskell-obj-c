{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKContact@.
module ObjC.PassKit.PKContact
  ( PKContact
  , IsPKContact(..)
  , name
  , setName
  , emailAddress
  , setEmailAddress
  , supplementarySubLocality
  , setSupplementarySubLocality
  , emailAddressSelector
  , nameSelector
  , setEmailAddressSelector
  , setNameSelector
  , setSupplementarySubLocalitySelector
  , supplementarySubLocalitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsPKContact pkContact => pkContact -> IO (Id NSPersonNameComponents)
name pkContact =
  sendMessage pkContact nameSelector

-- | @- setName:@
setName :: (IsPKContact pkContact, IsNSPersonNameComponents value) => pkContact -> value -> IO ()
setName pkContact value =
  sendMessage pkContact setNameSelector (toNSPersonNameComponents value)

-- | @- emailAddress@
emailAddress :: IsPKContact pkContact => pkContact -> IO (Id NSString)
emailAddress pkContact =
  sendMessage pkContact emailAddressSelector

-- | @- setEmailAddress:@
setEmailAddress :: (IsPKContact pkContact, IsNSString value) => pkContact -> value -> IO ()
setEmailAddress pkContact value =
  sendMessage pkContact setEmailAddressSelector (toNSString value)

-- | @- supplementarySubLocality@
supplementarySubLocality :: IsPKContact pkContact => pkContact -> IO (Id NSString)
supplementarySubLocality pkContact =
  sendMessage pkContact supplementarySubLocalitySelector

-- | @- setSupplementarySubLocality:@
setSupplementarySubLocality :: (IsPKContact pkContact, IsNSString value) => pkContact -> value -> IO ()
setSupplementarySubLocality pkContact value =
  sendMessage pkContact setSupplementarySubLocalitySelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSPersonNameComponents)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSPersonNameComponents] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @emailAddress@
emailAddressSelector :: Selector '[] (Id NSString)
emailAddressSelector = mkSelector "emailAddress"

-- | @Selector@ for @setEmailAddress:@
setEmailAddressSelector :: Selector '[Id NSString] ()
setEmailAddressSelector = mkSelector "setEmailAddress:"

-- | @Selector@ for @supplementarySubLocality@
supplementarySubLocalitySelector :: Selector '[] (Id NSString)
supplementarySubLocalitySelector = mkSelector "supplementarySubLocality"

-- | @Selector@ for @setSupplementarySubLocality:@
setSupplementarySubLocalitySelector :: Selector '[Id NSString] ()
setSupplementarySubLocalitySelector = mkSelector "setSupplementarySubLocality:"

