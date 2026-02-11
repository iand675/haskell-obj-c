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
  , nameSelector
  , setNameSelector
  , emailAddressSelector
  , setEmailAddressSelector
  , supplementarySubLocalitySelector
  , setSupplementarySubLocalitySelector


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

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsPKContact pkContact => pkContact -> IO (Id NSPersonNameComponents)
name pkContact  =
  sendMsg pkContact (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsPKContact pkContact, IsNSPersonNameComponents value) => pkContact -> value -> IO ()
setName pkContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkContact (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- emailAddress@
emailAddress :: IsPKContact pkContact => pkContact -> IO (Id NSString)
emailAddress pkContact  =
  sendMsg pkContact (mkSelector "emailAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEmailAddress:@
setEmailAddress :: (IsPKContact pkContact, IsNSString value) => pkContact -> value -> IO ()
setEmailAddress pkContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkContact (mkSelector "setEmailAddress:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- supplementarySubLocality@
supplementarySubLocality :: IsPKContact pkContact => pkContact -> IO (Id NSString)
supplementarySubLocality pkContact  =
  sendMsg pkContact (mkSelector "supplementarySubLocality") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSupplementarySubLocality:@
setSupplementarySubLocality :: (IsPKContact pkContact, IsNSString value) => pkContact -> value -> IO ()
setSupplementarySubLocality pkContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkContact (mkSelector "setSupplementarySubLocality:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @emailAddress@
emailAddressSelector :: Selector
emailAddressSelector = mkSelector "emailAddress"

-- | @Selector@ for @setEmailAddress:@
setEmailAddressSelector :: Selector
setEmailAddressSelector = mkSelector "setEmailAddress:"

-- | @Selector@ for @supplementarySubLocality@
supplementarySubLocalitySelector :: Selector
supplementarySubLocalitySelector = mkSelector "supplementarySubLocality"

-- | @Selector@ for @setSupplementarySubLocality:@
setSupplementarySubLocalitySelector :: Selector
setSupplementarySubLocalitySelector = mkSelector "setSupplementarySubLocality:"

