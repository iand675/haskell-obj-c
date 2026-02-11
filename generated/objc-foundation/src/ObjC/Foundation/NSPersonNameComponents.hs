{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPersonNameComponents@.
module ObjC.Foundation.NSPersonNameComponents
  ( NSPersonNameComponents
  , IsNSPersonNameComponents(..)
  , namePrefix
  , setNamePrefix
  , givenName
  , setGivenName
  , middleName
  , setMiddleName
  , familyName
  , setFamilyName
  , nameSuffix
  , setNameSuffix
  , nickname
  , setNickname
  , phoneticRepresentation
  , setPhoneticRepresentation
  , namePrefixSelector
  , setNamePrefixSelector
  , givenNameSelector
  , setGivenNameSelector
  , middleNameSelector
  , setMiddleNameSelector
  , familyNameSelector
  , setFamilyNameSelector
  , nameSuffixSelector
  , setNameSuffixSelector
  , nicknameSelector
  , setNicknameSelector
  , phoneticRepresentationSelector
  , setPhoneticRepresentationSelector


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

import ObjC.Foundation.Internal.Classes

-- | @- namePrefix@
namePrefix :: IsNSPersonNameComponents nsPersonNameComponents => nsPersonNameComponents -> IO (Id NSString)
namePrefix nsPersonNameComponents  =
  sendMsg nsPersonNameComponents (mkSelector "namePrefix") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNamePrefix:@
setNamePrefix :: (IsNSPersonNameComponents nsPersonNameComponents, IsNSString value) => nsPersonNameComponents -> value -> IO ()
setNamePrefix nsPersonNameComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPersonNameComponents (mkSelector "setNamePrefix:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- givenName@
givenName :: IsNSPersonNameComponents nsPersonNameComponents => nsPersonNameComponents -> IO (Id NSString)
givenName nsPersonNameComponents  =
  sendMsg nsPersonNameComponents (mkSelector "givenName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGivenName:@
setGivenName :: (IsNSPersonNameComponents nsPersonNameComponents, IsNSString value) => nsPersonNameComponents -> value -> IO ()
setGivenName nsPersonNameComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPersonNameComponents (mkSelector "setGivenName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- middleName@
middleName :: IsNSPersonNameComponents nsPersonNameComponents => nsPersonNameComponents -> IO (Id NSString)
middleName nsPersonNameComponents  =
  sendMsg nsPersonNameComponents (mkSelector "middleName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMiddleName:@
setMiddleName :: (IsNSPersonNameComponents nsPersonNameComponents, IsNSString value) => nsPersonNameComponents -> value -> IO ()
setMiddleName nsPersonNameComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPersonNameComponents (mkSelector "setMiddleName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- familyName@
familyName :: IsNSPersonNameComponents nsPersonNameComponents => nsPersonNameComponents -> IO (Id NSString)
familyName nsPersonNameComponents  =
  sendMsg nsPersonNameComponents (mkSelector "familyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFamilyName:@
setFamilyName :: (IsNSPersonNameComponents nsPersonNameComponents, IsNSString value) => nsPersonNameComponents -> value -> IO ()
setFamilyName nsPersonNameComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPersonNameComponents (mkSelector "setFamilyName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nameSuffix@
nameSuffix :: IsNSPersonNameComponents nsPersonNameComponents => nsPersonNameComponents -> IO (Id NSString)
nameSuffix nsPersonNameComponents  =
  sendMsg nsPersonNameComponents (mkSelector "nameSuffix") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNameSuffix:@
setNameSuffix :: (IsNSPersonNameComponents nsPersonNameComponents, IsNSString value) => nsPersonNameComponents -> value -> IO ()
setNameSuffix nsPersonNameComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPersonNameComponents (mkSelector "setNameSuffix:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nickname@
nickname :: IsNSPersonNameComponents nsPersonNameComponents => nsPersonNameComponents -> IO (Id NSString)
nickname nsPersonNameComponents  =
  sendMsg nsPersonNameComponents (mkSelector "nickname") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNickname:@
setNickname :: (IsNSPersonNameComponents nsPersonNameComponents, IsNSString value) => nsPersonNameComponents -> value -> IO ()
setNickname nsPersonNameComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPersonNameComponents (mkSelector "setNickname:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- phoneticRepresentation@
phoneticRepresentation :: IsNSPersonNameComponents nsPersonNameComponents => nsPersonNameComponents -> IO (Id NSPersonNameComponents)
phoneticRepresentation nsPersonNameComponents  =
  sendMsg nsPersonNameComponents (mkSelector "phoneticRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPhoneticRepresentation:@
setPhoneticRepresentation :: (IsNSPersonNameComponents nsPersonNameComponents, IsNSPersonNameComponents value) => nsPersonNameComponents -> value -> IO ()
setPhoneticRepresentation nsPersonNameComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPersonNameComponents (mkSelector "setPhoneticRepresentation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @namePrefix@
namePrefixSelector :: Selector
namePrefixSelector = mkSelector "namePrefix"

-- | @Selector@ for @setNamePrefix:@
setNamePrefixSelector :: Selector
setNamePrefixSelector = mkSelector "setNamePrefix:"

-- | @Selector@ for @givenName@
givenNameSelector :: Selector
givenNameSelector = mkSelector "givenName"

-- | @Selector@ for @setGivenName:@
setGivenNameSelector :: Selector
setGivenNameSelector = mkSelector "setGivenName:"

-- | @Selector@ for @middleName@
middleNameSelector :: Selector
middleNameSelector = mkSelector "middleName"

-- | @Selector@ for @setMiddleName:@
setMiddleNameSelector :: Selector
setMiddleNameSelector = mkSelector "setMiddleName:"

-- | @Selector@ for @familyName@
familyNameSelector :: Selector
familyNameSelector = mkSelector "familyName"

-- | @Selector@ for @setFamilyName:@
setFamilyNameSelector :: Selector
setFamilyNameSelector = mkSelector "setFamilyName:"

-- | @Selector@ for @nameSuffix@
nameSuffixSelector :: Selector
nameSuffixSelector = mkSelector "nameSuffix"

-- | @Selector@ for @setNameSuffix:@
setNameSuffixSelector :: Selector
setNameSuffixSelector = mkSelector "setNameSuffix:"

-- | @Selector@ for @nickname@
nicknameSelector :: Selector
nicknameSelector = mkSelector "nickname"

-- | @Selector@ for @setNickname:@
setNicknameSelector :: Selector
setNicknameSelector = mkSelector "setNickname:"

-- | @Selector@ for @phoneticRepresentation@
phoneticRepresentationSelector :: Selector
phoneticRepresentationSelector = mkSelector "phoneticRepresentation"

-- | @Selector@ for @setPhoneticRepresentation:@
setPhoneticRepresentationSelector :: Selector
setPhoneticRepresentationSelector = mkSelector "setPhoneticRepresentation:"

