{-# LANGUAGE DataKinds #-}
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
  , familyNameSelector
  , givenNameSelector
  , middleNameSelector
  , namePrefixSelector
  , nameSuffixSelector
  , nicknameSelector
  , phoneticRepresentationSelector
  , setFamilyNameSelector
  , setGivenNameSelector
  , setMiddleNameSelector
  , setNamePrefixSelector
  , setNameSuffixSelector
  , setNicknameSelector
  , setPhoneticRepresentationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- namePrefix@
namePrefix :: IsNSPersonNameComponents nsPersonNameComponents => nsPersonNameComponents -> IO (Id NSString)
namePrefix nsPersonNameComponents =
  sendMessage nsPersonNameComponents namePrefixSelector

-- | @- setNamePrefix:@
setNamePrefix :: (IsNSPersonNameComponents nsPersonNameComponents, IsNSString value) => nsPersonNameComponents -> value -> IO ()
setNamePrefix nsPersonNameComponents value =
  sendMessage nsPersonNameComponents setNamePrefixSelector (toNSString value)

-- | @- givenName@
givenName :: IsNSPersonNameComponents nsPersonNameComponents => nsPersonNameComponents -> IO (Id NSString)
givenName nsPersonNameComponents =
  sendMessage nsPersonNameComponents givenNameSelector

-- | @- setGivenName:@
setGivenName :: (IsNSPersonNameComponents nsPersonNameComponents, IsNSString value) => nsPersonNameComponents -> value -> IO ()
setGivenName nsPersonNameComponents value =
  sendMessage nsPersonNameComponents setGivenNameSelector (toNSString value)

-- | @- middleName@
middleName :: IsNSPersonNameComponents nsPersonNameComponents => nsPersonNameComponents -> IO (Id NSString)
middleName nsPersonNameComponents =
  sendMessage nsPersonNameComponents middleNameSelector

-- | @- setMiddleName:@
setMiddleName :: (IsNSPersonNameComponents nsPersonNameComponents, IsNSString value) => nsPersonNameComponents -> value -> IO ()
setMiddleName nsPersonNameComponents value =
  sendMessage nsPersonNameComponents setMiddleNameSelector (toNSString value)

-- | @- familyName@
familyName :: IsNSPersonNameComponents nsPersonNameComponents => nsPersonNameComponents -> IO (Id NSString)
familyName nsPersonNameComponents =
  sendMessage nsPersonNameComponents familyNameSelector

-- | @- setFamilyName:@
setFamilyName :: (IsNSPersonNameComponents nsPersonNameComponents, IsNSString value) => nsPersonNameComponents -> value -> IO ()
setFamilyName nsPersonNameComponents value =
  sendMessage nsPersonNameComponents setFamilyNameSelector (toNSString value)

-- | @- nameSuffix@
nameSuffix :: IsNSPersonNameComponents nsPersonNameComponents => nsPersonNameComponents -> IO (Id NSString)
nameSuffix nsPersonNameComponents =
  sendMessage nsPersonNameComponents nameSuffixSelector

-- | @- setNameSuffix:@
setNameSuffix :: (IsNSPersonNameComponents nsPersonNameComponents, IsNSString value) => nsPersonNameComponents -> value -> IO ()
setNameSuffix nsPersonNameComponents value =
  sendMessage nsPersonNameComponents setNameSuffixSelector (toNSString value)

-- | @- nickname@
nickname :: IsNSPersonNameComponents nsPersonNameComponents => nsPersonNameComponents -> IO (Id NSString)
nickname nsPersonNameComponents =
  sendMessage nsPersonNameComponents nicknameSelector

-- | @- setNickname:@
setNickname :: (IsNSPersonNameComponents nsPersonNameComponents, IsNSString value) => nsPersonNameComponents -> value -> IO ()
setNickname nsPersonNameComponents value =
  sendMessage nsPersonNameComponents setNicknameSelector (toNSString value)

-- | @- phoneticRepresentation@
phoneticRepresentation :: IsNSPersonNameComponents nsPersonNameComponents => nsPersonNameComponents -> IO (Id NSPersonNameComponents)
phoneticRepresentation nsPersonNameComponents =
  sendMessage nsPersonNameComponents phoneticRepresentationSelector

-- | @- setPhoneticRepresentation:@
setPhoneticRepresentation :: (IsNSPersonNameComponents nsPersonNameComponents, IsNSPersonNameComponents value) => nsPersonNameComponents -> value -> IO ()
setPhoneticRepresentation nsPersonNameComponents value =
  sendMessage nsPersonNameComponents setPhoneticRepresentationSelector (toNSPersonNameComponents value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @namePrefix@
namePrefixSelector :: Selector '[] (Id NSString)
namePrefixSelector = mkSelector "namePrefix"

-- | @Selector@ for @setNamePrefix:@
setNamePrefixSelector :: Selector '[Id NSString] ()
setNamePrefixSelector = mkSelector "setNamePrefix:"

-- | @Selector@ for @givenName@
givenNameSelector :: Selector '[] (Id NSString)
givenNameSelector = mkSelector "givenName"

-- | @Selector@ for @setGivenName:@
setGivenNameSelector :: Selector '[Id NSString] ()
setGivenNameSelector = mkSelector "setGivenName:"

-- | @Selector@ for @middleName@
middleNameSelector :: Selector '[] (Id NSString)
middleNameSelector = mkSelector "middleName"

-- | @Selector@ for @setMiddleName:@
setMiddleNameSelector :: Selector '[Id NSString] ()
setMiddleNameSelector = mkSelector "setMiddleName:"

-- | @Selector@ for @familyName@
familyNameSelector :: Selector '[] (Id NSString)
familyNameSelector = mkSelector "familyName"

-- | @Selector@ for @setFamilyName:@
setFamilyNameSelector :: Selector '[Id NSString] ()
setFamilyNameSelector = mkSelector "setFamilyName:"

-- | @Selector@ for @nameSuffix@
nameSuffixSelector :: Selector '[] (Id NSString)
nameSuffixSelector = mkSelector "nameSuffix"

-- | @Selector@ for @setNameSuffix:@
setNameSuffixSelector :: Selector '[Id NSString] ()
setNameSuffixSelector = mkSelector "setNameSuffix:"

-- | @Selector@ for @nickname@
nicknameSelector :: Selector '[] (Id NSString)
nicknameSelector = mkSelector "nickname"

-- | @Selector@ for @setNickname:@
setNicknameSelector :: Selector '[Id NSString] ()
setNicknameSelector = mkSelector "setNickname:"

-- | @Selector@ for @phoneticRepresentation@
phoneticRepresentationSelector :: Selector '[] (Id NSPersonNameComponents)
phoneticRepresentationSelector = mkSelector "phoneticRepresentation"

-- | @Selector@ for @setPhoneticRepresentation:@
setPhoneticRepresentationSelector :: Selector '[Id NSPersonNameComponents] ()
setPhoneticRepresentationSelector = mkSelector "setPhoneticRepresentation:"

