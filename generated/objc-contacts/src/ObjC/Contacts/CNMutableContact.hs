{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A mutable value object representing a contact.
--
-- CNMutableContact is not thread safe. If this is a mutable copy of CNContact then it will throw CNContactPropertyNotFetchedExceptionName when accessing a property that was not fetched for the CNContact.
--
-- Note: To remove properties when saving a mutable contact, set string properties and array properties to empty values. Set other properties to nil.
--
-- Generated bindings for @CNMutableContact@.
module ObjC.Contacts.CNMutableContact
  ( CNMutableContact
  , IsCNMutableContact(..)
  , contactType
  , setContactType
  , namePrefix
  , setNamePrefix
  , givenName
  , setGivenName
  , middleName
  , setMiddleName
  , familyName
  , setFamilyName
  , previousFamilyName
  , setPreviousFamilyName
  , nameSuffix
  , setNameSuffix
  , nickname
  , setNickname
  , organizationName
  , setOrganizationName
  , departmentName
  , setDepartmentName
  , jobTitle
  , setJobTitle
  , phoneticGivenName
  , setPhoneticGivenName
  , phoneticMiddleName
  , setPhoneticMiddleName
  , phoneticFamilyName
  , setPhoneticFamilyName
  , phoneticOrganizationName
  , setPhoneticOrganizationName
  , note
  , setNote
  , imageData
  , setImageData
  , phoneNumbers
  , setPhoneNumbers
  , emailAddresses
  , setEmailAddresses
  , postalAddresses
  , setPostalAddresses
  , urlAddresses
  , setUrlAddresses
  , contactRelations
  , setContactRelations
  , socialProfiles
  , setSocialProfiles
  , instantMessageAddresses
  , setInstantMessageAddresses
  , birthday
  , setBirthday
  , nonGregorianBirthday
  , setNonGregorianBirthday
  , dates
  , setDates
  , contactTypeSelector
  , setContactTypeSelector
  , namePrefixSelector
  , setNamePrefixSelector
  , givenNameSelector
  , setGivenNameSelector
  , middleNameSelector
  , setMiddleNameSelector
  , familyNameSelector
  , setFamilyNameSelector
  , previousFamilyNameSelector
  , setPreviousFamilyNameSelector
  , nameSuffixSelector
  , setNameSuffixSelector
  , nicknameSelector
  , setNicknameSelector
  , organizationNameSelector
  , setOrganizationNameSelector
  , departmentNameSelector
  , setDepartmentNameSelector
  , jobTitleSelector
  , setJobTitleSelector
  , phoneticGivenNameSelector
  , setPhoneticGivenNameSelector
  , phoneticMiddleNameSelector
  , setPhoneticMiddleNameSelector
  , phoneticFamilyNameSelector
  , setPhoneticFamilyNameSelector
  , phoneticOrganizationNameSelector
  , setPhoneticOrganizationNameSelector
  , noteSelector
  , setNoteSelector
  , imageDataSelector
  , setImageDataSelector
  , phoneNumbersSelector
  , setPhoneNumbersSelector
  , emailAddressesSelector
  , setEmailAddressesSelector
  , postalAddressesSelector
  , setPostalAddressesSelector
  , urlAddressesSelector
  , setUrlAddressesSelector
  , contactRelationsSelector
  , setContactRelationsSelector
  , socialProfilesSelector
  , setSocialProfilesSelector
  , instantMessageAddressesSelector
  , setInstantMessageAddressesSelector
  , birthdaySelector
  , setBirthdaySelector
  , nonGregorianBirthdaySelector
  , setNonGregorianBirthdaySelector
  , datesSelector
  , setDatesSelector

  -- * Enum types
  , CNContactType(CNContactType)
  , pattern CNContactTypePerson
  , pattern CNContactTypeOrganization

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
import ObjC.Contacts.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- contactType@
contactType :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO CNContactType
contactType cnMutableContact  =
  fmap (coerce :: CLong -> CNContactType) $ sendMsg cnMutableContact (mkSelector "contactType") retCLong []

-- | @- setContactType:@
setContactType :: IsCNMutableContact cnMutableContact => cnMutableContact -> CNContactType -> IO ()
setContactType cnMutableContact  value =
  sendMsg cnMutableContact (mkSelector "setContactType:") retVoid [argCLong (coerce value)]

-- | @- namePrefix@
namePrefix :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
namePrefix cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "namePrefix") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNamePrefix:@
setNamePrefix :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setNamePrefix cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setNamePrefix:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- givenName@
givenName :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
givenName cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "givenName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGivenName:@
setGivenName :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setGivenName cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setGivenName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- middleName@
middleName :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
middleName cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "middleName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMiddleName:@
setMiddleName :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setMiddleName cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setMiddleName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- familyName@
familyName :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
familyName cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "familyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFamilyName:@
setFamilyName :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setFamilyName cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setFamilyName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- previousFamilyName@
previousFamilyName :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
previousFamilyName cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "previousFamilyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPreviousFamilyName:@
setPreviousFamilyName :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setPreviousFamilyName cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setPreviousFamilyName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nameSuffix@
nameSuffix :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
nameSuffix cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "nameSuffix") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNameSuffix:@
setNameSuffix :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setNameSuffix cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setNameSuffix:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nickname@
nickname :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
nickname cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "nickname") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNickname:@
setNickname :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setNickname cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setNickname:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- organizationName@
organizationName :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
organizationName cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "organizationName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOrganizationName:@
setOrganizationName :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setOrganizationName cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setOrganizationName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- departmentName@
departmentName :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
departmentName cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "departmentName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDepartmentName:@
setDepartmentName :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setDepartmentName cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setDepartmentName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- jobTitle@
jobTitle :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
jobTitle cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "jobTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setJobTitle:@
setJobTitle :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setJobTitle cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setJobTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- phoneticGivenName@
phoneticGivenName :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
phoneticGivenName cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "phoneticGivenName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPhoneticGivenName:@
setPhoneticGivenName :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setPhoneticGivenName cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setPhoneticGivenName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- phoneticMiddleName@
phoneticMiddleName :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
phoneticMiddleName cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "phoneticMiddleName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPhoneticMiddleName:@
setPhoneticMiddleName :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setPhoneticMiddleName cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setPhoneticMiddleName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- phoneticFamilyName@
phoneticFamilyName :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
phoneticFamilyName cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "phoneticFamilyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPhoneticFamilyName:@
setPhoneticFamilyName :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setPhoneticFamilyName cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setPhoneticFamilyName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- phoneticOrganizationName@
phoneticOrganizationName :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
phoneticOrganizationName cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "phoneticOrganizationName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPhoneticOrganizationName:@
setPhoneticOrganizationName :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setPhoneticOrganizationName cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setPhoneticOrganizationName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- note@
note :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
note cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "note") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNote:@
setNote :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setNote cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setNote:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- imageData@
imageData :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSData)
imageData cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "imageData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImageData:@
setImageData :: (IsCNMutableContact cnMutableContact, IsNSData value) => cnMutableContact -> value -> IO ()
setImageData cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setImageData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- phoneNumbers@
phoneNumbers :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSArray)
phoneNumbers cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "phoneNumbers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPhoneNumbers:@
setPhoneNumbers :: (IsCNMutableContact cnMutableContact, IsNSArray value) => cnMutableContact -> value -> IO ()
setPhoneNumbers cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setPhoneNumbers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- emailAddresses@
emailAddresses :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSArray)
emailAddresses cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "emailAddresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEmailAddresses:@
setEmailAddresses :: (IsCNMutableContact cnMutableContact, IsNSArray value) => cnMutableContact -> value -> IO ()
setEmailAddresses cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setEmailAddresses:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- postalAddresses@
postalAddresses :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSArray)
postalAddresses cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "postalAddresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPostalAddresses:@
setPostalAddresses :: (IsCNMutableContact cnMutableContact, IsNSArray value) => cnMutableContact -> value -> IO ()
setPostalAddresses cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setPostalAddresses:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- urlAddresses@
urlAddresses :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSArray)
urlAddresses cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "urlAddresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUrlAddresses:@
setUrlAddresses :: (IsCNMutableContact cnMutableContact, IsNSArray value) => cnMutableContact -> value -> IO ()
setUrlAddresses cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setUrlAddresses:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contactRelations@
contactRelations :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSArray)
contactRelations cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "contactRelations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContactRelations:@
setContactRelations :: (IsCNMutableContact cnMutableContact, IsNSArray value) => cnMutableContact -> value -> IO ()
setContactRelations cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setContactRelations:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- socialProfiles@
socialProfiles :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSArray)
socialProfiles cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "socialProfiles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSocialProfiles:@
setSocialProfiles :: (IsCNMutableContact cnMutableContact, IsNSArray value) => cnMutableContact -> value -> IO ()
setSocialProfiles cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setSocialProfiles:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- instantMessageAddresses@
instantMessageAddresses :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSArray)
instantMessageAddresses cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "instantMessageAddresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInstantMessageAddresses:@
setInstantMessageAddresses :: (IsCNMutableContact cnMutableContact, IsNSArray value) => cnMutableContact -> value -> IO ()
setInstantMessageAddresses cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setInstantMessageAddresses:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The Gregorian birthday.
--
-- Only uses day, month and year components. Needs to have at least a day and a month.
--
-- ObjC selector: @- birthday@
birthday :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSDateComponents)
birthday cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "birthday") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Gregorian birthday.
--
-- Only uses day, month and year components. Needs to have at least a day and a month.
--
-- ObjC selector: @- setBirthday:@
setBirthday :: (IsCNMutableContact cnMutableContact, IsNSDateComponents value) => cnMutableContact -> value -> IO ()
setBirthday cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setBirthday:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The alternate birthday (Lunisolar).
--
-- Only uses day, month, year and calendar components. Needs to have at least a day and a month. Calendar must be Chinese, Hebrew or Islamic.
--
-- ObjC selector: @- nonGregorianBirthday@
nonGregorianBirthday :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSDateComponents)
nonGregorianBirthday cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "nonGregorianBirthday") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The alternate birthday (Lunisolar).
--
-- Only uses day, month, year and calendar components. Needs to have at least a day and a month. Calendar must be Chinese, Hebrew or Islamic.
--
-- ObjC selector: @- setNonGregorianBirthday:@
setNonGregorianBirthday :: (IsCNMutableContact cnMutableContact, IsNSDateComponents value) => cnMutableContact -> value -> IO ()
setNonGregorianBirthday cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setNonGregorianBirthday:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Other Gregorian dates (anniversaries, etc).
--
-- Only uses day, month and year components. Needs to have at least a day and a month.
--
-- ObjC selector: @- dates@
dates :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSArray)
dates cnMutableContact  =
  sendMsg cnMutableContact (mkSelector "dates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Other Gregorian dates (anniversaries, etc).
--
-- Only uses day, month and year components. Needs to have at least a day and a month.
--
-- ObjC selector: @- setDates:@
setDates :: (IsCNMutableContact cnMutableContact, IsNSArray value) => cnMutableContact -> value -> IO ()
setDates cnMutableContact  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableContact (mkSelector "setDates:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contactType@
contactTypeSelector :: Selector
contactTypeSelector = mkSelector "contactType"

-- | @Selector@ for @setContactType:@
setContactTypeSelector :: Selector
setContactTypeSelector = mkSelector "setContactType:"

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

-- | @Selector@ for @previousFamilyName@
previousFamilyNameSelector :: Selector
previousFamilyNameSelector = mkSelector "previousFamilyName"

-- | @Selector@ for @setPreviousFamilyName:@
setPreviousFamilyNameSelector :: Selector
setPreviousFamilyNameSelector = mkSelector "setPreviousFamilyName:"

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

-- | @Selector@ for @organizationName@
organizationNameSelector :: Selector
organizationNameSelector = mkSelector "organizationName"

-- | @Selector@ for @setOrganizationName:@
setOrganizationNameSelector :: Selector
setOrganizationNameSelector = mkSelector "setOrganizationName:"

-- | @Selector@ for @departmentName@
departmentNameSelector :: Selector
departmentNameSelector = mkSelector "departmentName"

-- | @Selector@ for @setDepartmentName:@
setDepartmentNameSelector :: Selector
setDepartmentNameSelector = mkSelector "setDepartmentName:"

-- | @Selector@ for @jobTitle@
jobTitleSelector :: Selector
jobTitleSelector = mkSelector "jobTitle"

-- | @Selector@ for @setJobTitle:@
setJobTitleSelector :: Selector
setJobTitleSelector = mkSelector "setJobTitle:"

-- | @Selector@ for @phoneticGivenName@
phoneticGivenNameSelector :: Selector
phoneticGivenNameSelector = mkSelector "phoneticGivenName"

-- | @Selector@ for @setPhoneticGivenName:@
setPhoneticGivenNameSelector :: Selector
setPhoneticGivenNameSelector = mkSelector "setPhoneticGivenName:"

-- | @Selector@ for @phoneticMiddleName@
phoneticMiddleNameSelector :: Selector
phoneticMiddleNameSelector = mkSelector "phoneticMiddleName"

-- | @Selector@ for @setPhoneticMiddleName:@
setPhoneticMiddleNameSelector :: Selector
setPhoneticMiddleNameSelector = mkSelector "setPhoneticMiddleName:"

-- | @Selector@ for @phoneticFamilyName@
phoneticFamilyNameSelector :: Selector
phoneticFamilyNameSelector = mkSelector "phoneticFamilyName"

-- | @Selector@ for @setPhoneticFamilyName:@
setPhoneticFamilyNameSelector :: Selector
setPhoneticFamilyNameSelector = mkSelector "setPhoneticFamilyName:"

-- | @Selector@ for @phoneticOrganizationName@
phoneticOrganizationNameSelector :: Selector
phoneticOrganizationNameSelector = mkSelector "phoneticOrganizationName"

-- | @Selector@ for @setPhoneticOrganizationName:@
setPhoneticOrganizationNameSelector :: Selector
setPhoneticOrganizationNameSelector = mkSelector "setPhoneticOrganizationName:"

-- | @Selector@ for @note@
noteSelector :: Selector
noteSelector = mkSelector "note"

-- | @Selector@ for @setNote:@
setNoteSelector :: Selector
setNoteSelector = mkSelector "setNote:"

-- | @Selector@ for @imageData@
imageDataSelector :: Selector
imageDataSelector = mkSelector "imageData"

-- | @Selector@ for @setImageData:@
setImageDataSelector :: Selector
setImageDataSelector = mkSelector "setImageData:"

-- | @Selector@ for @phoneNumbers@
phoneNumbersSelector :: Selector
phoneNumbersSelector = mkSelector "phoneNumbers"

-- | @Selector@ for @setPhoneNumbers:@
setPhoneNumbersSelector :: Selector
setPhoneNumbersSelector = mkSelector "setPhoneNumbers:"

-- | @Selector@ for @emailAddresses@
emailAddressesSelector :: Selector
emailAddressesSelector = mkSelector "emailAddresses"

-- | @Selector@ for @setEmailAddresses:@
setEmailAddressesSelector :: Selector
setEmailAddressesSelector = mkSelector "setEmailAddresses:"

-- | @Selector@ for @postalAddresses@
postalAddressesSelector :: Selector
postalAddressesSelector = mkSelector "postalAddresses"

-- | @Selector@ for @setPostalAddresses:@
setPostalAddressesSelector :: Selector
setPostalAddressesSelector = mkSelector "setPostalAddresses:"

-- | @Selector@ for @urlAddresses@
urlAddressesSelector :: Selector
urlAddressesSelector = mkSelector "urlAddresses"

-- | @Selector@ for @setUrlAddresses:@
setUrlAddressesSelector :: Selector
setUrlAddressesSelector = mkSelector "setUrlAddresses:"

-- | @Selector@ for @contactRelations@
contactRelationsSelector :: Selector
contactRelationsSelector = mkSelector "contactRelations"

-- | @Selector@ for @setContactRelations:@
setContactRelationsSelector :: Selector
setContactRelationsSelector = mkSelector "setContactRelations:"

-- | @Selector@ for @socialProfiles@
socialProfilesSelector :: Selector
socialProfilesSelector = mkSelector "socialProfiles"

-- | @Selector@ for @setSocialProfiles:@
setSocialProfilesSelector :: Selector
setSocialProfilesSelector = mkSelector "setSocialProfiles:"

-- | @Selector@ for @instantMessageAddresses@
instantMessageAddressesSelector :: Selector
instantMessageAddressesSelector = mkSelector "instantMessageAddresses"

-- | @Selector@ for @setInstantMessageAddresses:@
setInstantMessageAddressesSelector :: Selector
setInstantMessageAddressesSelector = mkSelector "setInstantMessageAddresses:"

-- | @Selector@ for @birthday@
birthdaySelector :: Selector
birthdaySelector = mkSelector "birthday"

-- | @Selector@ for @setBirthday:@
setBirthdaySelector :: Selector
setBirthdaySelector = mkSelector "setBirthday:"

-- | @Selector@ for @nonGregorianBirthday@
nonGregorianBirthdaySelector :: Selector
nonGregorianBirthdaySelector = mkSelector "nonGregorianBirthday"

-- | @Selector@ for @setNonGregorianBirthday:@
setNonGregorianBirthdaySelector :: Selector
setNonGregorianBirthdaySelector = mkSelector "setNonGregorianBirthday:"

-- | @Selector@ for @dates@
datesSelector :: Selector
datesSelector = mkSelector "dates"

-- | @Selector@ for @setDates:@
setDatesSelector :: Selector
setDatesSelector = mkSelector "setDates:"

