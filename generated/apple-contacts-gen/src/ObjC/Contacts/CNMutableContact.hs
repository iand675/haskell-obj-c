{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , birthdaySelector
  , contactRelationsSelector
  , contactTypeSelector
  , datesSelector
  , departmentNameSelector
  , emailAddressesSelector
  , familyNameSelector
  , givenNameSelector
  , imageDataSelector
  , instantMessageAddressesSelector
  , jobTitleSelector
  , middleNameSelector
  , namePrefixSelector
  , nameSuffixSelector
  , nicknameSelector
  , nonGregorianBirthdaySelector
  , noteSelector
  , organizationNameSelector
  , phoneNumbersSelector
  , phoneticFamilyNameSelector
  , phoneticGivenNameSelector
  , phoneticMiddleNameSelector
  , phoneticOrganizationNameSelector
  , postalAddressesSelector
  , previousFamilyNameSelector
  , setBirthdaySelector
  , setContactRelationsSelector
  , setContactTypeSelector
  , setDatesSelector
  , setDepartmentNameSelector
  , setEmailAddressesSelector
  , setFamilyNameSelector
  , setGivenNameSelector
  , setImageDataSelector
  , setInstantMessageAddressesSelector
  , setJobTitleSelector
  , setMiddleNameSelector
  , setNamePrefixSelector
  , setNameSuffixSelector
  , setNicknameSelector
  , setNonGregorianBirthdaySelector
  , setNoteSelector
  , setOrganizationNameSelector
  , setPhoneNumbersSelector
  , setPhoneticFamilyNameSelector
  , setPhoneticGivenNameSelector
  , setPhoneticMiddleNameSelector
  , setPhoneticOrganizationNameSelector
  , setPostalAddressesSelector
  , setPreviousFamilyNameSelector
  , setSocialProfilesSelector
  , setUrlAddressesSelector
  , socialProfilesSelector
  , urlAddressesSelector

  -- * Enum types
  , CNContactType(CNContactType)
  , pattern CNContactTypePerson
  , pattern CNContactTypeOrganization

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Contacts.Internal.Classes
import ObjC.Contacts.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- contactType@
contactType :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO CNContactType
contactType cnMutableContact =
  sendMessage cnMutableContact contactTypeSelector

-- | @- setContactType:@
setContactType :: IsCNMutableContact cnMutableContact => cnMutableContact -> CNContactType -> IO ()
setContactType cnMutableContact value =
  sendMessage cnMutableContact setContactTypeSelector value

-- | @- namePrefix@
namePrefix :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
namePrefix cnMutableContact =
  sendMessage cnMutableContact namePrefixSelector

-- | @- setNamePrefix:@
setNamePrefix :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setNamePrefix cnMutableContact value =
  sendMessage cnMutableContact setNamePrefixSelector (toNSString value)

-- | @- givenName@
givenName :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
givenName cnMutableContact =
  sendMessage cnMutableContact givenNameSelector

-- | @- setGivenName:@
setGivenName :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setGivenName cnMutableContact value =
  sendMessage cnMutableContact setGivenNameSelector (toNSString value)

-- | @- middleName@
middleName :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
middleName cnMutableContact =
  sendMessage cnMutableContact middleNameSelector

-- | @- setMiddleName:@
setMiddleName :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setMiddleName cnMutableContact value =
  sendMessage cnMutableContact setMiddleNameSelector (toNSString value)

-- | @- familyName@
familyName :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
familyName cnMutableContact =
  sendMessage cnMutableContact familyNameSelector

-- | @- setFamilyName:@
setFamilyName :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setFamilyName cnMutableContact value =
  sendMessage cnMutableContact setFamilyNameSelector (toNSString value)

-- | @- previousFamilyName@
previousFamilyName :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
previousFamilyName cnMutableContact =
  sendMessage cnMutableContact previousFamilyNameSelector

-- | @- setPreviousFamilyName:@
setPreviousFamilyName :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setPreviousFamilyName cnMutableContact value =
  sendMessage cnMutableContact setPreviousFamilyNameSelector (toNSString value)

-- | @- nameSuffix@
nameSuffix :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
nameSuffix cnMutableContact =
  sendMessage cnMutableContact nameSuffixSelector

-- | @- setNameSuffix:@
setNameSuffix :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setNameSuffix cnMutableContact value =
  sendMessage cnMutableContact setNameSuffixSelector (toNSString value)

-- | @- nickname@
nickname :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
nickname cnMutableContact =
  sendMessage cnMutableContact nicknameSelector

-- | @- setNickname:@
setNickname :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setNickname cnMutableContact value =
  sendMessage cnMutableContact setNicknameSelector (toNSString value)

-- | @- organizationName@
organizationName :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
organizationName cnMutableContact =
  sendMessage cnMutableContact organizationNameSelector

-- | @- setOrganizationName:@
setOrganizationName :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setOrganizationName cnMutableContact value =
  sendMessage cnMutableContact setOrganizationNameSelector (toNSString value)

-- | @- departmentName@
departmentName :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
departmentName cnMutableContact =
  sendMessage cnMutableContact departmentNameSelector

-- | @- setDepartmentName:@
setDepartmentName :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setDepartmentName cnMutableContact value =
  sendMessage cnMutableContact setDepartmentNameSelector (toNSString value)

-- | @- jobTitle@
jobTitle :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
jobTitle cnMutableContact =
  sendMessage cnMutableContact jobTitleSelector

-- | @- setJobTitle:@
setJobTitle :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setJobTitle cnMutableContact value =
  sendMessage cnMutableContact setJobTitleSelector (toNSString value)

-- | @- phoneticGivenName@
phoneticGivenName :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
phoneticGivenName cnMutableContact =
  sendMessage cnMutableContact phoneticGivenNameSelector

-- | @- setPhoneticGivenName:@
setPhoneticGivenName :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setPhoneticGivenName cnMutableContact value =
  sendMessage cnMutableContact setPhoneticGivenNameSelector (toNSString value)

-- | @- phoneticMiddleName@
phoneticMiddleName :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
phoneticMiddleName cnMutableContact =
  sendMessage cnMutableContact phoneticMiddleNameSelector

-- | @- setPhoneticMiddleName:@
setPhoneticMiddleName :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setPhoneticMiddleName cnMutableContact value =
  sendMessage cnMutableContact setPhoneticMiddleNameSelector (toNSString value)

-- | @- phoneticFamilyName@
phoneticFamilyName :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
phoneticFamilyName cnMutableContact =
  sendMessage cnMutableContact phoneticFamilyNameSelector

-- | @- setPhoneticFamilyName:@
setPhoneticFamilyName :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setPhoneticFamilyName cnMutableContact value =
  sendMessage cnMutableContact setPhoneticFamilyNameSelector (toNSString value)

-- | @- phoneticOrganizationName@
phoneticOrganizationName :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
phoneticOrganizationName cnMutableContact =
  sendMessage cnMutableContact phoneticOrganizationNameSelector

-- | @- setPhoneticOrganizationName:@
setPhoneticOrganizationName :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setPhoneticOrganizationName cnMutableContact value =
  sendMessage cnMutableContact setPhoneticOrganizationNameSelector (toNSString value)

-- | @- note@
note :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSString)
note cnMutableContact =
  sendMessage cnMutableContact noteSelector

-- | @- setNote:@
setNote :: (IsCNMutableContact cnMutableContact, IsNSString value) => cnMutableContact -> value -> IO ()
setNote cnMutableContact value =
  sendMessage cnMutableContact setNoteSelector (toNSString value)

-- | @- imageData@
imageData :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSData)
imageData cnMutableContact =
  sendMessage cnMutableContact imageDataSelector

-- | @- setImageData:@
setImageData :: (IsCNMutableContact cnMutableContact, IsNSData value) => cnMutableContact -> value -> IO ()
setImageData cnMutableContact value =
  sendMessage cnMutableContact setImageDataSelector (toNSData value)

-- | @- phoneNumbers@
phoneNumbers :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSArray)
phoneNumbers cnMutableContact =
  sendMessage cnMutableContact phoneNumbersSelector

-- | @- setPhoneNumbers:@
setPhoneNumbers :: (IsCNMutableContact cnMutableContact, IsNSArray value) => cnMutableContact -> value -> IO ()
setPhoneNumbers cnMutableContact value =
  sendMessage cnMutableContact setPhoneNumbersSelector (toNSArray value)

-- | @- emailAddresses@
emailAddresses :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSArray)
emailAddresses cnMutableContact =
  sendMessage cnMutableContact emailAddressesSelector

-- | @- setEmailAddresses:@
setEmailAddresses :: (IsCNMutableContact cnMutableContact, IsNSArray value) => cnMutableContact -> value -> IO ()
setEmailAddresses cnMutableContact value =
  sendMessage cnMutableContact setEmailAddressesSelector (toNSArray value)

-- | @- postalAddresses@
postalAddresses :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSArray)
postalAddresses cnMutableContact =
  sendMessage cnMutableContact postalAddressesSelector

-- | @- setPostalAddresses:@
setPostalAddresses :: (IsCNMutableContact cnMutableContact, IsNSArray value) => cnMutableContact -> value -> IO ()
setPostalAddresses cnMutableContact value =
  sendMessage cnMutableContact setPostalAddressesSelector (toNSArray value)

-- | @- urlAddresses@
urlAddresses :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSArray)
urlAddresses cnMutableContact =
  sendMessage cnMutableContact urlAddressesSelector

-- | @- setUrlAddresses:@
setUrlAddresses :: (IsCNMutableContact cnMutableContact, IsNSArray value) => cnMutableContact -> value -> IO ()
setUrlAddresses cnMutableContact value =
  sendMessage cnMutableContact setUrlAddressesSelector (toNSArray value)

-- | @- contactRelations@
contactRelations :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSArray)
contactRelations cnMutableContact =
  sendMessage cnMutableContact contactRelationsSelector

-- | @- setContactRelations:@
setContactRelations :: (IsCNMutableContact cnMutableContact, IsNSArray value) => cnMutableContact -> value -> IO ()
setContactRelations cnMutableContact value =
  sendMessage cnMutableContact setContactRelationsSelector (toNSArray value)

-- | @- socialProfiles@
socialProfiles :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSArray)
socialProfiles cnMutableContact =
  sendMessage cnMutableContact socialProfilesSelector

-- | @- setSocialProfiles:@
setSocialProfiles :: (IsCNMutableContact cnMutableContact, IsNSArray value) => cnMutableContact -> value -> IO ()
setSocialProfiles cnMutableContact value =
  sendMessage cnMutableContact setSocialProfilesSelector (toNSArray value)

-- | @- instantMessageAddresses@
instantMessageAddresses :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSArray)
instantMessageAddresses cnMutableContact =
  sendMessage cnMutableContact instantMessageAddressesSelector

-- | @- setInstantMessageAddresses:@
setInstantMessageAddresses :: (IsCNMutableContact cnMutableContact, IsNSArray value) => cnMutableContact -> value -> IO ()
setInstantMessageAddresses cnMutableContact value =
  sendMessage cnMutableContact setInstantMessageAddressesSelector (toNSArray value)

-- | The Gregorian birthday.
--
-- Only uses day, month and year components. Needs to have at least a day and a month.
--
-- ObjC selector: @- birthday@
birthday :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSDateComponents)
birthday cnMutableContact =
  sendMessage cnMutableContact birthdaySelector

-- | The Gregorian birthday.
--
-- Only uses day, month and year components. Needs to have at least a day and a month.
--
-- ObjC selector: @- setBirthday:@
setBirthday :: (IsCNMutableContact cnMutableContact, IsNSDateComponents value) => cnMutableContact -> value -> IO ()
setBirthday cnMutableContact value =
  sendMessage cnMutableContact setBirthdaySelector (toNSDateComponents value)

-- | The alternate birthday (Lunisolar).
--
-- Only uses day, month, year and calendar components. Needs to have at least a day and a month. Calendar must be Chinese, Hebrew or Islamic.
--
-- ObjC selector: @- nonGregorianBirthday@
nonGregorianBirthday :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSDateComponents)
nonGregorianBirthday cnMutableContact =
  sendMessage cnMutableContact nonGregorianBirthdaySelector

-- | The alternate birthday (Lunisolar).
--
-- Only uses day, month, year and calendar components. Needs to have at least a day and a month. Calendar must be Chinese, Hebrew or Islamic.
--
-- ObjC selector: @- setNonGregorianBirthday:@
setNonGregorianBirthday :: (IsCNMutableContact cnMutableContact, IsNSDateComponents value) => cnMutableContact -> value -> IO ()
setNonGregorianBirthday cnMutableContact value =
  sendMessage cnMutableContact setNonGregorianBirthdaySelector (toNSDateComponents value)

-- | Other Gregorian dates (anniversaries, etc).
--
-- Only uses day, month and year components. Needs to have at least a day and a month.
--
-- ObjC selector: @- dates@
dates :: IsCNMutableContact cnMutableContact => cnMutableContact -> IO (Id NSArray)
dates cnMutableContact =
  sendMessage cnMutableContact datesSelector

-- | Other Gregorian dates (anniversaries, etc).
--
-- Only uses day, month and year components. Needs to have at least a day and a month.
--
-- ObjC selector: @- setDates:@
setDates :: (IsCNMutableContact cnMutableContact, IsNSArray value) => cnMutableContact -> value -> IO ()
setDates cnMutableContact value =
  sendMessage cnMutableContact setDatesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contactType@
contactTypeSelector :: Selector '[] CNContactType
contactTypeSelector = mkSelector "contactType"

-- | @Selector@ for @setContactType:@
setContactTypeSelector :: Selector '[CNContactType] ()
setContactTypeSelector = mkSelector "setContactType:"

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

-- | @Selector@ for @previousFamilyName@
previousFamilyNameSelector :: Selector '[] (Id NSString)
previousFamilyNameSelector = mkSelector "previousFamilyName"

-- | @Selector@ for @setPreviousFamilyName:@
setPreviousFamilyNameSelector :: Selector '[Id NSString] ()
setPreviousFamilyNameSelector = mkSelector "setPreviousFamilyName:"

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

-- | @Selector@ for @organizationName@
organizationNameSelector :: Selector '[] (Id NSString)
organizationNameSelector = mkSelector "organizationName"

-- | @Selector@ for @setOrganizationName:@
setOrganizationNameSelector :: Selector '[Id NSString] ()
setOrganizationNameSelector = mkSelector "setOrganizationName:"

-- | @Selector@ for @departmentName@
departmentNameSelector :: Selector '[] (Id NSString)
departmentNameSelector = mkSelector "departmentName"

-- | @Selector@ for @setDepartmentName:@
setDepartmentNameSelector :: Selector '[Id NSString] ()
setDepartmentNameSelector = mkSelector "setDepartmentName:"

-- | @Selector@ for @jobTitle@
jobTitleSelector :: Selector '[] (Id NSString)
jobTitleSelector = mkSelector "jobTitle"

-- | @Selector@ for @setJobTitle:@
setJobTitleSelector :: Selector '[Id NSString] ()
setJobTitleSelector = mkSelector "setJobTitle:"

-- | @Selector@ for @phoneticGivenName@
phoneticGivenNameSelector :: Selector '[] (Id NSString)
phoneticGivenNameSelector = mkSelector "phoneticGivenName"

-- | @Selector@ for @setPhoneticGivenName:@
setPhoneticGivenNameSelector :: Selector '[Id NSString] ()
setPhoneticGivenNameSelector = mkSelector "setPhoneticGivenName:"

-- | @Selector@ for @phoneticMiddleName@
phoneticMiddleNameSelector :: Selector '[] (Id NSString)
phoneticMiddleNameSelector = mkSelector "phoneticMiddleName"

-- | @Selector@ for @setPhoneticMiddleName:@
setPhoneticMiddleNameSelector :: Selector '[Id NSString] ()
setPhoneticMiddleNameSelector = mkSelector "setPhoneticMiddleName:"

-- | @Selector@ for @phoneticFamilyName@
phoneticFamilyNameSelector :: Selector '[] (Id NSString)
phoneticFamilyNameSelector = mkSelector "phoneticFamilyName"

-- | @Selector@ for @setPhoneticFamilyName:@
setPhoneticFamilyNameSelector :: Selector '[Id NSString] ()
setPhoneticFamilyNameSelector = mkSelector "setPhoneticFamilyName:"

-- | @Selector@ for @phoneticOrganizationName@
phoneticOrganizationNameSelector :: Selector '[] (Id NSString)
phoneticOrganizationNameSelector = mkSelector "phoneticOrganizationName"

-- | @Selector@ for @setPhoneticOrganizationName:@
setPhoneticOrganizationNameSelector :: Selector '[Id NSString] ()
setPhoneticOrganizationNameSelector = mkSelector "setPhoneticOrganizationName:"

-- | @Selector@ for @note@
noteSelector :: Selector '[] (Id NSString)
noteSelector = mkSelector "note"

-- | @Selector@ for @setNote:@
setNoteSelector :: Selector '[Id NSString] ()
setNoteSelector = mkSelector "setNote:"

-- | @Selector@ for @imageData@
imageDataSelector :: Selector '[] (Id NSData)
imageDataSelector = mkSelector "imageData"

-- | @Selector@ for @setImageData:@
setImageDataSelector :: Selector '[Id NSData] ()
setImageDataSelector = mkSelector "setImageData:"

-- | @Selector@ for @phoneNumbers@
phoneNumbersSelector :: Selector '[] (Id NSArray)
phoneNumbersSelector = mkSelector "phoneNumbers"

-- | @Selector@ for @setPhoneNumbers:@
setPhoneNumbersSelector :: Selector '[Id NSArray] ()
setPhoneNumbersSelector = mkSelector "setPhoneNumbers:"

-- | @Selector@ for @emailAddresses@
emailAddressesSelector :: Selector '[] (Id NSArray)
emailAddressesSelector = mkSelector "emailAddresses"

-- | @Selector@ for @setEmailAddresses:@
setEmailAddressesSelector :: Selector '[Id NSArray] ()
setEmailAddressesSelector = mkSelector "setEmailAddresses:"

-- | @Selector@ for @postalAddresses@
postalAddressesSelector :: Selector '[] (Id NSArray)
postalAddressesSelector = mkSelector "postalAddresses"

-- | @Selector@ for @setPostalAddresses:@
setPostalAddressesSelector :: Selector '[Id NSArray] ()
setPostalAddressesSelector = mkSelector "setPostalAddresses:"

-- | @Selector@ for @urlAddresses@
urlAddressesSelector :: Selector '[] (Id NSArray)
urlAddressesSelector = mkSelector "urlAddresses"

-- | @Selector@ for @setUrlAddresses:@
setUrlAddressesSelector :: Selector '[Id NSArray] ()
setUrlAddressesSelector = mkSelector "setUrlAddresses:"

-- | @Selector@ for @contactRelations@
contactRelationsSelector :: Selector '[] (Id NSArray)
contactRelationsSelector = mkSelector "contactRelations"

-- | @Selector@ for @setContactRelations:@
setContactRelationsSelector :: Selector '[Id NSArray] ()
setContactRelationsSelector = mkSelector "setContactRelations:"

-- | @Selector@ for @socialProfiles@
socialProfilesSelector :: Selector '[] (Id NSArray)
socialProfilesSelector = mkSelector "socialProfiles"

-- | @Selector@ for @setSocialProfiles:@
setSocialProfilesSelector :: Selector '[Id NSArray] ()
setSocialProfilesSelector = mkSelector "setSocialProfiles:"

-- | @Selector@ for @instantMessageAddresses@
instantMessageAddressesSelector :: Selector '[] (Id NSArray)
instantMessageAddressesSelector = mkSelector "instantMessageAddresses"

-- | @Selector@ for @setInstantMessageAddresses:@
setInstantMessageAddressesSelector :: Selector '[Id NSArray] ()
setInstantMessageAddressesSelector = mkSelector "setInstantMessageAddresses:"

-- | @Selector@ for @birthday@
birthdaySelector :: Selector '[] (Id NSDateComponents)
birthdaySelector = mkSelector "birthday"

-- | @Selector@ for @setBirthday:@
setBirthdaySelector :: Selector '[Id NSDateComponents] ()
setBirthdaySelector = mkSelector "setBirthday:"

-- | @Selector@ for @nonGregorianBirthday@
nonGregorianBirthdaySelector :: Selector '[] (Id NSDateComponents)
nonGregorianBirthdaySelector = mkSelector "nonGregorianBirthday"

-- | @Selector@ for @setNonGregorianBirthday:@
setNonGregorianBirthdaySelector :: Selector '[Id NSDateComponents] ()
setNonGregorianBirthdaySelector = mkSelector "setNonGregorianBirthday:"

-- | @Selector@ for @dates@
datesSelector :: Selector '[] (Id NSArray)
datesSelector = mkSelector "dates"

-- | @Selector@ for @setDates:@
setDatesSelector :: Selector '[Id NSArray] ()
setDatesSelector = mkSelector "setDates:"

