{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An immutable value object representing a contact.
--
-- CNContact is thread safe.
--
-- If using a CNContact instance where you are not certain of the keys that were fetched, use isKeyAvailable: or areKeysAvailable:. If these return NO you need to refetch the contact by the contact identifier with the keys you want to fetch. Accessing a property that was not fetched will throw CNContactPropertyNotFetchedExceptionName.
--
-- Generated bindings for @CNContact@.
module ObjC.Contacts.CNContact
  ( CNContact
  , IsCNContact(..)
  , isKeyAvailable
  , areKeysAvailable
  , localizedStringForKey
  , comparatorForNameSortOrder
  , descriptorForAllComparatorKeys
  , isUnifiedWithContactWithIdentifier
  , predicateForContactsMatchingName
  , predicateForContactsMatchingEmailAddress
  , predicateForContactsMatchingPhoneNumber
  , predicateForContactsWithIdentifiers
  , predicateForContactsInGroupWithIdentifier
  , predicateForContactsInContainerWithIdentifier
  , identifier
  , contactType
  , namePrefix
  , givenName
  , middleName
  , familyName
  , previousFamilyName
  , nameSuffix
  , nickname
  , organizationName
  , departmentName
  , jobTitle
  , phoneticGivenName
  , phoneticMiddleName
  , phoneticFamilyName
  , phoneticOrganizationName
  , note
  , imageData
  , thumbnailImageData
  , imageDataAvailable
  , phoneNumbers
  , emailAddresses
  , postalAddresses
  , urlAddresses
  , contactRelations
  , socialProfiles
  , instantMessageAddresses
  , birthday
  , nonGregorianBirthday
  , dates
  , areKeysAvailableSelector
  , birthdaySelector
  , comparatorForNameSortOrderSelector
  , contactRelationsSelector
  , contactTypeSelector
  , datesSelector
  , departmentNameSelector
  , descriptorForAllComparatorKeysSelector
  , emailAddressesSelector
  , familyNameSelector
  , givenNameSelector
  , identifierSelector
  , imageDataAvailableSelector
  , imageDataSelector
  , instantMessageAddressesSelector
  , isKeyAvailableSelector
  , isUnifiedWithContactWithIdentifierSelector
  , jobTitleSelector
  , localizedStringForKeySelector
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
  , predicateForContactsInContainerWithIdentifierSelector
  , predicateForContactsInGroupWithIdentifierSelector
  , predicateForContactsMatchingEmailAddressSelector
  , predicateForContactsMatchingNameSelector
  , predicateForContactsMatchingPhoneNumberSelector
  , predicateForContactsWithIdentifiersSelector
  , previousFamilyNameSelector
  , socialProfilesSelector
  , thumbnailImageDataSelector
  , urlAddressesSelector

  -- * Enum types
  , CNContactSortOrder(CNContactSortOrder)
  , pattern CNContactSortOrderNone
  , pattern CNContactSortOrderUserDefault
  , pattern CNContactSortOrderGivenName
  , pattern CNContactSortOrderFamilyName
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

-- | Returns YES if the value for the specified key was fetched.
--
-- ObjC selector: @- isKeyAvailable:@
isKeyAvailable :: (IsCNContact cnContact, IsNSString key) => cnContact -> key -> IO Bool
isKeyAvailable cnContact key =
  sendMessage cnContact isKeyAvailableSelector (toNSString key)

-- | Returns YES if the values for the keys specified by all the descriptors were fetched.
--
-- ObjC selector: @- areKeysAvailable:@
areKeysAvailable :: (IsCNContact cnContact, IsNSArray keyDescriptors) => cnContact -> keyDescriptors -> IO Bool
areKeysAvailable cnContact keyDescriptors =
  sendMessage cnContact areKeysAvailableSelector (toNSArray keyDescriptors)

-- | Returns a user displayable property name.
--
-- ObjC selector: @+ localizedStringForKey:@
localizedStringForKey :: IsNSString key => key -> IO (Id NSString)
localizedStringForKey key =
  do
    cls' <- getRequiredClass "CNContact"
    sendClassMessage cls' localizedStringForKeySelector (toNSString key)

-- | The contact comparator for a given sort order.
--
-- ObjC selector: @+ comparatorForNameSortOrder:@
comparatorForNameSortOrder :: CNContactSortOrder -> IO (Ptr ())
comparatorForNameSortOrder sortOrder =
  do
    cls' <- getRequiredClass "CNContact"
    sendClassMessage cls' comparatorForNameSortOrderSelector sortOrder

-- | Use to fetch all contact keys required for the contact sort comparator.
--
-- ObjC selector: @+ descriptorForAllComparatorKeys@
descriptorForAllComparatorKeys :: IO RawId
descriptorForAllComparatorKeys  =
  do
    cls' <- getRequiredClass "CNContact"
    sendClassMessage cls' descriptorForAllComparatorKeysSelector

-- | Returns YES if the receiver was fetched as a unified contact and includes the contact having contactIdentifier in its unification
--
-- ObjC selector: @- isUnifiedWithContactWithIdentifier:@
isUnifiedWithContactWithIdentifier :: (IsCNContact cnContact, IsNSString contactIdentifier) => cnContact -> contactIdentifier -> IO Bool
isUnifiedWithContactWithIdentifier cnContact contactIdentifier =
  sendMessage cnContact isUnifiedWithContactWithIdentifierSelector (toNSString contactIdentifier)

-- | To fetch contacts matching a name.
--
-- The name can contain any number of words.
--
-- ObjC selector: @+ predicateForContactsMatchingName:@
predicateForContactsMatchingName :: IsNSString name => name -> IO (Id NSPredicate)
predicateForContactsMatchingName name =
  do
    cls' <- getRequiredClass "CNContact"
    sendClassMessage cls' predicateForContactsMatchingNameSelector (toNSString name)

-- | Fetch contacts matching an email address.
--
-- Use this predicate to find the contact(s) which contain the specified              email address. The search is not case-sensitive.
--
-- @emailAddress@ — The email address to search for. Do not include a scheme (e.g., "mailto:").
--
-- ObjC selector: @+ predicateForContactsMatchingEmailAddress:@
predicateForContactsMatchingEmailAddress :: IsNSString emailAddress => emailAddress -> IO (Id NSPredicate)
predicateForContactsMatchingEmailAddress emailAddress =
  do
    cls' <- getRequiredClass "CNContact"
    sendClassMessage cls' predicateForContactsMatchingEmailAddressSelector (toNSString emailAddress)

-- | Fetch contacts matching a phone number.
--
-- If the predicate and contact differ in their use or presence of country              codes, a best effort will be made to match results; however, inexact              matches are not guaranteed.
--
-- @phoneNumber@ — A @CNPhoneNumber@ representing the phone number to search for.              Do not include a scheme (e.g., "tel:").
--
-- ObjC selector: @+ predicateForContactsMatchingPhoneNumber:@
predicateForContactsMatchingPhoneNumber :: IsCNPhoneNumber phoneNumber => phoneNumber -> IO (Id NSPredicate)
predicateForContactsMatchingPhoneNumber phoneNumber =
  do
    cls' <- getRequiredClass "CNContact"
    sendClassMessage cls' predicateForContactsMatchingPhoneNumberSelector (toCNPhoneNumber phoneNumber)

-- | To fetch contacts matching contact identifiers.
--
-- ObjC selector: @+ predicateForContactsWithIdentifiers:@
predicateForContactsWithIdentifiers :: IsNSArray identifiers => identifiers -> IO (Id NSPredicate)
predicateForContactsWithIdentifiers identifiers =
  do
    cls' <- getRequiredClass "CNContact"
    sendClassMessage cls' predicateForContactsWithIdentifiersSelector (toNSArray identifiers)

-- | @+ predicateForContactsInGroupWithIdentifier:@
predicateForContactsInGroupWithIdentifier :: IsNSString groupIdentifier => groupIdentifier -> IO (Id NSPredicate)
predicateForContactsInGroupWithIdentifier groupIdentifier =
  do
    cls' <- getRequiredClass "CNContact"
    sendClassMessage cls' predicateForContactsInGroupWithIdentifierSelector (toNSString groupIdentifier)

-- | @+ predicateForContactsInContainerWithIdentifier:@
predicateForContactsInContainerWithIdentifier :: IsNSString containerIdentifier => containerIdentifier -> IO (Id NSPredicate)
predicateForContactsInContainerWithIdentifier containerIdentifier =
  do
    cls' <- getRequiredClass "CNContact"
    sendClassMessage cls' predicateForContactsInContainerWithIdentifierSelector (toNSString containerIdentifier)

-- | The identifier is unique among contacts on the device. It can be saved and used for fetching contacts next application launch.
--
-- ObjC selector: @- identifier@
identifier :: IsCNContact cnContact => cnContact -> IO (Id NSString)
identifier cnContact =
  sendMessage cnContact identifierSelector

-- | @- contactType@
contactType :: IsCNContact cnContact => cnContact -> IO CNContactType
contactType cnContact =
  sendMessage cnContact contactTypeSelector

-- | @- namePrefix@
namePrefix :: IsCNContact cnContact => cnContact -> IO (Id NSString)
namePrefix cnContact =
  sendMessage cnContact namePrefixSelector

-- | @- givenName@
givenName :: IsCNContact cnContact => cnContact -> IO (Id NSString)
givenName cnContact =
  sendMessage cnContact givenNameSelector

-- | @- middleName@
middleName :: IsCNContact cnContact => cnContact -> IO (Id NSString)
middleName cnContact =
  sendMessage cnContact middleNameSelector

-- | @- familyName@
familyName :: IsCNContact cnContact => cnContact -> IO (Id NSString)
familyName cnContact =
  sendMessage cnContact familyNameSelector

-- | @- previousFamilyName@
previousFamilyName :: IsCNContact cnContact => cnContact -> IO (Id NSString)
previousFamilyName cnContact =
  sendMessage cnContact previousFamilyNameSelector

-- | @- nameSuffix@
nameSuffix :: IsCNContact cnContact => cnContact -> IO (Id NSString)
nameSuffix cnContact =
  sendMessage cnContact nameSuffixSelector

-- | @- nickname@
nickname :: IsCNContact cnContact => cnContact -> IO (Id NSString)
nickname cnContact =
  sendMessage cnContact nicknameSelector

-- | @- organizationName@
organizationName :: IsCNContact cnContact => cnContact -> IO (Id NSString)
organizationName cnContact =
  sendMessage cnContact organizationNameSelector

-- | @- departmentName@
departmentName :: IsCNContact cnContact => cnContact -> IO (Id NSString)
departmentName cnContact =
  sendMessage cnContact departmentNameSelector

-- | @- jobTitle@
jobTitle :: IsCNContact cnContact => cnContact -> IO (Id NSString)
jobTitle cnContact =
  sendMessage cnContact jobTitleSelector

-- | @- phoneticGivenName@
phoneticGivenName :: IsCNContact cnContact => cnContact -> IO (Id NSString)
phoneticGivenName cnContact =
  sendMessage cnContact phoneticGivenNameSelector

-- | @- phoneticMiddleName@
phoneticMiddleName :: IsCNContact cnContact => cnContact -> IO (Id NSString)
phoneticMiddleName cnContact =
  sendMessage cnContact phoneticMiddleNameSelector

-- | @- phoneticFamilyName@
phoneticFamilyName :: IsCNContact cnContact => cnContact -> IO (Id NSString)
phoneticFamilyName cnContact =
  sendMessage cnContact phoneticFamilyNameSelector

-- | @- phoneticOrganizationName@
phoneticOrganizationName :: IsCNContact cnContact => cnContact -> IO RawId
phoneticOrganizationName cnContact =
  sendMessage cnContact phoneticOrganizationNameSelector

-- | @- note@
note :: IsCNContact cnContact => cnContact -> IO (Id NSString)
note cnContact =
  sendMessage cnContact noteSelector

-- | @- imageData@
imageData :: IsCNContact cnContact => cnContact -> IO (Id NSData)
imageData cnContact =
  sendMessage cnContact imageDataSelector

-- | @- thumbnailImageData@
thumbnailImageData :: IsCNContact cnContact => cnContact -> IO (Id NSData)
thumbnailImageData cnContact =
  sendMessage cnContact thumbnailImageDataSelector

-- | @- imageDataAvailable@
imageDataAvailable :: IsCNContact cnContact => cnContact -> IO Bool
imageDataAvailable cnContact =
  sendMessage cnContact imageDataAvailableSelector

-- | @- phoneNumbers@
phoneNumbers :: IsCNContact cnContact => cnContact -> IO (Id NSArray)
phoneNumbers cnContact =
  sendMessage cnContact phoneNumbersSelector

-- | @- emailAddresses@
emailAddresses :: IsCNContact cnContact => cnContact -> IO (Id NSArray)
emailAddresses cnContact =
  sendMessage cnContact emailAddressesSelector

-- | @- postalAddresses@
postalAddresses :: IsCNContact cnContact => cnContact -> IO (Id NSArray)
postalAddresses cnContact =
  sendMessage cnContact postalAddressesSelector

-- | @- urlAddresses@
urlAddresses :: IsCNContact cnContact => cnContact -> IO (Id NSArray)
urlAddresses cnContact =
  sendMessage cnContact urlAddressesSelector

-- | @- contactRelations@
contactRelations :: IsCNContact cnContact => cnContact -> IO (Id NSArray)
contactRelations cnContact =
  sendMessage cnContact contactRelationsSelector

-- | @- socialProfiles@
socialProfiles :: IsCNContact cnContact => cnContact -> IO (Id NSArray)
socialProfiles cnContact =
  sendMessage cnContact socialProfilesSelector

-- | @- instantMessageAddresses@
instantMessageAddresses :: IsCNContact cnContact => cnContact -> IO (Id NSArray)
instantMessageAddresses cnContact =
  sendMessage cnContact instantMessageAddressesSelector

-- | The Gregorian birthday.
--
-- ObjC selector: @- birthday@
birthday :: IsCNContact cnContact => cnContact -> IO (Id NSDateComponents)
birthday cnContact =
  sendMessage cnContact birthdaySelector

-- | The alternate birthday (Lunisolar).
--
-- ObjC selector: @- nonGregorianBirthday@
nonGregorianBirthday :: IsCNContact cnContact => cnContact -> IO (Id NSDateComponents)
nonGregorianBirthday cnContact =
  sendMessage cnContact nonGregorianBirthdaySelector

-- | Other Gregorian dates (anniversaries, etc).
--
-- ObjC selector: @- dates@
dates :: IsCNContact cnContact => cnContact -> IO (Id NSArray)
dates cnContact =
  sendMessage cnContact datesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isKeyAvailable:@
isKeyAvailableSelector :: Selector '[Id NSString] Bool
isKeyAvailableSelector = mkSelector "isKeyAvailable:"

-- | @Selector@ for @areKeysAvailable:@
areKeysAvailableSelector :: Selector '[Id NSArray] Bool
areKeysAvailableSelector = mkSelector "areKeysAvailable:"

-- | @Selector@ for @localizedStringForKey:@
localizedStringForKeySelector :: Selector '[Id NSString] (Id NSString)
localizedStringForKeySelector = mkSelector "localizedStringForKey:"

-- | @Selector@ for @comparatorForNameSortOrder:@
comparatorForNameSortOrderSelector :: Selector '[CNContactSortOrder] (Ptr ())
comparatorForNameSortOrderSelector = mkSelector "comparatorForNameSortOrder:"

-- | @Selector@ for @descriptorForAllComparatorKeys@
descriptorForAllComparatorKeysSelector :: Selector '[] RawId
descriptorForAllComparatorKeysSelector = mkSelector "descriptorForAllComparatorKeys"

-- | @Selector@ for @isUnifiedWithContactWithIdentifier:@
isUnifiedWithContactWithIdentifierSelector :: Selector '[Id NSString] Bool
isUnifiedWithContactWithIdentifierSelector = mkSelector "isUnifiedWithContactWithIdentifier:"

-- | @Selector@ for @predicateForContactsMatchingName:@
predicateForContactsMatchingNameSelector :: Selector '[Id NSString] (Id NSPredicate)
predicateForContactsMatchingNameSelector = mkSelector "predicateForContactsMatchingName:"

-- | @Selector@ for @predicateForContactsMatchingEmailAddress:@
predicateForContactsMatchingEmailAddressSelector :: Selector '[Id NSString] (Id NSPredicate)
predicateForContactsMatchingEmailAddressSelector = mkSelector "predicateForContactsMatchingEmailAddress:"

-- | @Selector@ for @predicateForContactsMatchingPhoneNumber:@
predicateForContactsMatchingPhoneNumberSelector :: Selector '[Id CNPhoneNumber] (Id NSPredicate)
predicateForContactsMatchingPhoneNumberSelector = mkSelector "predicateForContactsMatchingPhoneNumber:"

-- | @Selector@ for @predicateForContactsWithIdentifiers:@
predicateForContactsWithIdentifiersSelector :: Selector '[Id NSArray] (Id NSPredicate)
predicateForContactsWithIdentifiersSelector = mkSelector "predicateForContactsWithIdentifiers:"

-- | @Selector@ for @predicateForContactsInGroupWithIdentifier:@
predicateForContactsInGroupWithIdentifierSelector :: Selector '[Id NSString] (Id NSPredicate)
predicateForContactsInGroupWithIdentifierSelector = mkSelector "predicateForContactsInGroupWithIdentifier:"

-- | @Selector@ for @predicateForContactsInContainerWithIdentifier:@
predicateForContactsInContainerWithIdentifierSelector :: Selector '[Id NSString] (Id NSPredicate)
predicateForContactsInContainerWithIdentifierSelector = mkSelector "predicateForContactsInContainerWithIdentifier:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @contactType@
contactTypeSelector :: Selector '[] CNContactType
contactTypeSelector = mkSelector "contactType"

-- | @Selector@ for @namePrefix@
namePrefixSelector :: Selector '[] (Id NSString)
namePrefixSelector = mkSelector "namePrefix"

-- | @Selector@ for @givenName@
givenNameSelector :: Selector '[] (Id NSString)
givenNameSelector = mkSelector "givenName"

-- | @Selector@ for @middleName@
middleNameSelector :: Selector '[] (Id NSString)
middleNameSelector = mkSelector "middleName"

-- | @Selector@ for @familyName@
familyNameSelector :: Selector '[] (Id NSString)
familyNameSelector = mkSelector "familyName"

-- | @Selector@ for @previousFamilyName@
previousFamilyNameSelector :: Selector '[] (Id NSString)
previousFamilyNameSelector = mkSelector "previousFamilyName"

-- | @Selector@ for @nameSuffix@
nameSuffixSelector :: Selector '[] (Id NSString)
nameSuffixSelector = mkSelector "nameSuffix"

-- | @Selector@ for @nickname@
nicknameSelector :: Selector '[] (Id NSString)
nicknameSelector = mkSelector "nickname"

-- | @Selector@ for @organizationName@
organizationNameSelector :: Selector '[] (Id NSString)
organizationNameSelector = mkSelector "organizationName"

-- | @Selector@ for @departmentName@
departmentNameSelector :: Selector '[] (Id NSString)
departmentNameSelector = mkSelector "departmentName"

-- | @Selector@ for @jobTitle@
jobTitleSelector :: Selector '[] (Id NSString)
jobTitleSelector = mkSelector "jobTitle"

-- | @Selector@ for @phoneticGivenName@
phoneticGivenNameSelector :: Selector '[] (Id NSString)
phoneticGivenNameSelector = mkSelector "phoneticGivenName"

-- | @Selector@ for @phoneticMiddleName@
phoneticMiddleNameSelector :: Selector '[] (Id NSString)
phoneticMiddleNameSelector = mkSelector "phoneticMiddleName"

-- | @Selector@ for @phoneticFamilyName@
phoneticFamilyNameSelector :: Selector '[] (Id NSString)
phoneticFamilyNameSelector = mkSelector "phoneticFamilyName"

-- | @Selector@ for @phoneticOrganizationName@
phoneticOrganizationNameSelector :: Selector '[] RawId
phoneticOrganizationNameSelector = mkSelector "phoneticOrganizationName"

-- | @Selector@ for @note@
noteSelector :: Selector '[] (Id NSString)
noteSelector = mkSelector "note"

-- | @Selector@ for @imageData@
imageDataSelector :: Selector '[] (Id NSData)
imageDataSelector = mkSelector "imageData"

-- | @Selector@ for @thumbnailImageData@
thumbnailImageDataSelector :: Selector '[] (Id NSData)
thumbnailImageDataSelector = mkSelector "thumbnailImageData"

-- | @Selector@ for @imageDataAvailable@
imageDataAvailableSelector :: Selector '[] Bool
imageDataAvailableSelector = mkSelector "imageDataAvailable"

-- | @Selector@ for @phoneNumbers@
phoneNumbersSelector :: Selector '[] (Id NSArray)
phoneNumbersSelector = mkSelector "phoneNumbers"

-- | @Selector@ for @emailAddresses@
emailAddressesSelector :: Selector '[] (Id NSArray)
emailAddressesSelector = mkSelector "emailAddresses"

-- | @Selector@ for @postalAddresses@
postalAddressesSelector :: Selector '[] (Id NSArray)
postalAddressesSelector = mkSelector "postalAddresses"

-- | @Selector@ for @urlAddresses@
urlAddressesSelector :: Selector '[] (Id NSArray)
urlAddressesSelector = mkSelector "urlAddresses"

-- | @Selector@ for @contactRelations@
contactRelationsSelector :: Selector '[] (Id NSArray)
contactRelationsSelector = mkSelector "contactRelations"

-- | @Selector@ for @socialProfiles@
socialProfilesSelector :: Selector '[] (Id NSArray)
socialProfilesSelector = mkSelector "socialProfiles"

-- | @Selector@ for @instantMessageAddresses@
instantMessageAddressesSelector :: Selector '[] (Id NSArray)
instantMessageAddressesSelector = mkSelector "instantMessageAddresses"

-- | @Selector@ for @birthday@
birthdaySelector :: Selector '[] (Id NSDateComponents)
birthdaySelector = mkSelector "birthday"

-- | @Selector@ for @nonGregorianBirthday@
nonGregorianBirthdaySelector :: Selector '[] (Id NSDateComponents)
nonGregorianBirthdaySelector = mkSelector "nonGregorianBirthday"

-- | @Selector@ for @dates@
datesSelector :: Selector '[] (Id NSArray)
datesSelector = mkSelector "dates"

