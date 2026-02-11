{-# LANGUAGE PatternSynonyms #-}
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
  , isKeyAvailableSelector
  , areKeysAvailableSelector
  , localizedStringForKeySelector
  , comparatorForNameSortOrderSelector
  , descriptorForAllComparatorKeysSelector
  , isUnifiedWithContactWithIdentifierSelector
  , predicateForContactsMatchingNameSelector
  , predicateForContactsMatchingEmailAddressSelector
  , predicateForContactsMatchingPhoneNumberSelector
  , predicateForContactsWithIdentifiersSelector
  , predicateForContactsInGroupWithIdentifierSelector
  , predicateForContactsInContainerWithIdentifierSelector
  , identifierSelector
  , contactTypeSelector
  , namePrefixSelector
  , givenNameSelector
  , middleNameSelector
  , familyNameSelector
  , previousFamilyNameSelector
  , nameSuffixSelector
  , nicknameSelector
  , organizationNameSelector
  , departmentNameSelector
  , jobTitleSelector
  , phoneticGivenNameSelector
  , phoneticMiddleNameSelector
  , phoneticFamilyNameSelector
  , noteSelector
  , imageDataSelector
  , thumbnailImageDataSelector
  , imageDataAvailableSelector
  , phoneNumbersSelector
  , emailAddressesSelector
  , postalAddressesSelector
  , urlAddressesSelector
  , contactRelationsSelector
  , socialProfilesSelector
  , instantMessageAddressesSelector
  , birthdaySelector
  , nonGregorianBirthdaySelector
  , datesSelector

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

-- | Returns YES if the value for the specified key was fetched.
--
-- ObjC selector: @- isKeyAvailable:@
isKeyAvailable :: (IsCNContact cnContact, IsNSString key) => cnContact -> key -> IO Bool
isKeyAvailable cnContact  key =
withObjCPtr key $ \raw_key ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cnContact (mkSelector "isKeyAvailable:") retCULong [argPtr (castPtr raw_key :: Ptr ())]

-- | Returns YES if the values for the keys specified by all the descriptors were fetched.
--
-- ObjC selector: @- areKeysAvailable:@
areKeysAvailable :: (IsCNContact cnContact, IsNSArray keyDescriptors) => cnContact -> keyDescriptors -> IO Bool
areKeysAvailable cnContact  keyDescriptors =
withObjCPtr keyDescriptors $ \raw_keyDescriptors ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cnContact (mkSelector "areKeysAvailable:") retCULong [argPtr (castPtr raw_keyDescriptors :: Ptr ())]

-- | Returns a user displayable property name.
--
-- ObjC selector: @+ localizedStringForKey:@
localizedStringForKey :: IsNSString key => key -> IO (Id NSString)
localizedStringForKey key =
  do
    cls' <- getRequiredClass "CNContact"
    withObjCPtr key $ \raw_key ->
      sendClassMsg cls' (mkSelector "localizedStringForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | The contact comparator for a given sort order.
--
-- ObjC selector: @+ comparatorForNameSortOrder:@
comparatorForNameSortOrder :: CNContactSortOrder -> IO (Ptr ())
comparatorForNameSortOrder sortOrder =
  do
    cls' <- getRequiredClass "CNContact"
    fmap castPtr $ sendClassMsg cls' (mkSelector "comparatorForNameSortOrder:") (retPtr retVoid) [argCLong (coerce sortOrder)]

-- | Use to fetch all contact keys required for the contact sort comparator.
--
-- ObjC selector: @+ descriptorForAllComparatorKeys@
descriptorForAllComparatorKeys :: IO RawId
descriptorForAllComparatorKeys  =
  do
    cls' <- getRequiredClass "CNContact"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "descriptorForAllComparatorKeys") (retPtr retVoid) []

-- | Returns YES if the receiver was fetched as a unified contact and includes the contact having contactIdentifier in its unification
--
-- ObjC selector: @- isUnifiedWithContactWithIdentifier:@
isUnifiedWithContactWithIdentifier :: (IsCNContact cnContact, IsNSString contactIdentifier) => cnContact -> contactIdentifier -> IO Bool
isUnifiedWithContactWithIdentifier cnContact  contactIdentifier =
withObjCPtr contactIdentifier $ \raw_contactIdentifier ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cnContact (mkSelector "isUnifiedWithContactWithIdentifier:") retCULong [argPtr (castPtr raw_contactIdentifier :: Ptr ())]

-- | To fetch contacts matching a name.
--
-- The name can contain any number of words.
--
-- ObjC selector: @+ predicateForContactsMatchingName:@
predicateForContactsMatchingName :: IsNSString name => name -> IO (Id NSPredicate)
predicateForContactsMatchingName name =
  do
    cls' <- getRequiredClass "CNContact"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "predicateForContactsMatchingName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr emailAddress $ \raw_emailAddress ->
      sendClassMsg cls' (mkSelector "predicateForContactsMatchingEmailAddress:") (retPtr retVoid) [argPtr (castPtr raw_emailAddress :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr phoneNumber $ \raw_phoneNumber ->
      sendClassMsg cls' (mkSelector "predicateForContactsMatchingPhoneNumber:") (retPtr retVoid) [argPtr (castPtr raw_phoneNumber :: Ptr ())] >>= retainedObject . castPtr

-- | To fetch contacts matching contact identifiers.
--
-- ObjC selector: @+ predicateForContactsWithIdentifiers:@
predicateForContactsWithIdentifiers :: IsNSArray identifiers => identifiers -> IO (Id NSPredicate)
predicateForContactsWithIdentifiers identifiers =
  do
    cls' <- getRequiredClass "CNContact"
    withObjCPtr identifiers $ \raw_identifiers ->
      sendClassMsg cls' (mkSelector "predicateForContactsWithIdentifiers:") (retPtr retVoid) [argPtr (castPtr raw_identifiers :: Ptr ())] >>= retainedObject . castPtr

-- | @+ predicateForContactsInGroupWithIdentifier:@
predicateForContactsInGroupWithIdentifier :: IsNSString groupIdentifier => groupIdentifier -> IO (Id NSPredicate)
predicateForContactsInGroupWithIdentifier groupIdentifier =
  do
    cls' <- getRequiredClass "CNContact"
    withObjCPtr groupIdentifier $ \raw_groupIdentifier ->
      sendClassMsg cls' (mkSelector "predicateForContactsInGroupWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_groupIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | @+ predicateForContactsInContainerWithIdentifier:@
predicateForContactsInContainerWithIdentifier :: IsNSString containerIdentifier => containerIdentifier -> IO (Id NSPredicate)
predicateForContactsInContainerWithIdentifier containerIdentifier =
  do
    cls' <- getRequiredClass "CNContact"
    withObjCPtr containerIdentifier $ \raw_containerIdentifier ->
      sendClassMsg cls' (mkSelector "predicateForContactsInContainerWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_containerIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | The identifier is unique among contacts on the device. It can be saved and used for fetching contacts next application launch.
--
-- ObjC selector: @- identifier@
identifier :: IsCNContact cnContact => cnContact -> IO (Id NSString)
identifier cnContact  =
  sendMsg cnContact (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- contactType@
contactType :: IsCNContact cnContact => cnContact -> IO CNContactType
contactType cnContact  =
  fmap (coerce :: CLong -> CNContactType) $ sendMsg cnContact (mkSelector "contactType") retCLong []

-- | @- namePrefix@
namePrefix :: IsCNContact cnContact => cnContact -> IO (Id NSString)
namePrefix cnContact  =
  sendMsg cnContact (mkSelector "namePrefix") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- givenName@
givenName :: IsCNContact cnContact => cnContact -> IO (Id NSString)
givenName cnContact  =
  sendMsg cnContact (mkSelector "givenName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- middleName@
middleName :: IsCNContact cnContact => cnContact -> IO (Id NSString)
middleName cnContact  =
  sendMsg cnContact (mkSelector "middleName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- familyName@
familyName :: IsCNContact cnContact => cnContact -> IO (Id NSString)
familyName cnContact  =
  sendMsg cnContact (mkSelector "familyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- previousFamilyName@
previousFamilyName :: IsCNContact cnContact => cnContact -> IO (Id NSString)
previousFamilyName cnContact  =
  sendMsg cnContact (mkSelector "previousFamilyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- nameSuffix@
nameSuffix :: IsCNContact cnContact => cnContact -> IO (Id NSString)
nameSuffix cnContact  =
  sendMsg cnContact (mkSelector "nameSuffix") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- nickname@
nickname :: IsCNContact cnContact => cnContact -> IO (Id NSString)
nickname cnContact  =
  sendMsg cnContact (mkSelector "nickname") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- organizationName@
organizationName :: IsCNContact cnContact => cnContact -> IO (Id NSString)
organizationName cnContact  =
  sendMsg cnContact (mkSelector "organizationName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- departmentName@
departmentName :: IsCNContact cnContact => cnContact -> IO (Id NSString)
departmentName cnContact  =
  sendMsg cnContact (mkSelector "departmentName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- jobTitle@
jobTitle :: IsCNContact cnContact => cnContact -> IO (Id NSString)
jobTitle cnContact  =
  sendMsg cnContact (mkSelector "jobTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- phoneticGivenName@
phoneticGivenName :: IsCNContact cnContact => cnContact -> IO (Id NSString)
phoneticGivenName cnContact  =
  sendMsg cnContact (mkSelector "phoneticGivenName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- phoneticMiddleName@
phoneticMiddleName :: IsCNContact cnContact => cnContact -> IO (Id NSString)
phoneticMiddleName cnContact  =
  sendMsg cnContact (mkSelector "phoneticMiddleName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- phoneticFamilyName@
phoneticFamilyName :: IsCNContact cnContact => cnContact -> IO (Id NSString)
phoneticFamilyName cnContact  =
  sendMsg cnContact (mkSelector "phoneticFamilyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- note@
note :: IsCNContact cnContact => cnContact -> IO (Id NSString)
note cnContact  =
  sendMsg cnContact (mkSelector "note") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- imageData@
imageData :: IsCNContact cnContact => cnContact -> IO (Id NSData)
imageData cnContact  =
  sendMsg cnContact (mkSelector "imageData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- thumbnailImageData@
thumbnailImageData :: IsCNContact cnContact => cnContact -> IO (Id NSData)
thumbnailImageData cnContact  =
  sendMsg cnContact (mkSelector "thumbnailImageData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- imageDataAvailable@
imageDataAvailable :: IsCNContact cnContact => cnContact -> IO Bool
imageDataAvailable cnContact  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cnContact (mkSelector "imageDataAvailable") retCULong []

-- | @- phoneNumbers@
phoneNumbers :: IsCNContact cnContact => cnContact -> IO (Id NSArray)
phoneNumbers cnContact  =
  sendMsg cnContact (mkSelector "phoneNumbers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- emailAddresses@
emailAddresses :: IsCNContact cnContact => cnContact -> IO (Id NSArray)
emailAddresses cnContact  =
  sendMsg cnContact (mkSelector "emailAddresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- postalAddresses@
postalAddresses :: IsCNContact cnContact => cnContact -> IO (Id NSArray)
postalAddresses cnContact  =
  sendMsg cnContact (mkSelector "postalAddresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- urlAddresses@
urlAddresses :: IsCNContact cnContact => cnContact -> IO (Id NSArray)
urlAddresses cnContact  =
  sendMsg cnContact (mkSelector "urlAddresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- contactRelations@
contactRelations :: IsCNContact cnContact => cnContact -> IO (Id NSArray)
contactRelations cnContact  =
  sendMsg cnContact (mkSelector "contactRelations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- socialProfiles@
socialProfiles :: IsCNContact cnContact => cnContact -> IO (Id NSArray)
socialProfiles cnContact  =
  sendMsg cnContact (mkSelector "socialProfiles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- instantMessageAddresses@
instantMessageAddresses :: IsCNContact cnContact => cnContact -> IO (Id NSArray)
instantMessageAddresses cnContact  =
  sendMsg cnContact (mkSelector "instantMessageAddresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Gregorian birthday.
--
-- ObjC selector: @- birthday@
birthday :: IsCNContact cnContact => cnContact -> IO (Id NSDateComponents)
birthday cnContact  =
  sendMsg cnContact (mkSelector "birthday") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The alternate birthday (Lunisolar).
--
-- ObjC selector: @- nonGregorianBirthday@
nonGregorianBirthday :: IsCNContact cnContact => cnContact -> IO (Id NSDateComponents)
nonGregorianBirthday cnContact  =
  sendMsg cnContact (mkSelector "nonGregorianBirthday") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Other Gregorian dates (anniversaries, etc).
--
-- ObjC selector: @- dates@
dates :: IsCNContact cnContact => cnContact -> IO (Id NSArray)
dates cnContact  =
  sendMsg cnContact (mkSelector "dates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isKeyAvailable:@
isKeyAvailableSelector :: Selector
isKeyAvailableSelector = mkSelector "isKeyAvailable:"

-- | @Selector@ for @areKeysAvailable:@
areKeysAvailableSelector :: Selector
areKeysAvailableSelector = mkSelector "areKeysAvailable:"

-- | @Selector@ for @localizedStringForKey:@
localizedStringForKeySelector :: Selector
localizedStringForKeySelector = mkSelector "localizedStringForKey:"

-- | @Selector@ for @comparatorForNameSortOrder:@
comparatorForNameSortOrderSelector :: Selector
comparatorForNameSortOrderSelector = mkSelector "comparatorForNameSortOrder:"

-- | @Selector@ for @descriptorForAllComparatorKeys@
descriptorForAllComparatorKeysSelector :: Selector
descriptorForAllComparatorKeysSelector = mkSelector "descriptorForAllComparatorKeys"

-- | @Selector@ for @isUnifiedWithContactWithIdentifier:@
isUnifiedWithContactWithIdentifierSelector :: Selector
isUnifiedWithContactWithIdentifierSelector = mkSelector "isUnifiedWithContactWithIdentifier:"

-- | @Selector@ for @predicateForContactsMatchingName:@
predicateForContactsMatchingNameSelector :: Selector
predicateForContactsMatchingNameSelector = mkSelector "predicateForContactsMatchingName:"

-- | @Selector@ for @predicateForContactsMatchingEmailAddress:@
predicateForContactsMatchingEmailAddressSelector :: Selector
predicateForContactsMatchingEmailAddressSelector = mkSelector "predicateForContactsMatchingEmailAddress:"

-- | @Selector@ for @predicateForContactsMatchingPhoneNumber:@
predicateForContactsMatchingPhoneNumberSelector :: Selector
predicateForContactsMatchingPhoneNumberSelector = mkSelector "predicateForContactsMatchingPhoneNumber:"

-- | @Selector@ for @predicateForContactsWithIdentifiers:@
predicateForContactsWithIdentifiersSelector :: Selector
predicateForContactsWithIdentifiersSelector = mkSelector "predicateForContactsWithIdentifiers:"

-- | @Selector@ for @predicateForContactsInGroupWithIdentifier:@
predicateForContactsInGroupWithIdentifierSelector :: Selector
predicateForContactsInGroupWithIdentifierSelector = mkSelector "predicateForContactsInGroupWithIdentifier:"

-- | @Selector@ for @predicateForContactsInContainerWithIdentifier:@
predicateForContactsInContainerWithIdentifierSelector :: Selector
predicateForContactsInContainerWithIdentifierSelector = mkSelector "predicateForContactsInContainerWithIdentifier:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @contactType@
contactTypeSelector :: Selector
contactTypeSelector = mkSelector "contactType"

-- | @Selector@ for @namePrefix@
namePrefixSelector :: Selector
namePrefixSelector = mkSelector "namePrefix"

-- | @Selector@ for @givenName@
givenNameSelector :: Selector
givenNameSelector = mkSelector "givenName"

-- | @Selector@ for @middleName@
middleNameSelector :: Selector
middleNameSelector = mkSelector "middleName"

-- | @Selector@ for @familyName@
familyNameSelector :: Selector
familyNameSelector = mkSelector "familyName"

-- | @Selector@ for @previousFamilyName@
previousFamilyNameSelector :: Selector
previousFamilyNameSelector = mkSelector "previousFamilyName"

-- | @Selector@ for @nameSuffix@
nameSuffixSelector :: Selector
nameSuffixSelector = mkSelector "nameSuffix"

-- | @Selector@ for @nickname@
nicknameSelector :: Selector
nicknameSelector = mkSelector "nickname"

-- | @Selector@ for @organizationName@
organizationNameSelector :: Selector
organizationNameSelector = mkSelector "organizationName"

-- | @Selector@ for @departmentName@
departmentNameSelector :: Selector
departmentNameSelector = mkSelector "departmentName"

-- | @Selector@ for @jobTitle@
jobTitleSelector :: Selector
jobTitleSelector = mkSelector "jobTitle"

-- | @Selector@ for @phoneticGivenName@
phoneticGivenNameSelector :: Selector
phoneticGivenNameSelector = mkSelector "phoneticGivenName"

-- | @Selector@ for @phoneticMiddleName@
phoneticMiddleNameSelector :: Selector
phoneticMiddleNameSelector = mkSelector "phoneticMiddleName"

-- | @Selector@ for @phoneticFamilyName@
phoneticFamilyNameSelector :: Selector
phoneticFamilyNameSelector = mkSelector "phoneticFamilyName"

-- | @Selector@ for @note@
noteSelector :: Selector
noteSelector = mkSelector "note"

-- | @Selector@ for @imageData@
imageDataSelector :: Selector
imageDataSelector = mkSelector "imageData"

-- | @Selector@ for @thumbnailImageData@
thumbnailImageDataSelector :: Selector
thumbnailImageDataSelector = mkSelector "thumbnailImageData"

-- | @Selector@ for @imageDataAvailable@
imageDataAvailableSelector :: Selector
imageDataAvailableSelector = mkSelector "imageDataAvailable"

-- | @Selector@ for @phoneNumbers@
phoneNumbersSelector :: Selector
phoneNumbersSelector = mkSelector "phoneNumbers"

-- | @Selector@ for @emailAddresses@
emailAddressesSelector :: Selector
emailAddressesSelector = mkSelector "emailAddresses"

-- | @Selector@ for @postalAddresses@
postalAddressesSelector :: Selector
postalAddressesSelector = mkSelector "postalAddresses"

-- | @Selector@ for @urlAddresses@
urlAddressesSelector :: Selector
urlAddressesSelector = mkSelector "urlAddresses"

-- | @Selector@ for @contactRelations@
contactRelationsSelector :: Selector
contactRelationsSelector = mkSelector "contactRelations"

-- | @Selector@ for @socialProfiles@
socialProfilesSelector :: Selector
socialProfilesSelector = mkSelector "socialProfiles"

-- | @Selector@ for @instantMessageAddresses@
instantMessageAddressesSelector :: Selector
instantMessageAddressesSelector = mkSelector "instantMessageAddresses"

-- | @Selector@ for @birthday@
birthdaySelector :: Selector
birthdaySelector = mkSelector "birthday"

-- | @Selector@ for @nonGregorianBirthday@
nonGregorianBirthdaySelector :: Selector
nonGregorianBirthdaySelector = mkSelector "nonGregorianBirthday"

-- | @Selector@ for @dates@
datesSelector :: Selector
datesSelector = mkSelector "dates"

