{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Elements that can be requested from identity documents. Not all elements are supported by all document types. If an element is requested from a type that does not support it, the element is ignored.
--
-- Generated bindings for @PKIdentityElement@.
module ObjC.PassKit.PKIdentityElement
  ( PKIdentityElement
  , IsPKIdentityElement(..)
  , ageThresholdElementWithAge
  , init_
  , new
  , givenNameElement
  , familyNameElement
  , portraitElement
  , addressElement
  , heightElement
  , weightElement
  , eyeColorElement
  , hairColorElement
  , organDonorStatusElement
  , veteranStatusElement
  , issuingAuthorityElement
  , documentIssueDateElement
  , documentExpirationDateElement
  , documentDHSComplianceStatusElement
  , documentNumberElement
  , drivingPrivilegesElement
  , ageElement
  , dateOfBirthElement
  , sexElement
  , addressElementSelector
  , ageElementSelector
  , ageThresholdElementWithAgeSelector
  , dateOfBirthElementSelector
  , documentDHSComplianceStatusElementSelector
  , documentExpirationDateElementSelector
  , documentIssueDateElementSelector
  , documentNumberElementSelector
  , drivingPrivilegesElementSelector
  , eyeColorElementSelector
  , familyNameElementSelector
  , givenNameElementSelector
  , hairColorElementSelector
  , heightElementSelector
  , initSelector
  , issuingAuthorityElementSelector
  , newSelector
  , organDonorStatusElementSelector
  , portraitElementSelector
  , sexElementSelector
  , veteranStatusElementSelector
  , weightElementSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Boolean indicating whether the user's age is at least the given age. For example, ageThresholdElementWithAge:21 will return true if the user is at least 21 years old. This value is only available for a given age if it was provided by the issuer. If this value is not available, it will automatically fall back to a request for age.
--
-- ObjC selector: @+ ageThresholdElementWithAge:@
ageThresholdElementWithAge :: CLong -> IO (Id PKIdentityElement)
ageThresholdElementWithAge age =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMessage cls' ageThresholdElementWithAgeSelector age

-- | @- init@
init_ :: IsPKIdentityElement pkIdentityElement => pkIdentityElement -> IO (Id PKIdentityElement)
init_ pkIdentityElement =
  sendOwnedMessage pkIdentityElement initSelector

-- | @+ new@
new :: IO (Id PKIdentityElement)
new  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendOwnedClassMessage cls' newSelector

-- | The user's given name or first name.
--
-- ObjC selector: @+ givenNameElement@
givenNameElement :: IO (Id PKIdentityElement)
givenNameElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMessage cls' givenNameElementSelector

-- | The user's family name or last name.
--
-- ObjC selector: @+ familyNameElement@
familyNameElement :: IO (Id PKIdentityElement)
familyNameElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMessage cls' familyNameElementSelector

-- | The portrait of the user on record with the issuer.
--
-- ObjC selector: @+ portraitElement@
portraitElement :: IO (Id PKIdentityElement)
portraitElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMessage cls' portraitElementSelector

-- | The address on record with the issuer.
--
-- ObjC selector: @+ addressElement@
addressElement :: IO (Id PKIdentityElement)
addressElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMessage cls' addressElementSelector

-- | The user's height on record with the issuer.
--
-- ObjC selector: @+ heightElement@
heightElement :: IO (Id PKIdentityElement)
heightElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMessage cls' heightElementSelector

-- | The user's weight on record with the issuer.
--
-- ObjC selector: @+ weightElement@
weightElement :: IO (Id PKIdentityElement)
weightElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMessage cls' weightElementSelector

-- | The user's eye color on record with the issuer.
--
-- ObjC selector: @+ eyeColorElement@
eyeColorElement :: IO (Id PKIdentityElement)
eyeColorElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMessage cls' eyeColorElementSelector

-- | The user's hair color on record with the issuer.
--
-- ObjC selector: @+ hairColorElement@
hairColorElement :: IO (Id PKIdentityElement)
hairColorElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMessage cls' hairColorElementSelector

-- | The user's organ donor status on record with the issuer.
--
-- ObjC selector: @+ organDonorStatusElement@
organDonorStatusElement :: IO (Id PKIdentityElement)
organDonorStatusElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMessage cls' organDonorStatusElementSelector

-- | The user's veteran status on record with the issuer.
--
-- ObjC selector: @+ veteranStatusElement@
veteranStatusElement :: IO (Id PKIdentityElement)
veteranStatusElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMessage cls' veteranStatusElementSelector

-- | The state or government that issued the identity document.
--
-- ObjC selector: @+ issuingAuthorityElement@
issuingAuthorityElement :: IO (Id PKIdentityElement)
issuingAuthorityElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMessage cls' issuingAuthorityElementSelector

-- | The document's issue date. This is usually the issue date of the corresponding physical document, if applicable.
--
-- ObjC selector: @+ documentIssueDateElement@
documentIssueDateElement :: IO (Id PKIdentityElement)
documentIssueDateElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMessage cls' documentIssueDateElementSelector

-- | The document's expiration date. This is usually the expiration date of the corresponding physical document, if applicable.
--
-- ObjC selector: @+ documentExpirationDateElement@
documentExpirationDateElement :: IO (Id PKIdentityElement)
documentExpirationDateElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMessage cls' documentExpirationDateElementSelector

-- | The document's DHS (U.S. Department of Homeland Security) compliance status.
--
-- This is also known as the document's "REAL ID status".
--
-- ObjC selector: @+ documentDHSComplianceStatusElement@
documentDHSComplianceStatusElement :: IO (Id PKIdentityElement)
documentDHSComplianceStatusElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMessage cls' documentDHSComplianceStatusElementSelector

-- | The document's number, as defined by the document's issuing authority.
--
-- ObjC selector: @+ documentNumberElement@
documentNumberElement :: IO (Id PKIdentityElement)
documentNumberElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMessage cls' documentNumberElementSelector

-- | The user's driving privileges.
--
-- ObjC selector: @+ drivingPrivilegesElement@
drivingPrivilegesElement :: IO (Id PKIdentityElement)
drivingPrivilegesElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMessage cls' drivingPrivilegesElementSelector

-- | The user's age in years.
--
-- ObjC selector: @+ ageElement@
ageElement :: IO (Id PKIdentityElement)
ageElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMessage cls' ageElementSelector

-- | The user's date of birth.
--
-- ObjC selector: @+ dateOfBirthElement@
dateOfBirthElement :: IO (Id PKIdentityElement)
dateOfBirthElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMessage cls' dateOfBirthElementSelector

-- | The user's sex.
--
-- ObjC selector: @+ sexElement@
sexElement :: IO (Id PKIdentityElement)
sexElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMessage cls' sexElementSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ageThresholdElementWithAge:@
ageThresholdElementWithAgeSelector :: Selector '[CLong] (Id PKIdentityElement)
ageThresholdElementWithAgeSelector = mkSelector "ageThresholdElementWithAge:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKIdentityElement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PKIdentityElement)
newSelector = mkSelector "new"

-- | @Selector@ for @givenNameElement@
givenNameElementSelector :: Selector '[] (Id PKIdentityElement)
givenNameElementSelector = mkSelector "givenNameElement"

-- | @Selector@ for @familyNameElement@
familyNameElementSelector :: Selector '[] (Id PKIdentityElement)
familyNameElementSelector = mkSelector "familyNameElement"

-- | @Selector@ for @portraitElement@
portraitElementSelector :: Selector '[] (Id PKIdentityElement)
portraitElementSelector = mkSelector "portraitElement"

-- | @Selector@ for @addressElement@
addressElementSelector :: Selector '[] (Id PKIdentityElement)
addressElementSelector = mkSelector "addressElement"

-- | @Selector@ for @heightElement@
heightElementSelector :: Selector '[] (Id PKIdentityElement)
heightElementSelector = mkSelector "heightElement"

-- | @Selector@ for @weightElement@
weightElementSelector :: Selector '[] (Id PKIdentityElement)
weightElementSelector = mkSelector "weightElement"

-- | @Selector@ for @eyeColorElement@
eyeColorElementSelector :: Selector '[] (Id PKIdentityElement)
eyeColorElementSelector = mkSelector "eyeColorElement"

-- | @Selector@ for @hairColorElement@
hairColorElementSelector :: Selector '[] (Id PKIdentityElement)
hairColorElementSelector = mkSelector "hairColorElement"

-- | @Selector@ for @organDonorStatusElement@
organDonorStatusElementSelector :: Selector '[] (Id PKIdentityElement)
organDonorStatusElementSelector = mkSelector "organDonorStatusElement"

-- | @Selector@ for @veteranStatusElement@
veteranStatusElementSelector :: Selector '[] (Id PKIdentityElement)
veteranStatusElementSelector = mkSelector "veteranStatusElement"

-- | @Selector@ for @issuingAuthorityElement@
issuingAuthorityElementSelector :: Selector '[] (Id PKIdentityElement)
issuingAuthorityElementSelector = mkSelector "issuingAuthorityElement"

-- | @Selector@ for @documentIssueDateElement@
documentIssueDateElementSelector :: Selector '[] (Id PKIdentityElement)
documentIssueDateElementSelector = mkSelector "documentIssueDateElement"

-- | @Selector@ for @documentExpirationDateElement@
documentExpirationDateElementSelector :: Selector '[] (Id PKIdentityElement)
documentExpirationDateElementSelector = mkSelector "documentExpirationDateElement"

-- | @Selector@ for @documentDHSComplianceStatusElement@
documentDHSComplianceStatusElementSelector :: Selector '[] (Id PKIdentityElement)
documentDHSComplianceStatusElementSelector = mkSelector "documentDHSComplianceStatusElement"

-- | @Selector@ for @documentNumberElement@
documentNumberElementSelector :: Selector '[] (Id PKIdentityElement)
documentNumberElementSelector = mkSelector "documentNumberElement"

-- | @Selector@ for @drivingPrivilegesElement@
drivingPrivilegesElementSelector :: Selector '[] (Id PKIdentityElement)
drivingPrivilegesElementSelector = mkSelector "drivingPrivilegesElement"

-- | @Selector@ for @ageElement@
ageElementSelector :: Selector '[] (Id PKIdentityElement)
ageElementSelector = mkSelector "ageElement"

-- | @Selector@ for @dateOfBirthElement@
dateOfBirthElementSelector :: Selector '[] (Id PKIdentityElement)
dateOfBirthElementSelector = mkSelector "dateOfBirthElement"

-- | @Selector@ for @sexElement@
sexElementSelector :: Selector '[] (Id PKIdentityElement)
sexElementSelector = mkSelector "sexElement"

