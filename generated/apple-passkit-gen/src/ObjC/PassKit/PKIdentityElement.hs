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
  , ageThresholdElementWithAgeSelector
  , initSelector
  , newSelector
  , givenNameElementSelector
  , familyNameElementSelector
  , portraitElementSelector
  , addressElementSelector
  , heightElementSelector
  , weightElementSelector
  , eyeColorElementSelector
  , hairColorElementSelector
  , organDonorStatusElementSelector
  , veteranStatusElementSelector
  , issuingAuthorityElementSelector
  , documentIssueDateElementSelector
  , documentExpirationDateElementSelector
  , documentDHSComplianceStatusElementSelector
  , documentNumberElementSelector
  , drivingPrivilegesElementSelector
  , ageElementSelector
  , dateOfBirthElementSelector
  , sexElementSelector


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

-- | Boolean indicating whether the user's age is at least the given age. For example, ageThresholdElementWithAge:21 will return true if the user is at least 21 years old. This value is only available for a given age if it was provided by the issuer. If this value is not available, it will automatically fall back to a request for age.
--
-- ObjC selector: @+ ageThresholdElementWithAge:@
ageThresholdElementWithAge :: CLong -> IO (Id PKIdentityElement)
ageThresholdElementWithAge age =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMsg cls' (mkSelector "ageThresholdElementWithAge:") (retPtr retVoid) [argCLong age] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsPKIdentityElement pkIdentityElement => pkIdentityElement -> IO (Id PKIdentityElement)
init_ pkIdentityElement  =
    sendMsg pkIdentityElement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PKIdentityElement)
new  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The user's given name or first name.
--
-- ObjC selector: @+ givenNameElement@
givenNameElement :: IO (Id PKIdentityElement)
givenNameElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMsg cls' (mkSelector "givenNameElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The user's family name or last name.
--
-- ObjC selector: @+ familyNameElement@
familyNameElement :: IO (Id PKIdentityElement)
familyNameElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMsg cls' (mkSelector "familyNameElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The portrait of the user on record with the issuer.
--
-- ObjC selector: @+ portraitElement@
portraitElement :: IO (Id PKIdentityElement)
portraitElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMsg cls' (mkSelector "portraitElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The address on record with the issuer.
--
-- ObjC selector: @+ addressElement@
addressElement :: IO (Id PKIdentityElement)
addressElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMsg cls' (mkSelector "addressElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The user's height on record with the issuer.
--
-- ObjC selector: @+ heightElement@
heightElement :: IO (Id PKIdentityElement)
heightElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMsg cls' (mkSelector "heightElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The user's weight on record with the issuer.
--
-- ObjC selector: @+ weightElement@
weightElement :: IO (Id PKIdentityElement)
weightElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMsg cls' (mkSelector "weightElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The user's eye color on record with the issuer.
--
-- ObjC selector: @+ eyeColorElement@
eyeColorElement :: IO (Id PKIdentityElement)
eyeColorElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMsg cls' (mkSelector "eyeColorElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The user's hair color on record with the issuer.
--
-- ObjC selector: @+ hairColorElement@
hairColorElement :: IO (Id PKIdentityElement)
hairColorElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMsg cls' (mkSelector "hairColorElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The user's organ donor status on record with the issuer.
--
-- ObjC selector: @+ organDonorStatusElement@
organDonorStatusElement :: IO (Id PKIdentityElement)
organDonorStatusElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMsg cls' (mkSelector "organDonorStatusElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The user's veteran status on record with the issuer.
--
-- ObjC selector: @+ veteranStatusElement@
veteranStatusElement :: IO (Id PKIdentityElement)
veteranStatusElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMsg cls' (mkSelector "veteranStatusElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The state or government that issued the identity document.
--
-- ObjC selector: @+ issuingAuthorityElement@
issuingAuthorityElement :: IO (Id PKIdentityElement)
issuingAuthorityElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMsg cls' (mkSelector "issuingAuthorityElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The document's issue date. This is usually the issue date of the corresponding physical document, if applicable.
--
-- ObjC selector: @+ documentIssueDateElement@
documentIssueDateElement :: IO (Id PKIdentityElement)
documentIssueDateElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMsg cls' (mkSelector "documentIssueDateElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The document's expiration date. This is usually the expiration date of the corresponding physical document, if applicable.
--
-- ObjC selector: @+ documentExpirationDateElement@
documentExpirationDateElement :: IO (Id PKIdentityElement)
documentExpirationDateElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMsg cls' (mkSelector "documentExpirationDateElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The document's DHS (U.S. Department of Homeland Security) compliance status.
--
-- This is also known as the document's "REAL ID status".
--
-- ObjC selector: @+ documentDHSComplianceStatusElement@
documentDHSComplianceStatusElement :: IO (Id PKIdentityElement)
documentDHSComplianceStatusElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMsg cls' (mkSelector "documentDHSComplianceStatusElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The document's number, as defined by the document's issuing authority.
--
-- ObjC selector: @+ documentNumberElement@
documentNumberElement :: IO (Id PKIdentityElement)
documentNumberElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMsg cls' (mkSelector "documentNumberElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The user's driving privileges.
--
-- ObjC selector: @+ drivingPrivilegesElement@
drivingPrivilegesElement :: IO (Id PKIdentityElement)
drivingPrivilegesElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMsg cls' (mkSelector "drivingPrivilegesElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The user's age in years.
--
-- ObjC selector: @+ ageElement@
ageElement :: IO (Id PKIdentityElement)
ageElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMsg cls' (mkSelector "ageElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The user's date of birth.
--
-- ObjC selector: @+ dateOfBirthElement@
dateOfBirthElement :: IO (Id PKIdentityElement)
dateOfBirthElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMsg cls' (mkSelector "dateOfBirthElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The user's sex.
--
-- ObjC selector: @+ sexElement@
sexElement :: IO (Id PKIdentityElement)
sexElement  =
  do
    cls' <- getRequiredClass "PKIdentityElement"
    sendClassMsg cls' (mkSelector "sexElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ageThresholdElementWithAge:@
ageThresholdElementWithAgeSelector :: Selector
ageThresholdElementWithAgeSelector = mkSelector "ageThresholdElementWithAge:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @givenNameElement@
givenNameElementSelector :: Selector
givenNameElementSelector = mkSelector "givenNameElement"

-- | @Selector@ for @familyNameElement@
familyNameElementSelector :: Selector
familyNameElementSelector = mkSelector "familyNameElement"

-- | @Selector@ for @portraitElement@
portraitElementSelector :: Selector
portraitElementSelector = mkSelector "portraitElement"

-- | @Selector@ for @addressElement@
addressElementSelector :: Selector
addressElementSelector = mkSelector "addressElement"

-- | @Selector@ for @heightElement@
heightElementSelector :: Selector
heightElementSelector = mkSelector "heightElement"

-- | @Selector@ for @weightElement@
weightElementSelector :: Selector
weightElementSelector = mkSelector "weightElement"

-- | @Selector@ for @eyeColorElement@
eyeColorElementSelector :: Selector
eyeColorElementSelector = mkSelector "eyeColorElement"

-- | @Selector@ for @hairColorElement@
hairColorElementSelector :: Selector
hairColorElementSelector = mkSelector "hairColorElement"

-- | @Selector@ for @organDonorStatusElement@
organDonorStatusElementSelector :: Selector
organDonorStatusElementSelector = mkSelector "organDonorStatusElement"

-- | @Selector@ for @veteranStatusElement@
veteranStatusElementSelector :: Selector
veteranStatusElementSelector = mkSelector "veteranStatusElement"

-- | @Selector@ for @issuingAuthorityElement@
issuingAuthorityElementSelector :: Selector
issuingAuthorityElementSelector = mkSelector "issuingAuthorityElement"

-- | @Selector@ for @documentIssueDateElement@
documentIssueDateElementSelector :: Selector
documentIssueDateElementSelector = mkSelector "documentIssueDateElement"

-- | @Selector@ for @documentExpirationDateElement@
documentExpirationDateElementSelector :: Selector
documentExpirationDateElementSelector = mkSelector "documentExpirationDateElement"

-- | @Selector@ for @documentDHSComplianceStatusElement@
documentDHSComplianceStatusElementSelector :: Selector
documentDHSComplianceStatusElementSelector = mkSelector "documentDHSComplianceStatusElement"

-- | @Selector@ for @documentNumberElement@
documentNumberElementSelector :: Selector
documentNumberElementSelector = mkSelector "documentNumberElement"

-- | @Selector@ for @drivingPrivilegesElement@
drivingPrivilegesElementSelector :: Selector
drivingPrivilegesElementSelector = mkSelector "drivingPrivilegesElement"

-- | @Selector@ for @ageElement@
ageElementSelector :: Selector
ageElementSelector = mkSelector "ageElement"

-- | @Selector@ for @dateOfBirthElement@
dateOfBirthElementSelector :: Selector
dateOfBirthElementSelector = mkSelector "dateOfBirthElement"

-- | @Selector@ for @sexElement@
sexElementSelector :: Selector
sexElementSelector = mkSelector "sexElement"

