{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKPass@.
module ObjC.PassKit.PKPass
  ( PKPass
  , IsPKPass(..)
  , initWithData_error
  , localizedValueForFieldKey
  , passType
  , paymentPass
  , secureElementPass
  , serialNumber
  , passTypeIdentifier
  , webServiceURL
  , authenticationToken
  , localizedName
  , localizedDescription
  , organizationName
  , relevantDate
  , relevantDates
  , userInfo
  , passURL
  , remotePass
  , deviceName
  , authenticationTokenSelector
  , deviceNameSelector
  , initWithData_errorSelector
  , localizedDescriptionSelector
  , localizedNameSelector
  , localizedValueForFieldKeySelector
  , organizationNameSelector
  , passTypeIdentifierSelector
  , passTypeSelector
  , passURLSelector
  , paymentPassSelector
  , relevantDateSelector
  , relevantDatesSelector
  , remotePassSelector
  , secureElementPassSelector
  , serialNumberSelector
  , userInfoSelector
  , webServiceURLSelector

  -- * Enum types
  , PKPassType(PKPassType)
  , pattern PKPassTypeBarcode
  , pattern PKPassTypeSecureElement
  , pattern PKPassTypePayment
  , pattern PKPassTypeAny

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.PassKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithData:error:@
initWithData_error :: (IsPKPass pkPass, IsNSData data_, IsNSError error_) => pkPass -> data_ -> error_ -> IO (Id PKPass)
initWithData_error pkPass data_ error_ =
  sendOwnedMessage pkPass initWithData_errorSelector (toNSData data_) (toNSError error_)

-- | @- localizedValueForFieldKey:@
localizedValueForFieldKey :: (IsPKPass pkPass, IsNSString key) => pkPass -> key -> IO RawId
localizedValueForFieldKey pkPass key =
  sendMessage pkPass localizedValueForFieldKeySelector (toNSString key)

-- | @- passType@
passType :: IsPKPass pkPass => pkPass -> IO PKPassType
passType pkPass =
  sendMessage pkPass passTypeSelector

-- | @- paymentPass@
paymentPass :: IsPKPass pkPass => pkPass -> IO (Id PKPaymentPass)
paymentPass pkPass =
  sendMessage pkPass paymentPassSelector

-- | @- secureElementPass@
secureElementPass :: IsPKPass pkPass => pkPass -> IO (Id PKSecureElementPass)
secureElementPass pkPass =
  sendMessage pkPass secureElementPassSelector

-- | @- serialNumber@
serialNumber :: IsPKPass pkPass => pkPass -> IO (Id NSString)
serialNumber pkPass =
  sendMessage pkPass serialNumberSelector

-- | @- passTypeIdentifier@
passTypeIdentifier :: IsPKPass pkPass => pkPass -> IO (Id NSString)
passTypeIdentifier pkPass =
  sendMessage pkPass passTypeIdentifierSelector

-- | @- webServiceURL@
webServiceURL :: IsPKPass pkPass => pkPass -> IO (Id NSURL)
webServiceURL pkPass =
  sendMessage pkPass webServiceURLSelector

-- | @- authenticationToken@
authenticationToken :: IsPKPass pkPass => pkPass -> IO (Id NSString)
authenticationToken pkPass =
  sendMessage pkPass authenticationTokenSelector

-- | @- localizedName@
localizedName :: IsPKPass pkPass => pkPass -> IO (Id NSString)
localizedName pkPass =
  sendMessage pkPass localizedNameSelector

-- | @- localizedDescription@
localizedDescription :: IsPKPass pkPass => pkPass -> IO (Id NSString)
localizedDescription pkPass =
  sendMessage pkPass localizedDescriptionSelector

-- | @- organizationName@
organizationName :: IsPKPass pkPass => pkPass -> IO (Id NSString)
organizationName pkPass =
  sendMessage pkPass organizationNameSelector

-- | @- relevantDate@
relevantDate :: IsPKPass pkPass => pkPass -> IO (Id NSDate)
relevantDate pkPass =
  sendMessage pkPass relevantDateSelector

-- | @- relevantDates@
relevantDates :: IsPKPass pkPass => pkPass -> IO (Id NSArray)
relevantDates pkPass =
  sendMessage pkPass relevantDatesSelector

-- | @- userInfo@
userInfo :: IsPKPass pkPass => pkPass -> IO (Id NSDictionary)
userInfo pkPass =
  sendMessage pkPass userInfoSelector

-- | @- passURL@
passURL :: IsPKPass pkPass => pkPass -> IO (Id NSURL)
passURL pkPass =
  sendMessage pkPass passURLSelector

-- | @- remotePass@
remotePass :: IsPKPass pkPass => pkPass -> IO Bool
remotePass pkPass =
  sendMessage pkPass remotePassSelector

-- | @- deviceName@
deviceName :: IsPKPass pkPass => pkPass -> IO (Id NSString)
deviceName pkPass =
  sendMessage pkPass deviceNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithData:error:@
initWithData_errorSelector :: Selector '[Id NSData, Id NSError] (Id PKPass)
initWithData_errorSelector = mkSelector "initWithData:error:"

-- | @Selector@ for @localizedValueForFieldKey:@
localizedValueForFieldKeySelector :: Selector '[Id NSString] RawId
localizedValueForFieldKeySelector = mkSelector "localizedValueForFieldKey:"

-- | @Selector@ for @passType@
passTypeSelector :: Selector '[] PKPassType
passTypeSelector = mkSelector "passType"

-- | @Selector@ for @paymentPass@
paymentPassSelector :: Selector '[] (Id PKPaymentPass)
paymentPassSelector = mkSelector "paymentPass"

-- | @Selector@ for @secureElementPass@
secureElementPassSelector :: Selector '[] (Id PKSecureElementPass)
secureElementPassSelector = mkSelector "secureElementPass"

-- | @Selector@ for @serialNumber@
serialNumberSelector :: Selector '[] (Id NSString)
serialNumberSelector = mkSelector "serialNumber"

-- | @Selector@ for @passTypeIdentifier@
passTypeIdentifierSelector :: Selector '[] (Id NSString)
passTypeIdentifierSelector = mkSelector "passTypeIdentifier"

-- | @Selector@ for @webServiceURL@
webServiceURLSelector :: Selector '[] (Id NSURL)
webServiceURLSelector = mkSelector "webServiceURL"

-- | @Selector@ for @authenticationToken@
authenticationTokenSelector :: Selector '[] (Id NSString)
authenticationTokenSelector = mkSelector "authenticationToken"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector '[] (Id NSString)
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector '[] (Id NSString)
localizedDescriptionSelector = mkSelector "localizedDescription"

-- | @Selector@ for @organizationName@
organizationNameSelector :: Selector '[] (Id NSString)
organizationNameSelector = mkSelector "organizationName"

-- | @Selector@ for @relevantDate@
relevantDateSelector :: Selector '[] (Id NSDate)
relevantDateSelector = mkSelector "relevantDate"

-- | @Selector@ for @relevantDates@
relevantDatesSelector :: Selector '[] (Id NSArray)
relevantDatesSelector = mkSelector "relevantDates"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] (Id NSDictionary)
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @passURL@
passURLSelector :: Selector '[] (Id NSURL)
passURLSelector = mkSelector "passURL"

-- | @Selector@ for @remotePass@
remotePassSelector :: Selector '[] Bool
remotePassSelector = mkSelector "remotePass"

-- | @Selector@ for @deviceName@
deviceNameSelector :: Selector '[] (Id NSString)
deviceNameSelector = mkSelector "deviceName"

