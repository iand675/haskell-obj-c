{-# LANGUAGE PatternSynonyms #-}
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
  , initWithData_errorSelector
  , localizedValueForFieldKeySelector
  , passTypeSelector
  , paymentPassSelector
  , secureElementPassSelector
  , serialNumberSelector
  , passTypeIdentifierSelector
  , webServiceURLSelector
  , authenticationTokenSelector
  , localizedNameSelector
  , localizedDescriptionSelector
  , organizationNameSelector
  , relevantDateSelector
  , relevantDatesSelector
  , userInfoSelector
  , passURLSelector
  , remotePassSelector
  , deviceNameSelector

  -- * Enum types
  , PKPassType(PKPassType)
  , pattern PKPassTypeBarcode
  , pattern PKPassTypeSecureElement
  , pattern PKPassTypePayment
  , pattern PKPassTypeAny

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
import ObjC.PassKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithData:error:@
initWithData_error :: (IsPKPass pkPass, IsNSData data_, IsNSError error_) => pkPass -> data_ -> error_ -> IO (Id PKPass)
initWithData_error pkPass  data_ error_ =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg pkPass (mkSelector "initWithData:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- localizedValueForFieldKey:@
localizedValueForFieldKey :: (IsPKPass pkPass, IsNSString key) => pkPass -> key -> IO RawId
localizedValueForFieldKey pkPass  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg pkPass (mkSelector "localizedValueForFieldKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- passType@
passType :: IsPKPass pkPass => pkPass -> IO PKPassType
passType pkPass  =
  fmap (coerce :: CULong -> PKPassType) $ sendMsg pkPass (mkSelector "passType") retCULong []

-- | @- paymentPass@
paymentPass :: IsPKPass pkPass => pkPass -> IO (Id PKPaymentPass)
paymentPass pkPass  =
  sendMsg pkPass (mkSelector "paymentPass") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- secureElementPass@
secureElementPass :: IsPKPass pkPass => pkPass -> IO (Id PKSecureElementPass)
secureElementPass pkPass  =
  sendMsg pkPass (mkSelector "secureElementPass") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- serialNumber@
serialNumber :: IsPKPass pkPass => pkPass -> IO (Id NSString)
serialNumber pkPass  =
  sendMsg pkPass (mkSelector "serialNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- passTypeIdentifier@
passTypeIdentifier :: IsPKPass pkPass => pkPass -> IO (Id NSString)
passTypeIdentifier pkPass  =
  sendMsg pkPass (mkSelector "passTypeIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- webServiceURL@
webServiceURL :: IsPKPass pkPass => pkPass -> IO (Id NSURL)
webServiceURL pkPass  =
  sendMsg pkPass (mkSelector "webServiceURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- authenticationToken@
authenticationToken :: IsPKPass pkPass => pkPass -> IO (Id NSString)
authenticationToken pkPass  =
  sendMsg pkPass (mkSelector "authenticationToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localizedName@
localizedName :: IsPKPass pkPass => pkPass -> IO (Id NSString)
localizedName pkPass  =
  sendMsg pkPass (mkSelector "localizedName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localizedDescription@
localizedDescription :: IsPKPass pkPass => pkPass -> IO (Id NSString)
localizedDescription pkPass  =
  sendMsg pkPass (mkSelector "localizedDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- organizationName@
organizationName :: IsPKPass pkPass => pkPass -> IO (Id NSString)
organizationName pkPass  =
  sendMsg pkPass (mkSelector "organizationName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- relevantDate@
relevantDate :: IsPKPass pkPass => pkPass -> IO (Id NSDate)
relevantDate pkPass  =
  sendMsg pkPass (mkSelector "relevantDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- relevantDates@
relevantDates :: IsPKPass pkPass => pkPass -> IO (Id NSArray)
relevantDates pkPass  =
  sendMsg pkPass (mkSelector "relevantDates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- userInfo@
userInfo :: IsPKPass pkPass => pkPass -> IO (Id NSDictionary)
userInfo pkPass  =
  sendMsg pkPass (mkSelector "userInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- passURL@
passURL :: IsPKPass pkPass => pkPass -> IO (Id NSURL)
passURL pkPass  =
  sendMsg pkPass (mkSelector "passURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- remotePass@
remotePass :: IsPKPass pkPass => pkPass -> IO Bool
remotePass pkPass  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkPass (mkSelector "remotePass") retCULong []

-- | @- deviceName@
deviceName :: IsPKPass pkPass => pkPass -> IO (Id NSString)
deviceName pkPass  =
  sendMsg pkPass (mkSelector "deviceName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithData:error:@
initWithData_errorSelector :: Selector
initWithData_errorSelector = mkSelector "initWithData:error:"

-- | @Selector@ for @localizedValueForFieldKey:@
localizedValueForFieldKeySelector :: Selector
localizedValueForFieldKeySelector = mkSelector "localizedValueForFieldKey:"

-- | @Selector@ for @passType@
passTypeSelector :: Selector
passTypeSelector = mkSelector "passType"

-- | @Selector@ for @paymentPass@
paymentPassSelector :: Selector
paymentPassSelector = mkSelector "paymentPass"

-- | @Selector@ for @secureElementPass@
secureElementPassSelector :: Selector
secureElementPassSelector = mkSelector "secureElementPass"

-- | @Selector@ for @serialNumber@
serialNumberSelector :: Selector
serialNumberSelector = mkSelector "serialNumber"

-- | @Selector@ for @passTypeIdentifier@
passTypeIdentifierSelector :: Selector
passTypeIdentifierSelector = mkSelector "passTypeIdentifier"

-- | @Selector@ for @webServiceURL@
webServiceURLSelector :: Selector
webServiceURLSelector = mkSelector "webServiceURL"

-- | @Selector@ for @authenticationToken@
authenticationTokenSelector :: Selector
authenticationTokenSelector = mkSelector "authenticationToken"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector
localizedDescriptionSelector = mkSelector "localizedDescription"

-- | @Selector@ for @organizationName@
organizationNameSelector :: Selector
organizationNameSelector = mkSelector "organizationName"

-- | @Selector@ for @relevantDate@
relevantDateSelector :: Selector
relevantDateSelector = mkSelector "relevantDate"

-- | @Selector@ for @relevantDates@
relevantDatesSelector :: Selector
relevantDatesSelector = mkSelector "relevantDates"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @passURL@
passURLSelector :: Selector
passURLSelector = mkSelector "passURL"

-- | @Selector@ for @remotePass@
remotePassSelector :: Selector
remotePassSelector = mkSelector "remotePass"

-- | @Selector@ for @deviceName@
deviceNameSelector :: Selector
deviceNameSelector = mkSelector "deviceName"

