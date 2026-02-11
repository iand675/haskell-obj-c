{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKBarcodeEventMetadataResponse@.
module ObjC.PassKit.PKBarcodeEventMetadataResponse
  ( PKBarcodeEventMetadataResponse
  , IsPKBarcodeEventMetadataResponse(..)
  , initWithPaymentInformation
  , paymentInformation
  , setPaymentInformation
  , initWithPaymentInformationSelector
  , paymentInformationSelector
  , setPaymentInformationSelector


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

-- | @- initWithPaymentInformation:@
initWithPaymentInformation :: (IsPKBarcodeEventMetadataResponse pkBarcodeEventMetadataResponse, IsNSData paymentInformation) => pkBarcodeEventMetadataResponse -> paymentInformation -> IO (Id PKBarcodeEventMetadataResponse)
initWithPaymentInformation pkBarcodeEventMetadataResponse  paymentInformation =
withObjCPtr paymentInformation $ \raw_paymentInformation ->
    sendMsg pkBarcodeEventMetadataResponse (mkSelector "initWithPaymentInformation:") (retPtr retVoid) [argPtr (castPtr raw_paymentInformation :: Ptr ())] >>= ownedObject . castPtr

-- | @- paymentInformation@
paymentInformation :: IsPKBarcodeEventMetadataResponse pkBarcodeEventMetadataResponse => pkBarcodeEventMetadataResponse -> IO (Id NSData)
paymentInformation pkBarcodeEventMetadataResponse  =
  sendMsg pkBarcodeEventMetadataResponse (mkSelector "paymentInformation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPaymentInformation:@
setPaymentInformation :: (IsPKBarcodeEventMetadataResponse pkBarcodeEventMetadataResponse, IsNSData value) => pkBarcodeEventMetadataResponse -> value -> IO ()
setPaymentInformation pkBarcodeEventMetadataResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkBarcodeEventMetadataResponse (mkSelector "setPaymentInformation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPaymentInformation:@
initWithPaymentInformationSelector :: Selector
initWithPaymentInformationSelector = mkSelector "initWithPaymentInformation:"

-- | @Selector@ for @paymentInformation@
paymentInformationSelector :: Selector
paymentInformationSelector = mkSelector "paymentInformation"

-- | @Selector@ for @setPaymentInformation:@
setPaymentInformationSelector :: Selector
setPaymentInformationSelector = mkSelector "setPaymentInformation:"

