{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPaymentInformation:@
initWithPaymentInformation :: (IsPKBarcodeEventMetadataResponse pkBarcodeEventMetadataResponse, IsNSData paymentInformation) => pkBarcodeEventMetadataResponse -> paymentInformation -> IO (Id PKBarcodeEventMetadataResponse)
initWithPaymentInformation pkBarcodeEventMetadataResponse paymentInformation =
  sendOwnedMessage pkBarcodeEventMetadataResponse initWithPaymentInformationSelector (toNSData paymentInformation)

-- | @- paymentInformation@
paymentInformation :: IsPKBarcodeEventMetadataResponse pkBarcodeEventMetadataResponse => pkBarcodeEventMetadataResponse -> IO (Id NSData)
paymentInformation pkBarcodeEventMetadataResponse =
  sendMessage pkBarcodeEventMetadataResponse paymentInformationSelector

-- | @- setPaymentInformation:@
setPaymentInformation :: (IsPKBarcodeEventMetadataResponse pkBarcodeEventMetadataResponse, IsNSData value) => pkBarcodeEventMetadataResponse -> value -> IO ()
setPaymentInformation pkBarcodeEventMetadataResponse value =
  sendMessage pkBarcodeEventMetadataResponse setPaymentInformationSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPaymentInformation:@
initWithPaymentInformationSelector :: Selector '[Id NSData] (Id PKBarcodeEventMetadataResponse)
initWithPaymentInformationSelector = mkSelector "initWithPaymentInformation:"

-- | @Selector@ for @paymentInformation@
paymentInformationSelector :: Selector '[] (Id NSData)
paymentInformationSelector = mkSelector "paymentInformation"

-- | @Selector@ for @setPaymentInformation:@
setPaymentInformationSelector :: Selector '[Id NSData] ()
setPaymentInformationSelector = mkSelector "setPaymentInformation:"

