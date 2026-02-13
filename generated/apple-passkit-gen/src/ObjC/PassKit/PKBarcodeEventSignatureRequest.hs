{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKBarcodeEventSignatureRequest@.
module ObjC.PassKit.PKBarcodeEventSignatureRequest
  ( PKBarcodeEventSignatureRequest
  , IsPKBarcodeEventSignatureRequest(..)
  , deviceAccountIdentifier
  , transactionIdentifier
  , barcodeIdentifier
  , rawMerchantName
  , merchantName
  , transactionDate
  , currencyCode
  , amount
  , transactionStatus
  , partialSignature
  , amountSelector
  , barcodeIdentifierSelector
  , currencyCodeSelector
  , deviceAccountIdentifierSelector
  , merchantNameSelector
  , partialSignatureSelector
  , rawMerchantNameSelector
  , transactionDateSelector
  , transactionIdentifierSelector
  , transactionStatusSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- deviceAccountIdentifier@
deviceAccountIdentifier :: IsPKBarcodeEventSignatureRequest pkBarcodeEventSignatureRequest => pkBarcodeEventSignatureRequest -> IO (Id NSString)
deviceAccountIdentifier pkBarcodeEventSignatureRequest =
  sendMessage pkBarcodeEventSignatureRequest deviceAccountIdentifierSelector

-- | @- transactionIdentifier@
transactionIdentifier :: IsPKBarcodeEventSignatureRequest pkBarcodeEventSignatureRequest => pkBarcodeEventSignatureRequest -> IO (Id NSString)
transactionIdentifier pkBarcodeEventSignatureRequest =
  sendMessage pkBarcodeEventSignatureRequest transactionIdentifierSelector

-- | @- barcodeIdentifier@
barcodeIdentifier :: IsPKBarcodeEventSignatureRequest pkBarcodeEventSignatureRequest => pkBarcodeEventSignatureRequest -> IO (Id NSString)
barcodeIdentifier pkBarcodeEventSignatureRequest =
  sendMessage pkBarcodeEventSignatureRequest barcodeIdentifierSelector

-- | @- rawMerchantName@
rawMerchantName :: IsPKBarcodeEventSignatureRequest pkBarcodeEventSignatureRequest => pkBarcodeEventSignatureRequest -> IO (Id NSString)
rawMerchantName pkBarcodeEventSignatureRequest =
  sendMessage pkBarcodeEventSignatureRequest rawMerchantNameSelector

-- | @- merchantName@
merchantName :: IsPKBarcodeEventSignatureRequest pkBarcodeEventSignatureRequest => pkBarcodeEventSignatureRequest -> IO (Id NSString)
merchantName pkBarcodeEventSignatureRequest =
  sendMessage pkBarcodeEventSignatureRequest merchantNameSelector

-- | @- transactionDate@
transactionDate :: IsPKBarcodeEventSignatureRequest pkBarcodeEventSignatureRequest => pkBarcodeEventSignatureRequest -> IO (Id NSDate)
transactionDate pkBarcodeEventSignatureRequest =
  sendMessage pkBarcodeEventSignatureRequest transactionDateSelector

-- | @- currencyCode@
currencyCode :: IsPKBarcodeEventSignatureRequest pkBarcodeEventSignatureRequest => pkBarcodeEventSignatureRequest -> IO (Id NSString)
currencyCode pkBarcodeEventSignatureRequest =
  sendMessage pkBarcodeEventSignatureRequest currencyCodeSelector

-- | @- amount@
amount :: IsPKBarcodeEventSignatureRequest pkBarcodeEventSignatureRequest => pkBarcodeEventSignatureRequest -> IO (Id NSNumber)
amount pkBarcodeEventSignatureRequest =
  sendMessage pkBarcodeEventSignatureRequest amountSelector

-- | @- transactionStatus@
transactionStatus :: IsPKBarcodeEventSignatureRequest pkBarcodeEventSignatureRequest => pkBarcodeEventSignatureRequest -> IO (Id NSString)
transactionStatus pkBarcodeEventSignatureRequest =
  sendMessage pkBarcodeEventSignatureRequest transactionStatusSelector

-- | @- partialSignature@
partialSignature :: IsPKBarcodeEventSignatureRequest pkBarcodeEventSignatureRequest => pkBarcodeEventSignatureRequest -> IO (Id NSData)
partialSignature pkBarcodeEventSignatureRequest =
  sendMessage pkBarcodeEventSignatureRequest partialSignatureSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deviceAccountIdentifier@
deviceAccountIdentifierSelector :: Selector '[] (Id NSString)
deviceAccountIdentifierSelector = mkSelector "deviceAccountIdentifier"

-- | @Selector@ for @transactionIdentifier@
transactionIdentifierSelector :: Selector '[] (Id NSString)
transactionIdentifierSelector = mkSelector "transactionIdentifier"

-- | @Selector@ for @barcodeIdentifier@
barcodeIdentifierSelector :: Selector '[] (Id NSString)
barcodeIdentifierSelector = mkSelector "barcodeIdentifier"

-- | @Selector@ for @rawMerchantName@
rawMerchantNameSelector :: Selector '[] (Id NSString)
rawMerchantNameSelector = mkSelector "rawMerchantName"

-- | @Selector@ for @merchantName@
merchantNameSelector :: Selector '[] (Id NSString)
merchantNameSelector = mkSelector "merchantName"

-- | @Selector@ for @transactionDate@
transactionDateSelector :: Selector '[] (Id NSDate)
transactionDateSelector = mkSelector "transactionDate"

-- | @Selector@ for @currencyCode@
currencyCodeSelector :: Selector '[] (Id NSString)
currencyCodeSelector = mkSelector "currencyCode"

-- | @Selector@ for @amount@
amountSelector :: Selector '[] (Id NSNumber)
amountSelector = mkSelector "amount"

-- | @Selector@ for @transactionStatus@
transactionStatusSelector :: Selector '[] (Id NSString)
transactionStatusSelector = mkSelector "transactionStatus"

-- | @Selector@ for @partialSignature@
partialSignatureSelector :: Selector '[] (Id NSData)
partialSignatureSelector = mkSelector "partialSignature"

