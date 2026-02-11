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
  , deviceAccountIdentifierSelector
  , transactionIdentifierSelector
  , barcodeIdentifierSelector
  , rawMerchantNameSelector
  , merchantNameSelector
  , transactionDateSelector
  , currencyCodeSelector
  , amountSelector
  , transactionStatusSelector
  , partialSignatureSelector


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

-- | @- deviceAccountIdentifier@
deviceAccountIdentifier :: IsPKBarcodeEventSignatureRequest pkBarcodeEventSignatureRequest => pkBarcodeEventSignatureRequest -> IO (Id NSString)
deviceAccountIdentifier pkBarcodeEventSignatureRequest  =
  sendMsg pkBarcodeEventSignatureRequest (mkSelector "deviceAccountIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- transactionIdentifier@
transactionIdentifier :: IsPKBarcodeEventSignatureRequest pkBarcodeEventSignatureRequest => pkBarcodeEventSignatureRequest -> IO (Id NSString)
transactionIdentifier pkBarcodeEventSignatureRequest  =
  sendMsg pkBarcodeEventSignatureRequest (mkSelector "transactionIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- barcodeIdentifier@
barcodeIdentifier :: IsPKBarcodeEventSignatureRequest pkBarcodeEventSignatureRequest => pkBarcodeEventSignatureRequest -> IO (Id NSString)
barcodeIdentifier pkBarcodeEventSignatureRequest  =
  sendMsg pkBarcodeEventSignatureRequest (mkSelector "barcodeIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rawMerchantName@
rawMerchantName :: IsPKBarcodeEventSignatureRequest pkBarcodeEventSignatureRequest => pkBarcodeEventSignatureRequest -> IO (Id NSString)
rawMerchantName pkBarcodeEventSignatureRequest  =
  sendMsg pkBarcodeEventSignatureRequest (mkSelector "rawMerchantName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- merchantName@
merchantName :: IsPKBarcodeEventSignatureRequest pkBarcodeEventSignatureRequest => pkBarcodeEventSignatureRequest -> IO (Id NSString)
merchantName pkBarcodeEventSignatureRequest  =
  sendMsg pkBarcodeEventSignatureRequest (mkSelector "merchantName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- transactionDate@
transactionDate :: IsPKBarcodeEventSignatureRequest pkBarcodeEventSignatureRequest => pkBarcodeEventSignatureRequest -> IO (Id NSDate)
transactionDate pkBarcodeEventSignatureRequest  =
  sendMsg pkBarcodeEventSignatureRequest (mkSelector "transactionDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currencyCode@
currencyCode :: IsPKBarcodeEventSignatureRequest pkBarcodeEventSignatureRequest => pkBarcodeEventSignatureRequest -> IO (Id NSString)
currencyCode pkBarcodeEventSignatureRequest  =
  sendMsg pkBarcodeEventSignatureRequest (mkSelector "currencyCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- amount@
amount :: IsPKBarcodeEventSignatureRequest pkBarcodeEventSignatureRequest => pkBarcodeEventSignatureRequest -> IO (Id NSNumber)
amount pkBarcodeEventSignatureRequest  =
  sendMsg pkBarcodeEventSignatureRequest (mkSelector "amount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- transactionStatus@
transactionStatus :: IsPKBarcodeEventSignatureRequest pkBarcodeEventSignatureRequest => pkBarcodeEventSignatureRequest -> IO (Id NSString)
transactionStatus pkBarcodeEventSignatureRequest  =
  sendMsg pkBarcodeEventSignatureRequest (mkSelector "transactionStatus") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- partialSignature@
partialSignature :: IsPKBarcodeEventSignatureRequest pkBarcodeEventSignatureRequest => pkBarcodeEventSignatureRequest -> IO (Id NSData)
partialSignature pkBarcodeEventSignatureRequest  =
  sendMsg pkBarcodeEventSignatureRequest (mkSelector "partialSignature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deviceAccountIdentifier@
deviceAccountIdentifierSelector :: Selector
deviceAccountIdentifierSelector = mkSelector "deviceAccountIdentifier"

-- | @Selector@ for @transactionIdentifier@
transactionIdentifierSelector :: Selector
transactionIdentifierSelector = mkSelector "transactionIdentifier"

-- | @Selector@ for @barcodeIdentifier@
barcodeIdentifierSelector :: Selector
barcodeIdentifierSelector = mkSelector "barcodeIdentifier"

-- | @Selector@ for @rawMerchantName@
rawMerchantNameSelector :: Selector
rawMerchantNameSelector = mkSelector "rawMerchantName"

-- | @Selector@ for @merchantName@
merchantNameSelector :: Selector
merchantNameSelector = mkSelector "merchantName"

-- | @Selector@ for @transactionDate@
transactionDateSelector :: Selector
transactionDateSelector = mkSelector "transactionDate"

-- | @Selector@ for @currencyCode@
currencyCodeSelector :: Selector
currencyCodeSelector = mkSelector "currencyCode"

-- | @Selector@ for @amount@
amountSelector :: Selector
amountSelector = mkSelector "amount"

-- | @Selector@ for @transactionStatus@
transactionStatusSelector :: Selector
transactionStatusSelector = mkSelector "transactionStatus"

-- | @Selector@ for @partialSignature@
partialSignatureSelector :: Selector
partialSignatureSelector = mkSelector "partialSignature"

