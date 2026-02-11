{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKPaymentToken@.
module ObjC.PassKit.PKPaymentToken
  ( PKPaymentToken
  , IsPKPaymentToken(..)
  , paymentMethod
  , paymentInstrumentName
  , paymentNetwork
  , transactionIdentifier
  , paymentData
  , paymentMethodSelector
  , paymentInstrumentNameSelector
  , paymentNetworkSelector
  , transactionIdentifierSelector
  , paymentDataSelector


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

-- | @- paymentMethod@
paymentMethod :: IsPKPaymentToken pkPaymentToken => pkPaymentToken -> IO (Id PKPaymentMethod)
paymentMethod pkPaymentToken  =
    sendMsg pkPaymentToken (mkSelector "paymentMethod") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- paymentInstrumentName@
paymentInstrumentName :: IsPKPaymentToken pkPaymentToken => pkPaymentToken -> IO (Id NSString)
paymentInstrumentName pkPaymentToken  =
    sendMsg pkPaymentToken (mkSelector "paymentInstrumentName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- paymentNetwork@
paymentNetwork :: IsPKPaymentToken pkPaymentToken => pkPaymentToken -> IO (Id NSString)
paymentNetwork pkPaymentToken  =
    sendMsg pkPaymentToken (mkSelector "paymentNetwork") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- transactionIdentifier@
transactionIdentifier :: IsPKPaymentToken pkPaymentToken => pkPaymentToken -> IO (Id NSString)
transactionIdentifier pkPaymentToken  =
    sendMsg pkPaymentToken (mkSelector "transactionIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- paymentData@
paymentData :: IsPKPaymentToken pkPaymentToken => pkPaymentToken -> IO (Id NSData)
paymentData pkPaymentToken  =
    sendMsg pkPaymentToken (mkSelector "paymentData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @paymentMethod@
paymentMethodSelector :: Selector
paymentMethodSelector = mkSelector "paymentMethod"

-- | @Selector@ for @paymentInstrumentName@
paymentInstrumentNameSelector :: Selector
paymentInstrumentNameSelector = mkSelector "paymentInstrumentName"

-- | @Selector@ for @paymentNetwork@
paymentNetworkSelector :: Selector
paymentNetworkSelector = mkSelector "paymentNetwork"

-- | @Selector@ for @transactionIdentifier@
transactionIdentifierSelector :: Selector
transactionIdentifierSelector = mkSelector "transactionIdentifier"

-- | @Selector@ for @paymentData@
paymentDataSelector :: Selector
paymentDataSelector = mkSelector "paymentData"

