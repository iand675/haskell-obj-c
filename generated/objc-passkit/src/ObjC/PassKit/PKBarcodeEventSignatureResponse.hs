{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKBarcodeEventSignatureResponse@.
module ObjC.PassKit.PKBarcodeEventSignatureResponse
  ( PKBarcodeEventSignatureResponse
  , IsPKBarcodeEventSignatureResponse(..)
  , initWithSignedData
  , signedData
  , setSignedData
  , initWithSignedDataSelector
  , signedDataSelector
  , setSignedDataSelector


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

-- | @- initWithSignedData:@
initWithSignedData :: (IsPKBarcodeEventSignatureResponse pkBarcodeEventSignatureResponse, IsNSData signedData) => pkBarcodeEventSignatureResponse -> signedData -> IO (Id PKBarcodeEventSignatureResponse)
initWithSignedData pkBarcodeEventSignatureResponse  signedData =
withObjCPtr signedData $ \raw_signedData ->
    sendMsg pkBarcodeEventSignatureResponse (mkSelector "initWithSignedData:") (retPtr retVoid) [argPtr (castPtr raw_signedData :: Ptr ())] >>= ownedObject . castPtr

-- | @- signedData@
signedData :: IsPKBarcodeEventSignatureResponse pkBarcodeEventSignatureResponse => pkBarcodeEventSignatureResponse -> IO (Id NSData)
signedData pkBarcodeEventSignatureResponse  =
  sendMsg pkBarcodeEventSignatureResponse (mkSelector "signedData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSignedData:@
setSignedData :: (IsPKBarcodeEventSignatureResponse pkBarcodeEventSignatureResponse, IsNSData value) => pkBarcodeEventSignatureResponse -> value -> IO ()
setSignedData pkBarcodeEventSignatureResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkBarcodeEventSignatureResponse (mkSelector "setSignedData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSignedData:@
initWithSignedDataSelector :: Selector
initWithSignedDataSelector = mkSelector "initWithSignedData:"

-- | @Selector@ for @signedData@
signedDataSelector :: Selector
signedDataSelector = mkSelector "signedData"

-- | @Selector@ for @setSignedData:@
setSignedDataSelector :: Selector
setSignedDataSelector = mkSelector "setSignedData:"

