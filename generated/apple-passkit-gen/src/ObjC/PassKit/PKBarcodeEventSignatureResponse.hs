{-# LANGUAGE DataKinds #-}
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
  , setSignedDataSelector
  , signedDataSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithSignedData:@
initWithSignedData :: (IsPKBarcodeEventSignatureResponse pkBarcodeEventSignatureResponse, IsNSData signedData) => pkBarcodeEventSignatureResponse -> signedData -> IO (Id PKBarcodeEventSignatureResponse)
initWithSignedData pkBarcodeEventSignatureResponse signedData =
  sendOwnedMessage pkBarcodeEventSignatureResponse initWithSignedDataSelector (toNSData signedData)

-- | @- signedData@
signedData :: IsPKBarcodeEventSignatureResponse pkBarcodeEventSignatureResponse => pkBarcodeEventSignatureResponse -> IO (Id NSData)
signedData pkBarcodeEventSignatureResponse =
  sendMessage pkBarcodeEventSignatureResponse signedDataSelector

-- | @- setSignedData:@
setSignedData :: (IsPKBarcodeEventSignatureResponse pkBarcodeEventSignatureResponse, IsNSData value) => pkBarcodeEventSignatureResponse -> value -> IO ()
setSignedData pkBarcodeEventSignatureResponse value =
  sendMessage pkBarcodeEventSignatureResponse setSignedDataSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSignedData:@
initWithSignedDataSelector :: Selector '[Id NSData] (Id PKBarcodeEventSignatureResponse)
initWithSignedDataSelector = mkSelector "initWithSignedData:"

-- | @Selector@ for @signedData@
signedDataSelector :: Selector '[] (Id NSData)
signedDataSelector = mkSelector "signedData"

-- | @Selector@ for @setSignedData:@
setSignedDataSelector :: Selector '[Id NSData] ()
setSignedDataSelector = mkSelector "setSignedData:"

