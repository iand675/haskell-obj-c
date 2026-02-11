{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SWSignedPersonIdentityProof@.
module ObjC.SharedWithYouCore.SWSignedPersonIdentityProof
  ( SWSignedPersonIdentityProof
  , IsSWSignedPersonIdentityProof(..)
  , initWithPersonIdentityProof_signatureData
  , signatureData
  , initWithPersonIdentityProof_signatureDataSelector
  , signatureDataSelector


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

import ObjC.SharedWithYouCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPersonIdentityProof:signatureData:@
initWithPersonIdentityProof_signatureData :: (IsSWSignedPersonIdentityProof swSignedPersonIdentityProof, IsSWPersonIdentityProof personIdentityProof, IsNSData data_) => swSignedPersonIdentityProof -> personIdentityProof -> data_ -> IO (Id SWSignedPersonIdentityProof)
initWithPersonIdentityProof_signatureData swSignedPersonIdentityProof  personIdentityProof data_ =
withObjCPtr personIdentityProof $ \raw_personIdentityProof ->
  withObjCPtr data_ $ \raw_data_ ->
      sendMsg swSignedPersonIdentityProof (mkSelector "initWithPersonIdentityProof:signatureData:") (retPtr retVoid) [argPtr (castPtr raw_personIdentityProof :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | The signature created by signing the data with this identity.
--
-- ObjC selector: @- signatureData@
signatureData :: IsSWSignedPersonIdentityProof swSignedPersonIdentityProof => swSignedPersonIdentityProof -> IO (Id NSData)
signatureData swSignedPersonIdentityProof  =
  sendMsg swSignedPersonIdentityProof (mkSelector "signatureData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPersonIdentityProof:signatureData:@
initWithPersonIdentityProof_signatureDataSelector :: Selector
initWithPersonIdentityProof_signatureDataSelector = mkSelector "initWithPersonIdentityProof:signatureData:"

-- | @Selector@ for @signatureData@
signatureDataSelector :: Selector
signatureDataSelector = mkSelector "signatureData"

