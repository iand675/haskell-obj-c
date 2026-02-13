{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SharedWithYouCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPersonIdentityProof:signatureData:@
initWithPersonIdentityProof_signatureData :: (IsSWSignedPersonIdentityProof swSignedPersonIdentityProof, IsSWPersonIdentityProof personIdentityProof, IsNSData data_) => swSignedPersonIdentityProof -> personIdentityProof -> data_ -> IO (Id SWSignedPersonIdentityProof)
initWithPersonIdentityProof_signatureData swSignedPersonIdentityProof personIdentityProof data_ =
  sendOwnedMessage swSignedPersonIdentityProof initWithPersonIdentityProof_signatureDataSelector (toSWPersonIdentityProof personIdentityProof) (toNSData data_)

-- | The signature created by signing the data with this identity.
--
-- ObjC selector: @- signatureData@
signatureData :: IsSWSignedPersonIdentityProof swSignedPersonIdentityProof => swSignedPersonIdentityProof -> IO (Id NSData)
signatureData swSignedPersonIdentityProof =
  sendMessage swSignedPersonIdentityProof signatureDataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPersonIdentityProof:signatureData:@
initWithPersonIdentityProof_signatureDataSelector :: Selector '[Id SWPersonIdentityProof, Id NSData] (Id SWSignedPersonIdentityProof)
initWithPersonIdentityProof_signatureDataSelector = mkSelector "initWithPersonIdentityProof:signatureData:"

-- | @Selector@ for @signatureData@
signatureDataSelector :: Selector '[] (Id NSData)
signatureDataSelector = mkSelector "signatureData"

