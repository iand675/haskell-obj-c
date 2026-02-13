{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterSignVIDVerificationResponseParams@.
module ObjC.Matter.MTROperationalCredentialsClusterSignVIDVerificationResponseParams
  ( MTROperationalCredentialsClusterSignVIDVerificationResponseParams
  , IsMTROperationalCredentialsClusterSignVIDVerificationResponseParams(..)
  , initWithResponseValue_error
  , fabricIndex
  , setFabricIndex
  , fabricBindingVersion
  , setFabricBindingVersion
  , signature
  , setSignature
  , fabricBindingVersionSelector
  , fabricIndexSelector
  , initWithResponseValue_errorSelector
  , setFabricBindingVersionSelector
  , setFabricIndexSelector
  , setSignatureSelector
  , signatureSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTROperationalCredentialsClusterSignVIDVerificationResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTROperationalCredentialsClusterSignVIDVerificationResponseParams mtrOperationalCredentialsClusterSignVIDVerificationResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrOperationalCredentialsClusterSignVIDVerificationResponseParams -> responseValue -> error_ -> IO (Id MTROperationalCredentialsClusterSignVIDVerificationResponseParams)
initWithResponseValue_error mtrOperationalCredentialsClusterSignVIDVerificationResponseParams responseValue error_ =
  sendOwnedMessage mtrOperationalCredentialsClusterSignVIDVerificationResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- fabricIndex@
fabricIndex :: IsMTROperationalCredentialsClusterSignVIDVerificationResponseParams mtrOperationalCredentialsClusterSignVIDVerificationResponseParams => mtrOperationalCredentialsClusterSignVIDVerificationResponseParams -> IO (Id NSNumber)
fabricIndex mtrOperationalCredentialsClusterSignVIDVerificationResponseParams =
  sendMessage mtrOperationalCredentialsClusterSignVIDVerificationResponseParams fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTROperationalCredentialsClusterSignVIDVerificationResponseParams mtrOperationalCredentialsClusterSignVIDVerificationResponseParams, IsNSNumber value) => mtrOperationalCredentialsClusterSignVIDVerificationResponseParams -> value -> IO ()
setFabricIndex mtrOperationalCredentialsClusterSignVIDVerificationResponseParams value =
  sendMessage mtrOperationalCredentialsClusterSignVIDVerificationResponseParams setFabricIndexSelector (toNSNumber value)

-- | @- fabricBindingVersion@
fabricBindingVersion :: IsMTROperationalCredentialsClusterSignVIDVerificationResponseParams mtrOperationalCredentialsClusterSignVIDVerificationResponseParams => mtrOperationalCredentialsClusterSignVIDVerificationResponseParams -> IO (Id NSNumber)
fabricBindingVersion mtrOperationalCredentialsClusterSignVIDVerificationResponseParams =
  sendMessage mtrOperationalCredentialsClusterSignVIDVerificationResponseParams fabricBindingVersionSelector

-- | @- setFabricBindingVersion:@
setFabricBindingVersion :: (IsMTROperationalCredentialsClusterSignVIDVerificationResponseParams mtrOperationalCredentialsClusterSignVIDVerificationResponseParams, IsNSNumber value) => mtrOperationalCredentialsClusterSignVIDVerificationResponseParams -> value -> IO ()
setFabricBindingVersion mtrOperationalCredentialsClusterSignVIDVerificationResponseParams value =
  sendMessage mtrOperationalCredentialsClusterSignVIDVerificationResponseParams setFabricBindingVersionSelector (toNSNumber value)

-- | @- signature@
signature :: IsMTROperationalCredentialsClusterSignVIDVerificationResponseParams mtrOperationalCredentialsClusterSignVIDVerificationResponseParams => mtrOperationalCredentialsClusterSignVIDVerificationResponseParams -> IO (Id NSData)
signature mtrOperationalCredentialsClusterSignVIDVerificationResponseParams =
  sendMessage mtrOperationalCredentialsClusterSignVIDVerificationResponseParams signatureSelector

-- | @- setSignature:@
setSignature :: (IsMTROperationalCredentialsClusterSignVIDVerificationResponseParams mtrOperationalCredentialsClusterSignVIDVerificationResponseParams, IsNSData value) => mtrOperationalCredentialsClusterSignVIDVerificationResponseParams -> value -> IO ()
setSignature mtrOperationalCredentialsClusterSignVIDVerificationResponseParams value =
  sendMessage mtrOperationalCredentialsClusterSignVIDVerificationResponseParams setSignatureSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTROperationalCredentialsClusterSignVIDVerificationResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

-- | @Selector@ for @fabricBindingVersion@
fabricBindingVersionSelector :: Selector '[] (Id NSNumber)
fabricBindingVersionSelector = mkSelector "fabricBindingVersion"

-- | @Selector@ for @setFabricBindingVersion:@
setFabricBindingVersionSelector :: Selector '[Id NSNumber] ()
setFabricBindingVersionSelector = mkSelector "setFabricBindingVersion:"

-- | @Selector@ for @signature@
signatureSelector :: Selector '[] (Id NSData)
signatureSelector = mkSelector "signature"

-- | @Selector@ for @setSignature:@
setSignatureSelector :: Selector '[Id NSData] ()
setSignatureSelector = mkSelector "setSignature:"

