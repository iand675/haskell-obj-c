{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricAdministratorClusterICACCSRResponseParams@.
module ObjC.Matter.MTRJointFabricAdministratorClusterICACCSRResponseParams
  ( MTRJointFabricAdministratorClusterICACCSRResponseParams
  , IsMTRJointFabricAdministratorClusterICACCSRResponseParams(..)
  , initWithResponseValue_error
  , icaccsr
  , setIcaccsr
  , icaccsrSelector
  , initWithResponseValue_errorSelector
  , setIcaccsrSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRJointFabricAdministratorClusterICACCSRResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRJointFabricAdministratorClusterICACCSRResponseParams mtrJointFabricAdministratorClusterICACCSRResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrJointFabricAdministratorClusterICACCSRResponseParams -> responseValue -> error_ -> IO (Id MTRJointFabricAdministratorClusterICACCSRResponseParams)
initWithResponseValue_error mtrJointFabricAdministratorClusterICACCSRResponseParams responseValue error_ =
  sendOwnedMessage mtrJointFabricAdministratorClusterICACCSRResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- icaccsr@
icaccsr :: IsMTRJointFabricAdministratorClusterICACCSRResponseParams mtrJointFabricAdministratorClusterICACCSRResponseParams => mtrJointFabricAdministratorClusterICACCSRResponseParams -> IO (Id NSData)
icaccsr mtrJointFabricAdministratorClusterICACCSRResponseParams =
  sendMessage mtrJointFabricAdministratorClusterICACCSRResponseParams icaccsrSelector

-- | @- setIcaccsr:@
setIcaccsr :: (IsMTRJointFabricAdministratorClusterICACCSRResponseParams mtrJointFabricAdministratorClusterICACCSRResponseParams, IsNSData value) => mtrJointFabricAdministratorClusterICACCSRResponseParams -> value -> IO ()
setIcaccsr mtrJointFabricAdministratorClusterICACCSRResponseParams value =
  sendMessage mtrJointFabricAdministratorClusterICACCSRResponseParams setIcaccsrSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRJointFabricAdministratorClusterICACCSRResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @icaccsr@
icaccsrSelector :: Selector '[] (Id NSData)
icaccsrSelector = mkSelector "icaccsr"

-- | @Selector@ for @setIcaccsr:@
setIcaccsrSelector :: Selector '[Id NSData] ()
setIcaccsrSelector = mkSelector "setIcaccsr:"

