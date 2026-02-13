{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricAdministratorClusterTransferAnchorResponseParams@.
module ObjC.Matter.MTRJointFabricAdministratorClusterTransferAnchorResponseParams
  ( MTRJointFabricAdministratorClusterTransferAnchorResponseParams
  , IsMTRJointFabricAdministratorClusterTransferAnchorResponseParams(..)
  , initWithResponseValue_error
  , statusCode
  , setStatusCode
  , initWithResponseValue_errorSelector
  , setStatusCodeSelector
  , statusCodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRJointFabricAdministratorClusterTransferAnchorResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRJointFabricAdministratorClusterTransferAnchorResponseParams mtrJointFabricAdministratorClusterTransferAnchorResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrJointFabricAdministratorClusterTransferAnchorResponseParams -> responseValue -> error_ -> IO (Id MTRJointFabricAdministratorClusterTransferAnchorResponseParams)
initWithResponseValue_error mtrJointFabricAdministratorClusterTransferAnchorResponseParams responseValue error_ =
  sendOwnedMessage mtrJointFabricAdministratorClusterTransferAnchorResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- statusCode@
statusCode :: IsMTRJointFabricAdministratorClusterTransferAnchorResponseParams mtrJointFabricAdministratorClusterTransferAnchorResponseParams => mtrJointFabricAdministratorClusterTransferAnchorResponseParams -> IO (Id NSNumber)
statusCode mtrJointFabricAdministratorClusterTransferAnchorResponseParams =
  sendMessage mtrJointFabricAdministratorClusterTransferAnchorResponseParams statusCodeSelector

-- | @- setStatusCode:@
setStatusCode :: (IsMTRJointFabricAdministratorClusterTransferAnchorResponseParams mtrJointFabricAdministratorClusterTransferAnchorResponseParams, IsNSNumber value) => mtrJointFabricAdministratorClusterTransferAnchorResponseParams -> value -> IO ()
setStatusCode mtrJointFabricAdministratorClusterTransferAnchorResponseParams value =
  sendMessage mtrJointFabricAdministratorClusterTransferAnchorResponseParams setStatusCodeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRJointFabricAdministratorClusterTransferAnchorResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @statusCode@
statusCodeSelector :: Selector '[] (Id NSNumber)
statusCodeSelector = mkSelector "statusCode"

-- | @Selector@ for @setStatusCode:@
setStatusCodeSelector :: Selector '[Id NSNumber] ()
setStatusCodeSelector = mkSelector "setStatusCode:"

