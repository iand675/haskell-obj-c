{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricAdministratorClusterICACResponseParams@.
module ObjC.Matter.MTRJointFabricAdministratorClusterICACResponseParams
  ( MTRJointFabricAdministratorClusterICACResponseParams
  , IsMTRJointFabricAdministratorClusterICACResponseParams(..)
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

-- | Initialize an MTRJointFabricAdministratorClusterICACResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRJointFabricAdministratorClusterICACResponseParams mtrJointFabricAdministratorClusterICACResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrJointFabricAdministratorClusterICACResponseParams -> responseValue -> error_ -> IO (Id MTRJointFabricAdministratorClusterICACResponseParams)
initWithResponseValue_error mtrJointFabricAdministratorClusterICACResponseParams responseValue error_ =
  sendOwnedMessage mtrJointFabricAdministratorClusterICACResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- statusCode@
statusCode :: IsMTRJointFabricAdministratorClusterICACResponseParams mtrJointFabricAdministratorClusterICACResponseParams => mtrJointFabricAdministratorClusterICACResponseParams -> IO (Id NSNumber)
statusCode mtrJointFabricAdministratorClusterICACResponseParams =
  sendMessage mtrJointFabricAdministratorClusterICACResponseParams statusCodeSelector

-- | @- setStatusCode:@
setStatusCode :: (IsMTRJointFabricAdministratorClusterICACResponseParams mtrJointFabricAdministratorClusterICACResponseParams, IsNSNumber value) => mtrJointFabricAdministratorClusterICACResponseParams -> value -> IO ()
setStatusCode mtrJointFabricAdministratorClusterICACResponseParams value =
  sendMessage mtrJointFabricAdministratorClusterICACResponseParams setStatusCodeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRJointFabricAdministratorClusterICACResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @statusCode@
statusCodeSelector :: Selector '[] (Id NSNumber)
statusCodeSelector = mkSelector "statusCode"

-- | @Selector@ for @setStatusCode:@
setStatusCodeSelector :: Selector '[Id NSNumber] ()
setStatusCodeSelector = mkSelector "setStatusCode:"

