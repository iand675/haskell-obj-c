{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentControlClusterResetPINResponseParams@.
module ObjC.Matter.MTRContentControlClusterResetPINResponseParams
  ( MTRContentControlClusterResetPINResponseParams
  , IsMTRContentControlClusterResetPINResponseParams(..)
  , initWithResponseValue_error
  , pinCode
  , setPinCode
  , initWithResponseValue_errorSelector
  , pinCodeSelector
  , setPinCodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRContentControlClusterResetPINResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRContentControlClusterResetPINResponseParams mtrContentControlClusterResetPINResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrContentControlClusterResetPINResponseParams -> responseValue -> error_ -> IO (Id MTRContentControlClusterResetPINResponseParams)
initWithResponseValue_error mtrContentControlClusterResetPINResponseParams responseValue error_ =
  sendOwnedMessage mtrContentControlClusterResetPINResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- pinCode@
pinCode :: IsMTRContentControlClusterResetPINResponseParams mtrContentControlClusterResetPINResponseParams => mtrContentControlClusterResetPINResponseParams -> IO (Id NSString)
pinCode mtrContentControlClusterResetPINResponseParams =
  sendMessage mtrContentControlClusterResetPINResponseParams pinCodeSelector

-- | @- setPinCode:@
setPinCode :: (IsMTRContentControlClusterResetPINResponseParams mtrContentControlClusterResetPINResponseParams, IsNSString value) => mtrContentControlClusterResetPINResponseParams -> value -> IO ()
setPinCode mtrContentControlClusterResetPINResponseParams value =
  sendMessage mtrContentControlClusterResetPINResponseParams setPinCodeSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRContentControlClusterResetPINResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @pinCode@
pinCodeSelector :: Selector '[] (Id NSString)
pinCodeSelector = mkSelector "pinCode"

-- | @Selector@ for @setPinCode:@
setPinCodeSelector :: Selector '[Id NSString] ()
setPinCodeSelector = mkSelector "setPinCode:"

