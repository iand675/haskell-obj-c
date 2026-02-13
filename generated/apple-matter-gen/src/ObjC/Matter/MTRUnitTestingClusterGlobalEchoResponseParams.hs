{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterGlobalEchoResponseParams@.
module ObjC.Matter.MTRUnitTestingClusterGlobalEchoResponseParams
  ( MTRUnitTestingClusterGlobalEchoResponseParams
  , IsMTRUnitTestingClusterGlobalEchoResponseParams(..)
  , initWithResponseValue_error
  , field1
  , setField1
  , field2
  , setField2
  , field1Selector
  , field2Selector
  , initWithResponseValue_errorSelector
  , setField1Selector
  , setField2Selector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRUnitTestingClusterGlobalEchoResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRUnitTestingClusterGlobalEchoResponseParams mtrUnitTestingClusterGlobalEchoResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrUnitTestingClusterGlobalEchoResponseParams -> responseValue -> error_ -> IO (Id MTRUnitTestingClusterGlobalEchoResponseParams)
initWithResponseValue_error mtrUnitTestingClusterGlobalEchoResponseParams responseValue error_ =
  sendOwnedMessage mtrUnitTestingClusterGlobalEchoResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- field1@
field1 :: IsMTRUnitTestingClusterGlobalEchoResponseParams mtrUnitTestingClusterGlobalEchoResponseParams => mtrUnitTestingClusterGlobalEchoResponseParams -> IO (Id MTRDataTypeTestGlobalStruct)
field1 mtrUnitTestingClusterGlobalEchoResponseParams =
  sendMessage mtrUnitTestingClusterGlobalEchoResponseParams field1Selector

-- | @- setField1:@
setField1 :: (IsMTRUnitTestingClusterGlobalEchoResponseParams mtrUnitTestingClusterGlobalEchoResponseParams, IsMTRDataTypeTestGlobalStruct value) => mtrUnitTestingClusterGlobalEchoResponseParams -> value -> IO ()
setField1 mtrUnitTestingClusterGlobalEchoResponseParams value =
  sendMessage mtrUnitTestingClusterGlobalEchoResponseParams setField1Selector (toMTRDataTypeTestGlobalStruct value)

-- | @- field2@
field2 :: IsMTRUnitTestingClusterGlobalEchoResponseParams mtrUnitTestingClusterGlobalEchoResponseParams => mtrUnitTestingClusterGlobalEchoResponseParams -> IO (Id NSNumber)
field2 mtrUnitTestingClusterGlobalEchoResponseParams =
  sendMessage mtrUnitTestingClusterGlobalEchoResponseParams field2Selector

-- | @- setField2:@
setField2 :: (IsMTRUnitTestingClusterGlobalEchoResponseParams mtrUnitTestingClusterGlobalEchoResponseParams, IsNSNumber value) => mtrUnitTestingClusterGlobalEchoResponseParams -> value -> IO ()
setField2 mtrUnitTestingClusterGlobalEchoResponseParams value =
  sendMessage mtrUnitTestingClusterGlobalEchoResponseParams setField2Selector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRUnitTestingClusterGlobalEchoResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @field1@
field1Selector :: Selector '[] (Id MTRDataTypeTestGlobalStruct)
field1Selector = mkSelector "field1"

-- | @Selector@ for @setField1:@
setField1Selector :: Selector '[Id MTRDataTypeTestGlobalStruct] ()
setField1Selector = mkSelector "setField1:"

-- | @Selector@ for @field2@
field2Selector :: Selector '[] (Id NSNumber)
field2Selector = mkSelector "field2"

-- | @Selector@ for @setField2:@
setField2Selector :: Selector '[Id NSNumber] ()
setField2Selector = mkSelector "setField2:"

