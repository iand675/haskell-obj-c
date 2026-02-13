{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestDifferentVendorMeiResponseParams@.
module ObjC.Matter.MTRUnitTestingClusterTestDifferentVendorMeiResponseParams
  ( MTRUnitTestingClusterTestDifferentVendorMeiResponseParams
  , IsMTRUnitTestingClusterTestDifferentVendorMeiResponseParams(..)
  , initWithResponseValue_error
  , arg1
  , setArg1
  , eventNumber
  , setEventNumber
  , arg1Selector
  , eventNumberSelector
  , initWithResponseValue_errorSelector
  , setArg1Selector
  , setEventNumberSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRUnitTestingClusterTestDifferentVendorMeiResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRUnitTestingClusterTestDifferentVendorMeiResponseParams mtrUnitTestingClusterTestDifferentVendorMeiResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrUnitTestingClusterTestDifferentVendorMeiResponseParams -> responseValue -> error_ -> IO (Id MTRUnitTestingClusterTestDifferentVendorMeiResponseParams)
initWithResponseValue_error mtrUnitTestingClusterTestDifferentVendorMeiResponseParams responseValue error_ =
  sendOwnedMessage mtrUnitTestingClusterTestDifferentVendorMeiResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- arg1@
arg1 :: IsMTRUnitTestingClusterTestDifferentVendorMeiResponseParams mtrUnitTestingClusterTestDifferentVendorMeiResponseParams => mtrUnitTestingClusterTestDifferentVendorMeiResponseParams -> IO (Id NSNumber)
arg1 mtrUnitTestingClusterTestDifferentVendorMeiResponseParams =
  sendMessage mtrUnitTestingClusterTestDifferentVendorMeiResponseParams arg1Selector

-- | @- setArg1:@
setArg1 :: (IsMTRUnitTestingClusterTestDifferentVendorMeiResponseParams mtrUnitTestingClusterTestDifferentVendorMeiResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestDifferentVendorMeiResponseParams -> value -> IO ()
setArg1 mtrUnitTestingClusterTestDifferentVendorMeiResponseParams value =
  sendMessage mtrUnitTestingClusterTestDifferentVendorMeiResponseParams setArg1Selector (toNSNumber value)

-- | @- eventNumber@
eventNumber :: IsMTRUnitTestingClusterTestDifferentVendorMeiResponseParams mtrUnitTestingClusterTestDifferentVendorMeiResponseParams => mtrUnitTestingClusterTestDifferentVendorMeiResponseParams -> IO (Id NSNumber)
eventNumber mtrUnitTestingClusterTestDifferentVendorMeiResponseParams =
  sendMessage mtrUnitTestingClusterTestDifferentVendorMeiResponseParams eventNumberSelector

-- | @- setEventNumber:@
setEventNumber :: (IsMTRUnitTestingClusterTestDifferentVendorMeiResponseParams mtrUnitTestingClusterTestDifferentVendorMeiResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestDifferentVendorMeiResponseParams -> value -> IO ()
setEventNumber mtrUnitTestingClusterTestDifferentVendorMeiResponseParams value =
  sendMessage mtrUnitTestingClusterTestDifferentVendorMeiResponseParams setEventNumberSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRUnitTestingClusterTestDifferentVendorMeiResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @arg1@
arg1Selector :: Selector '[] (Id NSNumber)
arg1Selector = mkSelector "arg1"

-- | @Selector@ for @setArg1:@
setArg1Selector :: Selector '[Id NSNumber] ()
setArg1Selector = mkSelector "setArg1:"

-- | @Selector@ for @eventNumber@
eventNumberSelector :: Selector '[] (Id NSNumber)
eventNumberSelector = mkSelector "eventNumber"

-- | @Selector@ for @setEventNumber:@
setEventNumberSelector :: Selector '[Id NSNumber] ()
setEventNumberSelector = mkSelector "setEventNumber:"

