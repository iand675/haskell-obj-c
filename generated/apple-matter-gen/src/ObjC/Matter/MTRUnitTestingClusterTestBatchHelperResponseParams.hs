{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestBatchHelperResponseParams@.
module ObjC.Matter.MTRUnitTestingClusterTestBatchHelperResponseParams
  ( MTRUnitTestingClusterTestBatchHelperResponseParams
  , IsMTRUnitTestingClusterTestBatchHelperResponseParams(..)
  , initWithResponseValue_error
  , buffer
  , setBuffer
  , bufferSelector
  , initWithResponseValue_errorSelector
  , setBufferSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRUnitTestingClusterTestBatchHelperResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRUnitTestingClusterTestBatchHelperResponseParams mtrUnitTestingClusterTestBatchHelperResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrUnitTestingClusterTestBatchHelperResponseParams -> responseValue -> error_ -> IO (Id MTRUnitTestingClusterTestBatchHelperResponseParams)
initWithResponseValue_error mtrUnitTestingClusterTestBatchHelperResponseParams responseValue error_ =
  sendOwnedMessage mtrUnitTestingClusterTestBatchHelperResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- buffer@
buffer :: IsMTRUnitTestingClusterTestBatchHelperResponseParams mtrUnitTestingClusterTestBatchHelperResponseParams => mtrUnitTestingClusterTestBatchHelperResponseParams -> IO (Id NSData)
buffer mtrUnitTestingClusterTestBatchHelperResponseParams =
  sendMessage mtrUnitTestingClusterTestBatchHelperResponseParams bufferSelector

-- | @- setBuffer:@
setBuffer :: (IsMTRUnitTestingClusterTestBatchHelperResponseParams mtrUnitTestingClusterTestBatchHelperResponseParams, IsNSData value) => mtrUnitTestingClusterTestBatchHelperResponseParams -> value -> IO ()
setBuffer mtrUnitTestingClusterTestBatchHelperResponseParams value =
  sendMessage mtrUnitTestingClusterTestBatchHelperResponseParams setBufferSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRUnitTestingClusterTestBatchHelperResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @buffer@
bufferSelector :: Selector '[] (Id NSData)
bufferSelector = mkSelector "buffer"

-- | @Selector@ for @setBuffer:@
setBufferSelector :: Selector '[Id NSData] ()
setBufferSelector = mkSelector "setBuffer:"

