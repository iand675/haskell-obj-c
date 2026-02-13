{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentAppObserverClusterContentAppMessageResponseParams@.
module ObjC.Matter.MTRContentAppObserverClusterContentAppMessageResponseParams
  ( MTRContentAppObserverClusterContentAppMessageResponseParams
  , IsMTRContentAppObserverClusterContentAppMessageResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , data_
  , setData
  , encodingHint
  , setEncodingHint
  , dataSelector
  , encodingHintSelector
  , initWithResponseValue_errorSelector
  , setDataSelector
  , setEncodingHintSelector
  , setStatusSelector
  , statusSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRContentAppObserverClusterContentAppMessageResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRContentAppObserverClusterContentAppMessageResponseParams mtrContentAppObserverClusterContentAppMessageResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrContentAppObserverClusterContentAppMessageResponseParams -> responseValue -> error_ -> IO (Id MTRContentAppObserverClusterContentAppMessageResponseParams)
initWithResponseValue_error mtrContentAppObserverClusterContentAppMessageResponseParams responseValue error_ =
  sendOwnedMessage mtrContentAppObserverClusterContentAppMessageResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTRContentAppObserverClusterContentAppMessageResponseParams mtrContentAppObserverClusterContentAppMessageResponseParams => mtrContentAppObserverClusterContentAppMessageResponseParams -> IO (Id NSNumber)
status mtrContentAppObserverClusterContentAppMessageResponseParams =
  sendMessage mtrContentAppObserverClusterContentAppMessageResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRContentAppObserverClusterContentAppMessageResponseParams mtrContentAppObserverClusterContentAppMessageResponseParams, IsNSNumber value) => mtrContentAppObserverClusterContentAppMessageResponseParams -> value -> IO ()
setStatus mtrContentAppObserverClusterContentAppMessageResponseParams value =
  sendMessage mtrContentAppObserverClusterContentAppMessageResponseParams setStatusSelector (toNSNumber value)

-- | @- data@
data_ :: IsMTRContentAppObserverClusterContentAppMessageResponseParams mtrContentAppObserverClusterContentAppMessageResponseParams => mtrContentAppObserverClusterContentAppMessageResponseParams -> IO (Id NSString)
data_ mtrContentAppObserverClusterContentAppMessageResponseParams =
  sendMessage mtrContentAppObserverClusterContentAppMessageResponseParams dataSelector

-- | @- setData:@
setData :: (IsMTRContentAppObserverClusterContentAppMessageResponseParams mtrContentAppObserverClusterContentAppMessageResponseParams, IsNSString value) => mtrContentAppObserverClusterContentAppMessageResponseParams -> value -> IO ()
setData mtrContentAppObserverClusterContentAppMessageResponseParams value =
  sendMessage mtrContentAppObserverClusterContentAppMessageResponseParams setDataSelector (toNSString value)

-- | @- encodingHint@
encodingHint :: IsMTRContentAppObserverClusterContentAppMessageResponseParams mtrContentAppObserverClusterContentAppMessageResponseParams => mtrContentAppObserverClusterContentAppMessageResponseParams -> IO (Id NSString)
encodingHint mtrContentAppObserverClusterContentAppMessageResponseParams =
  sendMessage mtrContentAppObserverClusterContentAppMessageResponseParams encodingHintSelector

-- | @- setEncodingHint:@
setEncodingHint :: (IsMTRContentAppObserverClusterContentAppMessageResponseParams mtrContentAppObserverClusterContentAppMessageResponseParams, IsNSString value) => mtrContentAppObserverClusterContentAppMessageResponseParams -> value -> IO ()
setEncodingHint mtrContentAppObserverClusterContentAppMessageResponseParams value =
  sendMessage mtrContentAppObserverClusterContentAppMessageResponseParams setEncodingHintSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRContentAppObserverClusterContentAppMessageResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] (Id NSNumber)
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector '[Id NSNumber] ()
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSString)
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector '[Id NSString] ()
setDataSelector = mkSelector "setData:"

-- | @Selector@ for @encodingHint@
encodingHintSelector :: Selector '[] (Id NSString)
encodingHintSelector = mkSelector "encodingHint"

-- | @Selector@ for @setEncodingHint:@
setEncodingHintSelector :: Selector '[Id NSString] ()
setEncodingHintSelector = mkSelector "setEncodingHint:"

