{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRServiceAreaClusterSelectAreasResponseParams@.
module ObjC.Matter.MTRServiceAreaClusterSelectAreasResponseParams
  ( MTRServiceAreaClusterSelectAreasResponseParams
  , IsMTRServiceAreaClusterSelectAreasResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , statusText
  , setStatusText
  , initWithResponseValue_errorSelector
  , setStatusSelector
  , setStatusTextSelector
  , statusSelector
  , statusTextSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRServiceAreaClusterSelectAreasResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRServiceAreaClusterSelectAreasResponseParams mtrServiceAreaClusterSelectAreasResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrServiceAreaClusterSelectAreasResponseParams -> responseValue -> error_ -> IO (Id MTRServiceAreaClusterSelectAreasResponseParams)
initWithResponseValue_error mtrServiceAreaClusterSelectAreasResponseParams responseValue error_ =
  sendOwnedMessage mtrServiceAreaClusterSelectAreasResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTRServiceAreaClusterSelectAreasResponseParams mtrServiceAreaClusterSelectAreasResponseParams => mtrServiceAreaClusterSelectAreasResponseParams -> IO (Id NSNumber)
status mtrServiceAreaClusterSelectAreasResponseParams =
  sendMessage mtrServiceAreaClusterSelectAreasResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRServiceAreaClusterSelectAreasResponseParams mtrServiceAreaClusterSelectAreasResponseParams, IsNSNumber value) => mtrServiceAreaClusterSelectAreasResponseParams -> value -> IO ()
setStatus mtrServiceAreaClusterSelectAreasResponseParams value =
  sendMessage mtrServiceAreaClusterSelectAreasResponseParams setStatusSelector (toNSNumber value)

-- | @- statusText@
statusText :: IsMTRServiceAreaClusterSelectAreasResponseParams mtrServiceAreaClusterSelectAreasResponseParams => mtrServiceAreaClusterSelectAreasResponseParams -> IO (Id NSString)
statusText mtrServiceAreaClusterSelectAreasResponseParams =
  sendMessage mtrServiceAreaClusterSelectAreasResponseParams statusTextSelector

-- | @- setStatusText:@
setStatusText :: (IsMTRServiceAreaClusterSelectAreasResponseParams mtrServiceAreaClusterSelectAreasResponseParams, IsNSString value) => mtrServiceAreaClusterSelectAreasResponseParams -> value -> IO ()
setStatusText mtrServiceAreaClusterSelectAreasResponseParams value =
  sendMessage mtrServiceAreaClusterSelectAreasResponseParams setStatusTextSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRServiceAreaClusterSelectAreasResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] (Id NSNumber)
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector '[Id NSNumber] ()
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @statusText@
statusTextSelector :: Selector '[] (Id NSString)
statusTextSelector = mkSelector "statusText"

-- | @Selector@ for @setStatusText:@
setStatusTextSelector :: Selector '[Id NSString] ()
setStatusTextSelector = mkSelector "setStatusText:"

