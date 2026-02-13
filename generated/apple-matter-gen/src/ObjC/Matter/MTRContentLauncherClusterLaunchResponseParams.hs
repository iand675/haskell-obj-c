{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterLaunchResponseParams@.
module ObjC.Matter.MTRContentLauncherClusterLaunchResponseParams
  ( MTRContentLauncherClusterLaunchResponseParams
  , IsMTRContentLauncherClusterLaunchResponseParams(..)
  , status
  , setStatus
  , data_
  , setData
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , dataSelector
  , setDataSelector
  , setStatusSelector
  , setTimedInvokeTimeoutMsSelector
  , statusSelector
  , timedInvokeTimeoutMsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- status@
status :: IsMTRContentLauncherClusterLaunchResponseParams mtrContentLauncherClusterLaunchResponseParams => mtrContentLauncherClusterLaunchResponseParams -> IO (Id NSNumber)
status mtrContentLauncherClusterLaunchResponseParams =
  sendMessage mtrContentLauncherClusterLaunchResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRContentLauncherClusterLaunchResponseParams mtrContentLauncherClusterLaunchResponseParams, IsNSNumber value) => mtrContentLauncherClusterLaunchResponseParams -> value -> IO ()
setStatus mtrContentLauncherClusterLaunchResponseParams value =
  sendMessage mtrContentLauncherClusterLaunchResponseParams setStatusSelector (toNSNumber value)

-- | @- data@
data_ :: IsMTRContentLauncherClusterLaunchResponseParams mtrContentLauncherClusterLaunchResponseParams => mtrContentLauncherClusterLaunchResponseParams -> IO (Id NSString)
data_ mtrContentLauncherClusterLaunchResponseParams =
  sendMessage mtrContentLauncherClusterLaunchResponseParams dataSelector

-- | @- setData:@
setData :: (IsMTRContentLauncherClusterLaunchResponseParams mtrContentLauncherClusterLaunchResponseParams, IsNSString value) => mtrContentLauncherClusterLaunchResponseParams -> value -> IO ()
setData mtrContentLauncherClusterLaunchResponseParams value =
  sendMessage mtrContentLauncherClusterLaunchResponseParams setDataSelector (toNSString value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRContentLauncherClusterLaunchResponseParams mtrContentLauncherClusterLaunchResponseParams => mtrContentLauncherClusterLaunchResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrContentLauncherClusterLaunchResponseParams =
  sendMessage mtrContentLauncherClusterLaunchResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRContentLauncherClusterLaunchResponseParams mtrContentLauncherClusterLaunchResponseParams, IsNSNumber value) => mtrContentLauncherClusterLaunchResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrContentLauncherClusterLaunchResponseParams value =
  sendMessage mtrContentLauncherClusterLaunchResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

