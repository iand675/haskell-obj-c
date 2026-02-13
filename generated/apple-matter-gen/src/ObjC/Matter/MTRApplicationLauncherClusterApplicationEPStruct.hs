{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRApplicationLauncherClusterApplicationEPStruct@.
module ObjC.Matter.MTRApplicationLauncherClusterApplicationEPStruct
  ( MTRApplicationLauncherClusterApplicationEPStruct
  , IsMTRApplicationLauncherClusterApplicationEPStruct(..)
  , application
  , setApplication
  , endpoint
  , setEndpoint
  , applicationSelector
  , endpointSelector
  , setApplicationSelector
  , setEndpointSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- application@
application :: IsMTRApplicationLauncherClusterApplicationEPStruct mtrApplicationLauncherClusterApplicationEPStruct => mtrApplicationLauncherClusterApplicationEPStruct -> IO (Id MTRApplicationLauncherClusterApplicationStruct)
application mtrApplicationLauncherClusterApplicationEPStruct =
  sendMessage mtrApplicationLauncherClusterApplicationEPStruct applicationSelector

-- | @- setApplication:@
setApplication :: (IsMTRApplicationLauncherClusterApplicationEPStruct mtrApplicationLauncherClusterApplicationEPStruct, IsMTRApplicationLauncherClusterApplicationStruct value) => mtrApplicationLauncherClusterApplicationEPStruct -> value -> IO ()
setApplication mtrApplicationLauncherClusterApplicationEPStruct value =
  sendMessage mtrApplicationLauncherClusterApplicationEPStruct setApplicationSelector (toMTRApplicationLauncherClusterApplicationStruct value)

-- | @- endpoint@
endpoint :: IsMTRApplicationLauncherClusterApplicationEPStruct mtrApplicationLauncherClusterApplicationEPStruct => mtrApplicationLauncherClusterApplicationEPStruct -> IO (Id NSNumber)
endpoint mtrApplicationLauncherClusterApplicationEPStruct =
  sendMessage mtrApplicationLauncherClusterApplicationEPStruct endpointSelector

-- | @- setEndpoint:@
setEndpoint :: (IsMTRApplicationLauncherClusterApplicationEPStruct mtrApplicationLauncherClusterApplicationEPStruct, IsNSNumber value) => mtrApplicationLauncherClusterApplicationEPStruct -> value -> IO ()
setEndpoint mtrApplicationLauncherClusterApplicationEPStruct value =
  sendMessage mtrApplicationLauncherClusterApplicationEPStruct setEndpointSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @application@
applicationSelector :: Selector '[] (Id MTRApplicationLauncherClusterApplicationStruct)
applicationSelector = mkSelector "application"

-- | @Selector@ for @setApplication:@
setApplicationSelector :: Selector '[Id MTRApplicationLauncherClusterApplicationStruct] ()
setApplicationSelector = mkSelector "setApplication:"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector '[] (Id NSNumber)
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @setEndpoint:@
setEndpointSelector :: Selector '[Id NSNumber] ()
setEndpointSelector = mkSelector "setEndpoint:"

