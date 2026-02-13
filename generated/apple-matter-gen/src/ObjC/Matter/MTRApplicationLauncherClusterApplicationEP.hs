{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRApplicationLauncherClusterApplicationEP@.
module ObjC.Matter.MTRApplicationLauncherClusterApplicationEP
  ( MTRApplicationLauncherClusterApplicationEP
  , IsMTRApplicationLauncherClusterApplicationEP(..)
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
application :: IsMTRApplicationLauncherClusterApplicationEP mtrApplicationLauncherClusterApplicationEP => mtrApplicationLauncherClusterApplicationEP -> IO (Id MTRApplicationLauncherClusterApplicationStruct)
application mtrApplicationLauncherClusterApplicationEP =
  sendMessage mtrApplicationLauncherClusterApplicationEP applicationSelector

-- | @- setApplication:@
setApplication :: (IsMTRApplicationLauncherClusterApplicationEP mtrApplicationLauncherClusterApplicationEP, IsMTRApplicationLauncherClusterApplicationStruct value) => mtrApplicationLauncherClusterApplicationEP -> value -> IO ()
setApplication mtrApplicationLauncherClusterApplicationEP value =
  sendMessage mtrApplicationLauncherClusterApplicationEP setApplicationSelector (toMTRApplicationLauncherClusterApplicationStruct value)

-- | @- endpoint@
endpoint :: IsMTRApplicationLauncherClusterApplicationEP mtrApplicationLauncherClusterApplicationEP => mtrApplicationLauncherClusterApplicationEP -> IO (Id NSNumber)
endpoint mtrApplicationLauncherClusterApplicationEP =
  sendMessage mtrApplicationLauncherClusterApplicationEP endpointSelector

-- | @- setEndpoint:@
setEndpoint :: (IsMTRApplicationLauncherClusterApplicationEP mtrApplicationLauncherClusterApplicationEP, IsNSNumber value) => mtrApplicationLauncherClusterApplicationEP -> value -> IO ()
setEndpoint mtrApplicationLauncherClusterApplicationEP value =
  sendMessage mtrApplicationLauncherClusterApplicationEP setEndpointSelector (toNSNumber value)

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

