{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @EAAccessory@.
module ObjC.ExternalAccessory.EAAccessory
  ( EAAccessory
  , IsEAAccessory(..)
  , connected
  , connectionID
  , dockType
  , connectedSelector
  , connectionIDSelector
  , dockTypeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ExternalAccessory.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- connected@
connected :: IsEAAccessory eaAccessory => eaAccessory -> IO Bool
connected eaAccessory =
  sendMessage eaAccessory connectedSelector

-- | @- connectionID@
connectionID :: IsEAAccessory eaAccessory => eaAccessory -> IO CULong
connectionID eaAccessory =
  sendMessage eaAccessory connectionIDSelector

-- | @- dockType@
dockType :: IsEAAccessory eaAccessory => eaAccessory -> IO (Id NSString)
dockType eaAccessory =
  sendMessage eaAccessory dockTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @connected@
connectedSelector :: Selector '[] Bool
connectedSelector = mkSelector "connected"

-- | @Selector@ for @connectionID@
connectionIDSelector :: Selector '[] CULong
connectionIDSelector = mkSelector "connectionID"

-- | @Selector@ for @dockType@
dockTypeSelector :: Selector '[] (Id NSString)
dockTypeSelector = mkSelector "dockType"

