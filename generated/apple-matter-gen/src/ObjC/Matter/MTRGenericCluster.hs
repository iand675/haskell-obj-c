{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base for all MTRCluster* types.
--
-- Generated bindings for @MTRGenericCluster@.
module ObjC.Matter.MTRGenericCluster
  ( MTRGenericCluster
  , IsMTRGenericCluster(..)
  , device
  , deviceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The device this cluster object is associated with.
--
-- ObjC selector: @- device@
device :: IsMTRGenericCluster mtrGenericCluster => mtrGenericCluster -> IO (Id MTRDevice)
device mtrGenericCluster =
  sendMessage mtrGenericCluster deviceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @device@
deviceSelector :: Selector '[] (Id MTRDevice)
deviceSelector = mkSelector "device"

