{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for Metal 4 acceleration structure descriptors.
--
-- Don't use this class directly. Use one of its subclasses instead.
--
-- Generated bindings for @MTL4AccelerationStructureDescriptor@.
module ObjC.Metal.MTL4AccelerationStructureDescriptor
  ( MTL4AccelerationStructureDescriptor
  , IsMTL4AccelerationStructureDescriptor(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

