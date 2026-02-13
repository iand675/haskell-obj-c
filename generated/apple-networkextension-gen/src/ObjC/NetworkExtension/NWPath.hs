{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NWPath
--
-- A network path, represented with NWPath, expresses the viability status and		properties of the path that a networking connection will take on the device. For example,		if the path status is NWPathStatusSatisfied, then a connection could use that path.
--
-- Generated bindings for @NWPath@.
module ObjC.NetworkExtension.NWPath
  ( NWPath
  , IsNWPath(..)
  , isEqualToPath
  , status
  , expensive
  , constrained
  , constrainedSelector
  , expensiveSelector
  , isEqualToPathSelector
  , statusSelector

  -- * Enum types
  , NWPathStatus(NWPathStatus)
  , pattern NWPathStatusInvalid
  , pattern NWPathStatusSatisfied
  , pattern NWPathStatusUnsatisfied
  , pattern NWPathStatusSatisfiable

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.NetworkExtension.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | isEqualToPath:
--
-- @path@ — An NWPath object to compare.
--
-- Returns: YES if the two path objects have the same content, NO otherwise.
--
-- ObjC selector: @- isEqualToPath:@
isEqualToPath :: (IsNWPath nwPath, IsNWPath path) => nwPath -> path -> IO Bool
isEqualToPath nwPath path =
  sendMessage nwPath isEqualToPathSelector (toNWPath path)

-- | status
--
-- The evaluated NWPathStatus of the NWPath.
--
-- ObjC selector: @- status@
status :: IsNWPath nwPath => nwPath -> IO NWPathStatus
status nwPath =
  sendMessage nwPath statusSelector

-- | expensive
--
-- Returns YES if the path is considered expensive, as when using a cellular data plan.
--
-- ObjC selector: @- expensive@
expensive :: IsNWPath nwPath => nwPath -> IO Bool
expensive nwPath =
  sendMessage nwPath expensiveSelector

-- | constrained
--
-- Returns YES if the path is considered constrained, as when it is in save data mode.
--
-- ObjC selector: @- constrained@
constrained :: IsNWPath nwPath => nwPath -> IO Bool
constrained nwPath =
  sendMessage nwPath constrainedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isEqualToPath:@
isEqualToPathSelector :: Selector '[Id NWPath] Bool
isEqualToPathSelector = mkSelector "isEqualToPath:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] NWPathStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @expensive@
expensiveSelector :: Selector '[] Bool
expensiveSelector = mkSelector "expensive"

-- | @Selector@ for @constrained@
constrainedSelector :: Selector '[] Bool
constrainedSelector = mkSelector "constrained"

