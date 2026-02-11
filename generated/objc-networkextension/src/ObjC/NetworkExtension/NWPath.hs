{-# LANGUAGE PatternSynonyms #-}
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
  , isEqualToPathSelector
  , statusSelector
  , expensiveSelector
  , constrainedSelector

  -- * Enum types
  , NWPathStatus(NWPathStatus)
  , pattern NWPathStatusInvalid
  , pattern NWPathStatusSatisfied
  , pattern NWPathStatusUnsatisfied
  , pattern NWPathStatusSatisfiable

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
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
isEqualToPath nwPath  path =
withObjCPtr path $ \raw_path ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nwPath (mkSelector "isEqualToPath:") retCULong [argPtr (castPtr raw_path :: Ptr ())]

-- | status
--
-- The evaluated NWPathStatus of the NWPath.
--
-- ObjC selector: @- status@
status :: IsNWPath nwPath => nwPath -> IO NWPathStatus
status nwPath  =
  fmap (coerce :: CLong -> NWPathStatus) $ sendMsg nwPath (mkSelector "status") retCLong []

-- | expensive
--
-- Returns YES if the path is considered expensive, as when using a cellular data plan.
--
-- ObjC selector: @- expensive@
expensive :: IsNWPath nwPath => nwPath -> IO Bool
expensive nwPath  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nwPath (mkSelector "expensive") retCULong []

-- | constrained
--
-- Returns YES if the path is considered constrained, as when it is in save data mode.
--
-- ObjC selector: @- constrained@
constrained :: IsNWPath nwPath => nwPath -> IO Bool
constrained nwPath  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nwPath (mkSelector "constrained") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isEqualToPath:@
isEqualToPathSelector :: Selector
isEqualToPathSelector = mkSelector "isEqualToPath:"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @expensive@
expensiveSelector :: Selector
expensiveSelector = mkSelector "expensive"

-- | @Selector@ for @constrained@
constrainedSelector :: Selector
constrainedSelector = mkSelector "constrained"

