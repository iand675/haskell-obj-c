{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The anticipated compute devices that would be used for executing a layer/operation.
--
-- Generated bindings for @MLComputePlanDeviceUsage@.
module ObjC.CoreML.MLComputePlanDeviceUsage
  ( MLComputePlanDeviceUsage
  , IsMLComputePlanDeviceUsage(..)
  , init_
  , new
  , initSelector
  , newSelector


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

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMLComputePlanDeviceUsage mlComputePlanDeviceUsage => mlComputePlanDeviceUsage -> IO (Id MLComputePlanDeviceUsage)
init_ mlComputePlanDeviceUsage  =
  sendMsg mlComputePlanDeviceUsage (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MLComputePlanDeviceUsage)
new  =
  do
    cls' <- getRequiredClass "MLComputePlanDeviceUsage"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

