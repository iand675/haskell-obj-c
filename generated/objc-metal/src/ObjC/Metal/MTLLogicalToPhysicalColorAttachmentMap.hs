{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Allows you to easily specify color attachment remapping from logical to physical indices.
--
-- Generated bindings for @MTLLogicalToPhysicalColorAttachmentMap@.
module ObjC.Metal.MTLLogicalToPhysicalColorAttachmentMap
  ( MTLLogicalToPhysicalColorAttachmentMap
  , IsMTLLogicalToPhysicalColorAttachmentMap(..)
  , setPhysicalIndex_forLogicalIndex
  , getPhysicalIndexForLogicalIndex
  , reset
  , setPhysicalIndex_forLogicalIndexSelector
  , getPhysicalIndexForLogicalIndexSelector
  , resetSelector


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

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Maps a physical color attachment index to a logical index.
--
-- - Parameters:   - physicalIndex: index of the color attachment's physical mapping.   - logicalIndex: index of the color attachment's logical mapping.
--
-- ObjC selector: @- setPhysicalIndex:forLogicalIndex:@
setPhysicalIndex_forLogicalIndex :: IsMTLLogicalToPhysicalColorAttachmentMap mtlLogicalToPhysicalColorAttachmentMap => mtlLogicalToPhysicalColorAttachmentMap -> CULong -> CULong -> IO ()
setPhysicalIndex_forLogicalIndex mtlLogicalToPhysicalColorAttachmentMap  physicalIndex logicalIndex =
  sendMsg mtlLogicalToPhysicalColorAttachmentMap (mkSelector "setPhysicalIndex:forLogicalIndex:") retVoid [argCULong (fromIntegral physicalIndex), argCULong (fromIntegral logicalIndex)]

-- | Queries the physical color attachment index corresponding to a logical index.
--
-- ObjC selector: @- getPhysicalIndexForLogicalIndex:@
getPhysicalIndexForLogicalIndex :: IsMTLLogicalToPhysicalColorAttachmentMap mtlLogicalToPhysicalColorAttachmentMap => mtlLogicalToPhysicalColorAttachmentMap -> CULong -> IO CULong
getPhysicalIndexForLogicalIndex mtlLogicalToPhysicalColorAttachmentMap  logicalIndex =
  sendMsg mtlLogicalToPhysicalColorAttachmentMap (mkSelector "getPhysicalIndexForLogicalIndex:") retCULong [argCULong (fromIntegral logicalIndex)]

-- | @- reset@
reset :: IsMTLLogicalToPhysicalColorAttachmentMap mtlLogicalToPhysicalColorAttachmentMap => mtlLogicalToPhysicalColorAttachmentMap -> IO ()
reset mtlLogicalToPhysicalColorAttachmentMap  =
  sendMsg mtlLogicalToPhysicalColorAttachmentMap (mkSelector "reset") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setPhysicalIndex:forLogicalIndex:@
setPhysicalIndex_forLogicalIndexSelector :: Selector
setPhysicalIndex_forLogicalIndexSelector = mkSelector "setPhysicalIndex:forLogicalIndex:"

-- | @Selector@ for @getPhysicalIndexForLogicalIndex:@
getPhysicalIndexForLogicalIndexSelector :: Selector
getPhysicalIndexForLogicalIndexSelector = mkSelector "getPhysicalIndexForLogicalIndex:"

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

