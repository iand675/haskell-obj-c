{-# LANGUAGE DataKinds #-}
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
  , getPhysicalIndexForLogicalIndexSelector
  , resetSelector
  , setPhysicalIndex_forLogicalIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
setPhysicalIndex_forLogicalIndex mtlLogicalToPhysicalColorAttachmentMap physicalIndex logicalIndex =
  sendMessage mtlLogicalToPhysicalColorAttachmentMap setPhysicalIndex_forLogicalIndexSelector physicalIndex logicalIndex

-- | Queries the physical color attachment index corresponding to a logical index.
--
-- ObjC selector: @- getPhysicalIndexForLogicalIndex:@
getPhysicalIndexForLogicalIndex :: IsMTLLogicalToPhysicalColorAttachmentMap mtlLogicalToPhysicalColorAttachmentMap => mtlLogicalToPhysicalColorAttachmentMap -> CULong -> IO CULong
getPhysicalIndexForLogicalIndex mtlLogicalToPhysicalColorAttachmentMap logicalIndex =
  sendMessage mtlLogicalToPhysicalColorAttachmentMap getPhysicalIndexForLogicalIndexSelector logicalIndex

-- | @- reset@
reset :: IsMTLLogicalToPhysicalColorAttachmentMap mtlLogicalToPhysicalColorAttachmentMap => mtlLogicalToPhysicalColorAttachmentMap -> IO ()
reset mtlLogicalToPhysicalColorAttachmentMap =
  sendMessage mtlLogicalToPhysicalColorAttachmentMap resetSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setPhysicalIndex:forLogicalIndex:@
setPhysicalIndex_forLogicalIndexSelector :: Selector '[CULong, CULong] ()
setPhysicalIndex_forLogicalIndexSelector = mkSelector "setPhysicalIndex:forLogicalIndex:"

-- | @Selector@ for @getPhysicalIndexForLogicalIndex:@
getPhysicalIndexForLogicalIndexSelector :: Selector '[CULong] CULong
getPhysicalIndexForLogicalIndexSelector = mkSelector "getPhysicalIndexForLogicalIndex:"

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

