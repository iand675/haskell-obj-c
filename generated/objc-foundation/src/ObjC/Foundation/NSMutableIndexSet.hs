{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMutableIndexSet@.
module ObjC.Foundation.NSMutableIndexSet
  ( NSMutableIndexSet
  , IsNSMutableIndexSet(..)
  , addIndexes
  , removeIndexes
  , removeAllIndexes
  , addIndex
  , removeIndex
  , addIndexesInRange
  , removeIndexesInRange
  , shiftIndexesStartingAtIndex_by
  , addIndexesSelector
  , removeIndexesSelector
  , removeAllIndexesSelector
  , addIndexSelector
  , removeIndexSelector
  , addIndexesInRangeSelector
  , removeIndexesInRangeSelector
  , shiftIndexesStartingAtIndex_bySelector


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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs

-- | @- addIndexes:@
addIndexes :: (IsNSMutableIndexSet nsMutableIndexSet, IsNSIndexSet indexSet) => nsMutableIndexSet -> indexSet -> IO ()
addIndexes nsMutableIndexSet  indexSet =
withObjCPtr indexSet $ \raw_indexSet ->
    sendMsg nsMutableIndexSet (mkSelector "addIndexes:") retVoid [argPtr (castPtr raw_indexSet :: Ptr ())]

-- | @- removeIndexes:@
removeIndexes :: (IsNSMutableIndexSet nsMutableIndexSet, IsNSIndexSet indexSet) => nsMutableIndexSet -> indexSet -> IO ()
removeIndexes nsMutableIndexSet  indexSet =
withObjCPtr indexSet $ \raw_indexSet ->
    sendMsg nsMutableIndexSet (mkSelector "removeIndexes:") retVoid [argPtr (castPtr raw_indexSet :: Ptr ())]

-- | @- removeAllIndexes@
removeAllIndexes :: IsNSMutableIndexSet nsMutableIndexSet => nsMutableIndexSet -> IO ()
removeAllIndexes nsMutableIndexSet  =
  sendMsg nsMutableIndexSet (mkSelector "removeAllIndexes") retVoid []

-- | @- addIndex:@
addIndex :: IsNSMutableIndexSet nsMutableIndexSet => nsMutableIndexSet -> CULong -> IO ()
addIndex nsMutableIndexSet  value =
  sendMsg nsMutableIndexSet (mkSelector "addIndex:") retVoid [argCULong (fromIntegral value)]

-- | @- removeIndex:@
removeIndex :: IsNSMutableIndexSet nsMutableIndexSet => nsMutableIndexSet -> CULong -> IO ()
removeIndex nsMutableIndexSet  value =
  sendMsg nsMutableIndexSet (mkSelector "removeIndex:") retVoid [argCULong (fromIntegral value)]

-- | @- addIndexesInRange:@
addIndexesInRange :: IsNSMutableIndexSet nsMutableIndexSet => nsMutableIndexSet -> NSRange -> IO ()
addIndexesInRange nsMutableIndexSet  range =
  sendMsg nsMutableIndexSet (mkSelector "addIndexesInRange:") retVoid [argNSRange range]

-- | @- removeIndexesInRange:@
removeIndexesInRange :: IsNSMutableIndexSet nsMutableIndexSet => nsMutableIndexSet -> NSRange -> IO ()
removeIndexesInRange nsMutableIndexSet  range =
  sendMsg nsMutableIndexSet (mkSelector "removeIndexesInRange:") retVoid [argNSRange range]

-- | @- shiftIndexesStartingAtIndex:by:@
shiftIndexesStartingAtIndex_by :: IsNSMutableIndexSet nsMutableIndexSet => nsMutableIndexSet -> CULong -> CLong -> IO ()
shiftIndexesStartingAtIndex_by nsMutableIndexSet  index delta =
  sendMsg nsMutableIndexSet (mkSelector "shiftIndexesStartingAtIndex:by:") retVoid [argCULong (fromIntegral index), argCLong (fromIntegral delta)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addIndexes:@
addIndexesSelector :: Selector
addIndexesSelector = mkSelector "addIndexes:"

-- | @Selector@ for @removeIndexes:@
removeIndexesSelector :: Selector
removeIndexesSelector = mkSelector "removeIndexes:"

-- | @Selector@ for @removeAllIndexes@
removeAllIndexesSelector :: Selector
removeAllIndexesSelector = mkSelector "removeAllIndexes"

-- | @Selector@ for @addIndex:@
addIndexSelector :: Selector
addIndexSelector = mkSelector "addIndex:"

-- | @Selector@ for @removeIndex:@
removeIndexSelector :: Selector
removeIndexSelector = mkSelector "removeIndex:"

-- | @Selector@ for @addIndexesInRange:@
addIndexesInRangeSelector :: Selector
addIndexesInRangeSelector = mkSelector "addIndexesInRange:"

-- | @Selector@ for @removeIndexesInRange:@
removeIndexesInRangeSelector :: Selector
removeIndexesInRangeSelector = mkSelector "removeIndexesInRange:"

-- | @Selector@ for @shiftIndexesStartingAtIndex:by:@
shiftIndexesStartingAtIndex_bySelector :: Selector
shiftIndexesStartingAtIndex_bySelector = mkSelector "shiftIndexesStartingAtIndex:by:"

