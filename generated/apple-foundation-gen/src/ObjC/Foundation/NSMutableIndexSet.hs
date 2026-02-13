{-# LANGUAGE DataKinds #-}
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
  , addIndexSelector
  , addIndexesInRangeSelector
  , addIndexesSelector
  , removeAllIndexesSelector
  , removeIndexSelector
  , removeIndexesInRangeSelector
  , removeIndexesSelector
  , shiftIndexesStartingAtIndex_bySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs

-- | @- addIndexes:@
addIndexes :: (IsNSMutableIndexSet nsMutableIndexSet, IsNSIndexSet indexSet) => nsMutableIndexSet -> indexSet -> IO ()
addIndexes nsMutableIndexSet indexSet =
  sendMessage nsMutableIndexSet addIndexesSelector (toNSIndexSet indexSet)

-- | @- removeIndexes:@
removeIndexes :: (IsNSMutableIndexSet nsMutableIndexSet, IsNSIndexSet indexSet) => nsMutableIndexSet -> indexSet -> IO ()
removeIndexes nsMutableIndexSet indexSet =
  sendMessage nsMutableIndexSet removeIndexesSelector (toNSIndexSet indexSet)

-- | @- removeAllIndexes@
removeAllIndexes :: IsNSMutableIndexSet nsMutableIndexSet => nsMutableIndexSet -> IO ()
removeAllIndexes nsMutableIndexSet =
  sendMessage nsMutableIndexSet removeAllIndexesSelector

-- | @- addIndex:@
addIndex :: IsNSMutableIndexSet nsMutableIndexSet => nsMutableIndexSet -> CULong -> IO ()
addIndex nsMutableIndexSet value =
  sendMessage nsMutableIndexSet addIndexSelector value

-- | @- removeIndex:@
removeIndex :: IsNSMutableIndexSet nsMutableIndexSet => nsMutableIndexSet -> CULong -> IO ()
removeIndex nsMutableIndexSet value =
  sendMessage nsMutableIndexSet removeIndexSelector value

-- | @- addIndexesInRange:@
addIndexesInRange :: IsNSMutableIndexSet nsMutableIndexSet => nsMutableIndexSet -> NSRange -> IO ()
addIndexesInRange nsMutableIndexSet range =
  sendMessage nsMutableIndexSet addIndexesInRangeSelector range

-- | @- removeIndexesInRange:@
removeIndexesInRange :: IsNSMutableIndexSet nsMutableIndexSet => nsMutableIndexSet -> NSRange -> IO ()
removeIndexesInRange nsMutableIndexSet range =
  sendMessage nsMutableIndexSet removeIndexesInRangeSelector range

-- | @- shiftIndexesStartingAtIndex:by:@
shiftIndexesStartingAtIndex_by :: IsNSMutableIndexSet nsMutableIndexSet => nsMutableIndexSet -> CULong -> CLong -> IO ()
shiftIndexesStartingAtIndex_by nsMutableIndexSet index delta =
  sendMessage nsMutableIndexSet shiftIndexesStartingAtIndex_bySelector index delta

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addIndexes:@
addIndexesSelector :: Selector '[Id NSIndexSet] ()
addIndexesSelector = mkSelector "addIndexes:"

-- | @Selector@ for @removeIndexes:@
removeIndexesSelector :: Selector '[Id NSIndexSet] ()
removeIndexesSelector = mkSelector "removeIndexes:"

-- | @Selector@ for @removeAllIndexes@
removeAllIndexesSelector :: Selector '[] ()
removeAllIndexesSelector = mkSelector "removeAllIndexes"

-- | @Selector@ for @addIndex:@
addIndexSelector :: Selector '[CULong] ()
addIndexSelector = mkSelector "addIndex:"

-- | @Selector@ for @removeIndex:@
removeIndexSelector :: Selector '[CULong] ()
removeIndexSelector = mkSelector "removeIndex:"

-- | @Selector@ for @addIndexesInRange:@
addIndexesInRangeSelector :: Selector '[NSRange] ()
addIndexesInRangeSelector = mkSelector "addIndexesInRange:"

-- | @Selector@ for @removeIndexesInRange:@
removeIndexesInRangeSelector :: Selector '[NSRange] ()
removeIndexesInRangeSelector = mkSelector "removeIndexesInRange:"

-- | @Selector@ for @shiftIndexesStartingAtIndex:by:@
shiftIndexesStartingAtIndex_bySelector :: Selector '[CULong, CLong] ()
shiftIndexesStartingAtIndex_bySelector = mkSelector "shiftIndexesStartingAtIndex:by:"

