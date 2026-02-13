{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSIndexPath@.
module ObjC.Foundation.NSIndexPath
  ( NSIndexPath
  , IsNSIndexPath(..)
  , indexPathWithIndex
  , indexPathWithIndexes_length
  , initWithIndexes_length
  , initWithIndex
  , indexPathByAddingIndex
  , indexPathByRemovingLastIndex
  , indexAtPosition
  , getIndexes_range
  , compare_
  , getIndexes
  , length_
  , compareSelector
  , getIndexesSelector
  , getIndexes_rangeSelector
  , indexAtPositionSelector
  , indexPathByAddingIndexSelector
  , indexPathByRemovingLastIndexSelector
  , indexPathWithIndexSelector
  , indexPathWithIndexes_lengthSelector
  , initWithIndexSelector
  , initWithIndexes_lengthSelector
  , lengthSelector

  -- * Enum types
  , NSComparisonResult(NSComparisonResult)
  , pattern NSOrderedAscending
  , pattern NSOrderedSame
  , pattern NSOrderedDescending

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Enums

-- | @+ indexPathWithIndex:@
indexPathWithIndex :: CULong -> IO (Id NSIndexPath)
indexPathWithIndex index =
  do
    cls' <- getRequiredClass "NSIndexPath"
    sendClassMessage cls' indexPathWithIndexSelector index

-- | @+ indexPathWithIndexes:length:@
indexPathWithIndexes_length :: Const (Ptr CULong) -> CULong -> IO (Id NSIndexPath)
indexPathWithIndexes_length indexes length_ =
  do
    cls' <- getRequiredClass "NSIndexPath"
    sendClassMessage cls' indexPathWithIndexes_lengthSelector indexes length_

-- | @- initWithIndexes:length:@
initWithIndexes_length :: IsNSIndexPath nsIndexPath => nsIndexPath -> Const (Ptr CULong) -> CULong -> IO (Id NSIndexPath)
initWithIndexes_length nsIndexPath indexes length_ =
  sendOwnedMessage nsIndexPath initWithIndexes_lengthSelector indexes length_

-- | @- initWithIndex:@
initWithIndex :: IsNSIndexPath nsIndexPath => nsIndexPath -> CULong -> IO (Id NSIndexPath)
initWithIndex nsIndexPath index =
  sendOwnedMessage nsIndexPath initWithIndexSelector index

-- | @- indexPathByAddingIndex:@
indexPathByAddingIndex :: IsNSIndexPath nsIndexPath => nsIndexPath -> CULong -> IO (Id NSIndexPath)
indexPathByAddingIndex nsIndexPath index =
  sendMessage nsIndexPath indexPathByAddingIndexSelector index

-- | @- indexPathByRemovingLastIndex@
indexPathByRemovingLastIndex :: IsNSIndexPath nsIndexPath => nsIndexPath -> IO (Id NSIndexPath)
indexPathByRemovingLastIndex nsIndexPath =
  sendMessage nsIndexPath indexPathByRemovingLastIndexSelector

-- | @- indexAtPosition:@
indexAtPosition :: IsNSIndexPath nsIndexPath => nsIndexPath -> CULong -> IO CULong
indexAtPosition nsIndexPath position =
  sendMessage nsIndexPath indexAtPositionSelector position

-- | Copies the indexes stored in this index path from the positions specified by positionRange into indexes.
--
-- @indexes@ — Buffer of at least as many NSUIntegers as specified by the length of positionRange. On return, this memory will hold the index path's indexes.
--
-- @positionRange@ — A range of valid positions within this index path.  If the location plus the length of positionRange is greater than the length of this index path, this method raises an NSRangeException.
--
-- It is the developer’s responsibility to allocate the memory for the C array.
--
-- ObjC selector: @- getIndexes:range:@
getIndexes_range :: IsNSIndexPath nsIndexPath => nsIndexPath -> Ptr CULong -> NSRange -> IO ()
getIndexes_range nsIndexPath indexes positionRange =
  sendMessage nsIndexPath getIndexes_rangeSelector indexes positionRange

-- | @- compare:@
compare_ :: (IsNSIndexPath nsIndexPath, IsNSIndexPath otherObject) => nsIndexPath -> otherObject -> IO NSComparisonResult
compare_ nsIndexPath otherObject =
  sendMessage nsIndexPath compareSelector (toNSIndexPath otherObject)

-- | This method is unsafe because it could potentially cause buffer overruns. You should use -getIndexes:range: instead.
--
-- ObjC selector: @- getIndexes:@
getIndexes :: IsNSIndexPath nsIndexPath => nsIndexPath -> Ptr CULong -> IO ()
getIndexes nsIndexPath indexes =
  sendMessage nsIndexPath getIndexesSelector indexes

-- | @- length@
length_ :: IsNSIndexPath nsIndexPath => nsIndexPath -> IO CULong
length_ nsIndexPath =
  sendMessage nsIndexPath lengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @indexPathWithIndex:@
indexPathWithIndexSelector :: Selector '[CULong] (Id NSIndexPath)
indexPathWithIndexSelector = mkSelector "indexPathWithIndex:"

-- | @Selector@ for @indexPathWithIndexes:length:@
indexPathWithIndexes_lengthSelector :: Selector '[Const (Ptr CULong), CULong] (Id NSIndexPath)
indexPathWithIndexes_lengthSelector = mkSelector "indexPathWithIndexes:length:"

-- | @Selector@ for @initWithIndexes:length:@
initWithIndexes_lengthSelector :: Selector '[Const (Ptr CULong), CULong] (Id NSIndexPath)
initWithIndexes_lengthSelector = mkSelector "initWithIndexes:length:"

-- | @Selector@ for @initWithIndex:@
initWithIndexSelector :: Selector '[CULong] (Id NSIndexPath)
initWithIndexSelector = mkSelector "initWithIndex:"

-- | @Selector@ for @indexPathByAddingIndex:@
indexPathByAddingIndexSelector :: Selector '[CULong] (Id NSIndexPath)
indexPathByAddingIndexSelector = mkSelector "indexPathByAddingIndex:"

-- | @Selector@ for @indexPathByRemovingLastIndex@
indexPathByRemovingLastIndexSelector :: Selector '[] (Id NSIndexPath)
indexPathByRemovingLastIndexSelector = mkSelector "indexPathByRemovingLastIndex"

-- | @Selector@ for @indexAtPosition:@
indexAtPositionSelector :: Selector '[CULong] CULong
indexAtPositionSelector = mkSelector "indexAtPosition:"

-- | @Selector@ for @getIndexes:range:@
getIndexes_rangeSelector :: Selector '[Ptr CULong, NSRange] ()
getIndexes_rangeSelector = mkSelector "getIndexes:range:"

-- | @Selector@ for @compare:@
compareSelector :: Selector '[Id NSIndexPath] NSComparisonResult
compareSelector = mkSelector "compare:"

-- | @Selector@ for @getIndexes:@
getIndexesSelector :: Selector '[Ptr CULong] ()
getIndexesSelector = mkSelector "getIndexes:"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CULong
lengthSelector = mkSelector "length"

