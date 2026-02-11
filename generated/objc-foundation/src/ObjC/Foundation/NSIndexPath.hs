{-# LANGUAGE PatternSynonyms #-}
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
  , indexPathWithIndexSelector
  , indexPathWithIndexes_lengthSelector
  , initWithIndexes_lengthSelector
  , initWithIndexSelector
  , indexPathByAddingIndexSelector
  , indexPathByRemovingLastIndexSelector
  , indexAtPositionSelector
  , getIndexes_rangeSelector
  , compareSelector
  , getIndexesSelector
  , lengthSelector

  -- * Enum types
  , NSComparisonResult(NSComparisonResult)
  , pattern NSOrderedAscending
  , pattern NSOrderedSame
  , pattern NSOrderedDescending

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
import ObjC.Foundation.Internal.Enums

-- | @+ indexPathWithIndex:@
indexPathWithIndex :: CULong -> IO (Id NSIndexPath)
indexPathWithIndex index =
  do
    cls' <- getRequiredClass "NSIndexPath"
    sendClassMsg cls' (mkSelector "indexPathWithIndex:") (retPtr retVoid) [argCULong (fromIntegral index)] >>= retainedObject . castPtr

-- | @+ indexPathWithIndexes:length:@
indexPathWithIndexes_length :: Const (Ptr CULong) -> CULong -> IO (Id NSIndexPath)
indexPathWithIndexes_length indexes length_ =
  do
    cls' <- getRequiredClass "NSIndexPath"
    sendClassMsg cls' (mkSelector "indexPathWithIndexes:length:") (retPtr retVoid) [argPtr (unConst indexes), argCULong (fromIntegral length_)] >>= retainedObject . castPtr

-- | @- initWithIndexes:length:@
initWithIndexes_length :: IsNSIndexPath nsIndexPath => nsIndexPath -> Const (Ptr CULong) -> CULong -> IO (Id NSIndexPath)
initWithIndexes_length nsIndexPath  indexes length_ =
  sendMsg nsIndexPath (mkSelector "initWithIndexes:length:") (retPtr retVoid) [argPtr (unConst indexes), argCULong (fromIntegral length_)] >>= ownedObject . castPtr

-- | @- initWithIndex:@
initWithIndex :: IsNSIndexPath nsIndexPath => nsIndexPath -> CULong -> IO (Id NSIndexPath)
initWithIndex nsIndexPath  index =
  sendMsg nsIndexPath (mkSelector "initWithIndex:") (retPtr retVoid) [argCULong (fromIntegral index)] >>= ownedObject . castPtr

-- | @- indexPathByAddingIndex:@
indexPathByAddingIndex :: IsNSIndexPath nsIndexPath => nsIndexPath -> CULong -> IO (Id NSIndexPath)
indexPathByAddingIndex nsIndexPath  index =
  sendMsg nsIndexPath (mkSelector "indexPathByAddingIndex:") (retPtr retVoid) [argCULong (fromIntegral index)] >>= retainedObject . castPtr

-- | @- indexPathByRemovingLastIndex@
indexPathByRemovingLastIndex :: IsNSIndexPath nsIndexPath => nsIndexPath -> IO (Id NSIndexPath)
indexPathByRemovingLastIndex nsIndexPath  =
  sendMsg nsIndexPath (mkSelector "indexPathByRemovingLastIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- indexAtPosition:@
indexAtPosition :: IsNSIndexPath nsIndexPath => nsIndexPath -> CULong -> IO CULong
indexAtPosition nsIndexPath  position =
  sendMsg nsIndexPath (mkSelector "indexAtPosition:") retCULong [argCULong (fromIntegral position)]

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
getIndexes_range nsIndexPath  indexes positionRange =
  sendMsg nsIndexPath (mkSelector "getIndexes:range:") retVoid [argPtr indexes, argNSRange positionRange]

-- | @- compare:@
compare_ :: (IsNSIndexPath nsIndexPath, IsNSIndexPath otherObject) => nsIndexPath -> otherObject -> IO NSComparisonResult
compare_ nsIndexPath  otherObject =
withObjCPtr otherObject $ \raw_otherObject ->
    fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg nsIndexPath (mkSelector "compare:") retCLong [argPtr (castPtr raw_otherObject :: Ptr ())]

-- | This method is unsafe because it could potentially cause buffer overruns. You should use -getIndexes:range: instead.
--
-- ObjC selector: @- getIndexes:@
getIndexes :: IsNSIndexPath nsIndexPath => nsIndexPath -> Ptr CULong -> IO ()
getIndexes nsIndexPath  indexes =
  sendMsg nsIndexPath (mkSelector "getIndexes:") retVoid [argPtr indexes]

-- | @- length@
length_ :: IsNSIndexPath nsIndexPath => nsIndexPath -> IO CULong
length_ nsIndexPath  =
  sendMsg nsIndexPath (mkSelector "length") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @indexPathWithIndex:@
indexPathWithIndexSelector :: Selector
indexPathWithIndexSelector = mkSelector "indexPathWithIndex:"

-- | @Selector@ for @indexPathWithIndexes:length:@
indexPathWithIndexes_lengthSelector :: Selector
indexPathWithIndexes_lengthSelector = mkSelector "indexPathWithIndexes:length:"

-- | @Selector@ for @initWithIndexes:length:@
initWithIndexes_lengthSelector :: Selector
initWithIndexes_lengthSelector = mkSelector "initWithIndexes:length:"

-- | @Selector@ for @initWithIndex:@
initWithIndexSelector :: Selector
initWithIndexSelector = mkSelector "initWithIndex:"

-- | @Selector@ for @indexPathByAddingIndex:@
indexPathByAddingIndexSelector :: Selector
indexPathByAddingIndexSelector = mkSelector "indexPathByAddingIndex:"

-- | @Selector@ for @indexPathByRemovingLastIndex@
indexPathByRemovingLastIndexSelector :: Selector
indexPathByRemovingLastIndexSelector = mkSelector "indexPathByRemovingLastIndex"

-- | @Selector@ for @indexAtPosition:@
indexAtPositionSelector :: Selector
indexAtPositionSelector = mkSelector "indexAtPosition:"

-- | @Selector@ for @getIndexes:range:@
getIndexes_rangeSelector :: Selector
getIndexes_rangeSelector = mkSelector "getIndexes:range:"

-- | @Selector@ for @compare:@
compareSelector :: Selector
compareSelector = mkSelector "compare:"

-- | @Selector@ for @getIndexes:@
getIndexesSelector :: Selector
getIndexesSelector = mkSelector "getIndexes:"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

