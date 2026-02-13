{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHFetchResult@.
module ObjC.Photos.PHFetchResult
  ( PHFetchResult
  , IsPHFetchResult(..)
  , objectAtIndex
  , objectAtIndexedSubscript
  , containsObject
  , indexOfObject
  , indexOfObject_inRange
  , objectsAtIndexes
  , enumerateObjectsUsingBlock
  , enumerateObjectsWithOptions_usingBlock
  , enumerateObjectsAtIndexes_options_usingBlock
  , countOfAssetsWithMediaType
  , count
  , firstObject
  , lastObject
  , containsObjectSelector
  , countOfAssetsWithMediaTypeSelector
  , countSelector
  , enumerateObjectsAtIndexes_options_usingBlockSelector
  , enumerateObjectsUsingBlockSelector
  , enumerateObjectsWithOptions_usingBlockSelector
  , firstObjectSelector
  , indexOfObjectSelector
  , indexOfObject_inRangeSelector
  , lastObjectSelector
  , objectAtIndexSelector
  , objectAtIndexedSubscriptSelector
  , objectsAtIndexesSelector

  -- * Enum types
  , NSEnumerationOptions(NSEnumerationOptions)
  , pattern NSEnumerationConcurrent
  , pattern NSEnumerationReverse
  , PHAssetMediaType(PHAssetMediaType)
  , pattern PHAssetMediaTypeUnknown
  , pattern PHAssetMediaTypeImage
  , pattern PHAssetMediaTypeVideo
  , pattern PHAssetMediaTypeAudio

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Photos.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- objectAtIndex:@
objectAtIndex :: IsPHFetchResult phFetchResult => phFetchResult -> CULong -> IO RawId
objectAtIndex phFetchResult index =
  sendMessage phFetchResult objectAtIndexSelector index

-- | @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsPHFetchResult phFetchResult => phFetchResult -> CULong -> IO RawId
objectAtIndexedSubscript phFetchResult idx =
  sendMessage phFetchResult objectAtIndexedSubscriptSelector idx

-- | @- containsObject:@
containsObject :: IsPHFetchResult phFetchResult => phFetchResult -> RawId -> IO Bool
containsObject phFetchResult anObject =
  sendMessage phFetchResult containsObjectSelector anObject

-- | @- indexOfObject:@
indexOfObject :: IsPHFetchResult phFetchResult => phFetchResult -> RawId -> IO CULong
indexOfObject phFetchResult anObject =
  sendMessage phFetchResult indexOfObjectSelector anObject

-- | @- indexOfObject:inRange:@
indexOfObject_inRange :: IsPHFetchResult phFetchResult => phFetchResult -> RawId -> NSRange -> IO CULong
indexOfObject_inRange phFetchResult anObject range =
  sendMessage phFetchResult indexOfObject_inRangeSelector anObject range

-- | @- objectsAtIndexes:@
objectsAtIndexes :: (IsPHFetchResult phFetchResult, IsNSIndexSet indexes) => phFetchResult -> indexes -> IO (Id NSArray)
objectsAtIndexes phFetchResult indexes =
  sendMessage phFetchResult objectsAtIndexesSelector (toNSIndexSet indexes)

-- | @- enumerateObjectsUsingBlock:@
enumerateObjectsUsingBlock :: IsPHFetchResult phFetchResult => phFetchResult -> Ptr () -> IO ()
enumerateObjectsUsingBlock phFetchResult block =
  sendMessage phFetchResult enumerateObjectsUsingBlockSelector block

-- | @- enumerateObjectsWithOptions:usingBlock:@
enumerateObjectsWithOptions_usingBlock :: IsPHFetchResult phFetchResult => phFetchResult -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateObjectsWithOptions_usingBlock phFetchResult opts block =
  sendMessage phFetchResult enumerateObjectsWithOptions_usingBlockSelector opts block

-- | @- enumerateObjectsAtIndexes:options:usingBlock:@
enumerateObjectsAtIndexes_options_usingBlock :: (IsPHFetchResult phFetchResult, IsNSIndexSet s) => phFetchResult -> s -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateObjectsAtIndexes_options_usingBlock phFetchResult s opts block =
  sendMessage phFetchResult enumerateObjectsAtIndexes_options_usingBlockSelector (toNSIndexSet s) opts block

-- | @- countOfAssetsWithMediaType:@
countOfAssetsWithMediaType :: IsPHFetchResult phFetchResult => phFetchResult -> PHAssetMediaType -> IO CULong
countOfAssetsWithMediaType phFetchResult mediaType =
  sendMessage phFetchResult countOfAssetsWithMediaTypeSelector mediaType

-- | @- count@
count :: IsPHFetchResult phFetchResult => phFetchResult -> IO CULong
count phFetchResult =
  sendMessage phFetchResult countSelector

-- | @- firstObject@
firstObject :: IsPHFetchResult phFetchResult => phFetchResult -> IO RawId
firstObject phFetchResult =
  sendMessage phFetchResult firstObjectSelector

-- | @- lastObject@
lastObject :: IsPHFetchResult phFetchResult => phFetchResult -> IO RawId
lastObject phFetchResult =
  sendMessage phFetchResult lastObjectSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndex:@
objectAtIndexSelector :: Selector '[CULong] RawId
objectAtIndexSelector = mkSelector "objectAtIndex:"

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] RawId
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @containsObject:@
containsObjectSelector :: Selector '[RawId] Bool
containsObjectSelector = mkSelector "containsObject:"

-- | @Selector@ for @indexOfObject:@
indexOfObjectSelector :: Selector '[RawId] CULong
indexOfObjectSelector = mkSelector "indexOfObject:"

-- | @Selector@ for @indexOfObject:inRange:@
indexOfObject_inRangeSelector :: Selector '[RawId, NSRange] CULong
indexOfObject_inRangeSelector = mkSelector "indexOfObject:inRange:"

-- | @Selector@ for @objectsAtIndexes:@
objectsAtIndexesSelector :: Selector '[Id NSIndexSet] (Id NSArray)
objectsAtIndexesSelector = mkSelector "objectsAtIndexes:"

-- | @Selector@ for @enumerateObjectsUsingBlock:@
enumerateObjectsUsingBlockSelector :: Selector '[Ptr ()] ()
enumerateObjectsUsingBlockSelector = mkSelector "enumerateObjectsUsingBlock:"

-- | @Selector@ for @enumerateObjectsWithOptions:usingBlock:@
enumerateObjectsWithOptions_usingBlockSelector :: Selector '[NSEnumerationOptions, Ptr ()] ()
enumerateObjectsWithOptions_usingBlockSelector = mkSelector "enumerateObjectsWithOptions:usingBlock:"

-- | @Selector@ for @enumerateObjectsAtIndexes:options:usingBlock:@
enumerateObjectsAtIndexes_options_usingBlockSelector :: Selector '[Id NSIndexSet, NSEnumerationOptions, Ptr ()] ()
enumerateObjectsAtIndexes_options_usingBlockSelector = mkSelector "enumerateObjectsAtIndexes:options:usingBlock:"

-- | @Selector@ for @countOfAssetsWithMediaType:@
countOfAssetsWithMediaTypeSelector :: Selector '[PHAssetMediaType] CULong
countOfAssetsWithMediaTypeSelector = mkSelector "countOfAssetsWithMediaType:"

-- | @Selector@ for @count@
countSelector :: Selector '[] CULong
countSelector = mkSelector "count"

-- | @Selector@ for @firstObject@
firstObjectSelector :: Selector '[] RawId
firstObjectSelector = mkSelector "firstObject"

-- | @Selector@ for @lastObject@
lastObjectSelector :: Selector '[] RawId
lastObjectSelector = mkSelector "lastObject"

