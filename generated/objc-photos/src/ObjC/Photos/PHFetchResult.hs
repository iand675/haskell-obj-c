{-# LANGUAGE PatternSynonyms #-}
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
  , objectAtIndexSelector
  , objectAtIndexedSubscriptSelector
  , containsObjectSelector
  , indexOfObjectSelector
  , indexOfObject_inRangeSelector
  , objectsAtIndexesSelector
  , enumerateObjectsUsingBlockSelector
  , enumerateObjectsWithOptions_usingBlockSelector
  , enumerateObjectsAtIndexes_options_usingBlockSelector
  , countOfAssetsWithMediaTypeSelector
  , countSelector
  , firstObjectSelector
  , lastObjectSelector

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

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Photos.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- objectAtIndex:@
objectAtIndex :: IsPHFetchResult phFetchResult => phFetchResult -> CULong -> IO RawId
objectAtIndex phFetchResult  index =
  fmap (RawId . castPtr) $ sendMsg phFetchResult (mkSelector "objectAtIndex:") (retPtr retVoid) [argCULong (fromIntegral index)]

-- | @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsPHFetchResult phFetchResult => phFetchResult -> CULong -> IO RawId
objectAtIndexedSubscript phFetchResult  idx =
  fmap (RawId . castPtr) $ sendMsg phFetchResult (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCULong (fromIntegral idx)]

-- | @- containsObject:@
containsObject :: IsPHFetchResult phFetchResult => phFetchResult -> RawId -> IO Bool
containsObject phFetchResult  anObject =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phFetchResult (mkSelector "containsObject:") retCULong [argPtr (castPtr (unRawId anObject) :: Ptr ())]

-- | @- indexOfObject:@
indexOfObject :: IsPHFetchResult phFetchResult => phFetchResult -> RawId -> IO CULong
indexOfObject phFetchResult  anObject =
  sendMsg phFetchResult (mkSelector "indexOfObject:") retCULong [argPtr (castPtr (unRawId anObject) :: Ptr ())]

-- | @- indexOfObject:inRange:@
indexOfObject_inRange :: IsPHFetchResult phFetchResult => phFetchResult -> RawId -> NSRange -> IO CULong
indexOfObject_inRange phFetchResult  anObject range =
  sendMsg phFetchResult (mkSelector "indexOfObject:inRange:") retCULong [argPtr (castPtr (unRawId anObject) :: Ptr ()), argNSRange range]

-- | @- objectsAtIndexes:@
objectsAtIndexes :: (IsPHFetchResult phFetchResult, IsNSIndexSet indexes) => phFetchResult -> indexes -> IO (Id NSArray)
objectsAtIndexes phFetchResult  indexes =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg phFetchResult (mkSelector "objectsAtIndexes:") (retPtr retVoid) [argPtr (castPtr raw_indexes :: Ptr ())] >>= retainedObject . castPtr

-- | @- enumerateObjectsUsingBlock:@
enumerateObjectsUsingBlock :: IsPHFetchResult phFetchResult => phFetchResult -> Ptr () -> IO ()
enumerateObjectsUsingBlock phFetchResult  block =
  sendMsg phFetchResult (mkSelector "enumerateObjectsUsingBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @- enumerateObjectsWithOptions:usingBlock:@
enumerateObjectsWithOptions_usingBlock :: IsPHFetchResult phFetchResult => phFetchResult -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateObjectsWithOptions_usingBlock phFetchResult  opts block =
  sendMsg phFetchResult (mkSelector "enumerateObjectsWithOptions:usingBlock:") retVoid [argCULong (coerce opts), argPtr (castPtr block :: Ptr ())]

-- | @- enumerateObjectsAtIndexes:options:usingBlock:@
enumerateObjectsAtIndexes_options_usingBlock :: (IsPHFetchResult phFetchResult, IsNSIndexSet s) => phFetchResult -> s -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateObjectsAtIndexes_options_usingBlock phFetchResult  s opts block =
withObjCPtr s $ \raw_s ->
    sendMsg phFetchResult (mkSelector "enumerateObjectsAtIndexes:options:usingBlock:") retVoid [argPtr (castPtr raw_s :: Ptr ()), argCULong (coerce opts), argPtr (castPtr block :: Ptr ())]

-- | @- countOfAssetsWithMediaType:@
countOfAssetsWithMediaType :: IsPHFetchResult phFetchResult => phFetchResult -> PHAssetMediaType -> IO CULong
countOfAssetsWithMediaType phFetchResult  mediaType =
  sendMsg phFetchResult (mkSelector "countOfAssetsWithMediaType:") retCULong [argCLong (coerce mediaType)]

-- | @- count@
count :: IsPHFetchResult phFetchResult => phFetchResult -> IO CULong
count phFetchResult  =
  sendMsg phFetchResult (mkSelector "count") retCULong []

-- | @- firstObject@
firstObject :: IsPHFetchResult phFetchResult => phFetchResult -> IO RawId
firstObject phFetchResult  =
  fmap (RawId . castPtr) $ sendMsg phFetchResult (mkSelector "firstObject") (retPtr retVoid) []

-- | @- lastObject@
lastObject :: IsPHFetchResult phFetchResult => phFetchResult -> IO RawId
lastObject phFetchResult  =
  fmap (RawId . castPtr) $ sendMsg phFetchResult (mkSelector "lastObject") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectAtIndex:@
objectAtIndexSelector :: Selector
objectAtIndexSelector = mkSelector "objectAtIndex:"

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @containsObject:@
containsObjectSelector :: Selector
containsObjectSelector = mkSelector "containsObject:"

-- | @Selector@ for @indexOfObject:@
indexOfObjectSelector :: Selector
indexOfObjectSelector = mkSelector "indexOfObject:"

-- | @Selector@ for @indexOfObject:inRange:@
indexOfObject_inRangeSelector :: Selector
indexOfObject_inRangeSelector = mkSelector "indexOfObject:inRange:"

-- | @Selector@ for @objectsAtIndexes:@
objectsAtIndexesSelector :: Selector
objectsAtIndexesSelector = mkSelector "objectsAtIndexes:"

-- | @Selector@ for @enumerateObjectsUsingBlock:@
enumerateObjectsUsingBlockSelector :: Selector
enumerateObjectsUsingBlockSelector = mkSelector "enumerateObjectsUsingBlock:"

-- | @Selector@ for @enumerateObjectsWithOptions:usingBlock:@
enumerateObjectsWithOptions_usingBlockSelector :: Selector
enumerateObjectsWithOptions_usingBlockSelector = mkSelector "enumerateObjectsWithOptions:usingBlock:"

-- | @Selector@ for @enumerateObjectsAtIndexes:options:usingBlock:@
enumerateObjectsAtIndexes_options_usingBlockSelector :: Selector
enumerateObjectsAtIndexes_options_usingBlockSelector = mkSelector "enumerateObjectsAtIndexes:options:usingBlock:"

-- | @Selector@ for @countOfAssetsWithMediaType:@
countOfAssetsWithMediaTypeSelector :: Selector
countOfAssetsWithMediaTypeSelector = mkSelector "countOfAssetsWithMediaType:"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

-- | @Selector@ for @firstObject@
firstObjectSelector :: Selector
firstObjectSelector = mkSelector "firstObject"

-- | @Selector@ for @lastObject@
lastObjectSelector :: Selector
lastObjectSelector = mkSelector "lastObject"

