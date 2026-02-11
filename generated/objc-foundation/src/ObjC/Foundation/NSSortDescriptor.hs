{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSortDescriptor@.
module ObjC.Foundation.NSSortDescriptor
  ( NSSortDescriptor
  , IsNSSortDescriptor(..)
  , sortDescriptorWithKey_ascending
  , sortDescriptorWithKey_ascending_selector
  , initWithKey_ascending
  , initWithKey_ascending_selector
  , initWithCoder
  , allowEvaluation
  , sortDescriptorWithKey_ascending_comparator
  , initWithKey_ascending_comparator
  , compareObject_toObject
  , key
  , ascending
  , selector
  , comparator
  , reversedSortDescriptor
  , sortDescriptorWithKey_ascendingSelector
  , sortDescriptorWithKey_ascending_selectorSelector
  , initWithKey_ascendingSelector
  , initWithKey_ascending_selectorSelector
  , initWithCoderSelector
  , allowEvaluationSelector
  , sortDescriptorWithKey_ascending_comparatorSelector
  , initWithKey_ascending_comparatorSelector
  , compareObject_toObjectSelector
  , keySelector
  , ascendingSelector
  , selectorSelector
  , comparatorSelector
  , reversedSortDescriptorSelector

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
import ObjC.Foundation.Internal.Enums

-- | @+ sortDescriptorWithKey:ascending:@
sortDescriptorWithKey_ascending :: IsNSString key => key -> Bool -> IO (Id NSSortDescriptor)
sortDescriptorWithKey_ascending key ascending =
  do
    cls' <- getRequiredClass "NSSortDescriptor"
    withObjCPtr key $ \raw_key ->
      sendClassMsg cls' (mkSelector "sortDescriptorWithKey:ascending:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ()), argCULong (if ascending then 1 else 0)] >>= retainedObject . castPtr

-- | @+ sortDescriptorWithKey:ascending:selector:@
sortDescriptorWithKey_ascending_selector :: IsNSString key => key -> Bool -> Selector -> IO (Id NSSortDescriptor)
sortDescriptorWithKey_ascending_selector key ascending selector =
  do
    cls' <- getRequiredClass "NSSortDescriptor"
    withObjCPtr key $ \raw_key ->
      sendClassMsg cls' (mkSelector "sortDescriptorWithKey:ascending:selector:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ()), argCULong (if ascending then 1 else 0), argPtr (unSelector selector)] >>= retainedObject . castPtr

-- | @- initWithKey:ascending:@
initWithKey_ascending :: (IsNSSortDescriptor nsSortDescriptor, IsNSString key) => nsSortDescriptor -> key -> Bool -> IO (Id NSSortDescriptor)
initWithKey_ascending nsSortDescriptor  key ascending =
withObjCPtr key $ \raw_key ->
    sendMsg nsSortDescriptor (mkSelector "initWithKey:ascending:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ()), argCULong (if ascending then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithKey:ascending:selector:@
initWithKey_ascending_selector :: (IsNSSortDescriptor nsSortDescriptor, IsNSString key) => nsSortDescriptor -> key -> Bool -> Selector -> IO (Id NSSortDescriptor)
initWithKey_ascending_selector nsSortDescriptor  key ascending selector =
withObjCPtr key $ \raw_key ->
    sendMsg nsSortDescriptor (mkSelector "initWithKey:ascending:selector:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ()), argCULong (if ascending then 1 else 0), argPtr (unSelector selector)] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSSortDescriptor nsSortDescriptor, IsNSCoder coder) => nsSortDescriptor -> coder -> IO (Id NSSortDescriptor)
initWithCoder nsSortDescriptor  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsSortDescriptor (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- allowEvaluation@
allowEvaluation :: IsNSSortDescriptor nsSortDescriptor => nsSortDescriptor -> IO ()
allowEvaluation nsSortDescriptor  =
  sendMsg nsSortDescriptor (mkSelector "allowEvaluation") retVoid []

-- | @+ sortDescriptorWithKey:ascending:comparator:@
sortDescriptorWithKey_ascending_comparator :: IsNSString key => key -> Bool -> Ptr () -> IO (Id NSSortDescriptor)
sortDescriptorWithKey_ascending_comparator key ascending cmptr =
  do
    cls' <- getRequiredClass "NSSortDescriptor"
    withObjCPtr key $ \raw_key ->
      sendClassMsg cls' (mkSelector "sortDescriptorWithKey:ascending:comparator:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ()), argCULong (if ascending then 1 else 0), argPtr (castPtr cmptr :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithKey:ascending:comparator:@
initWithKey_ascending_comparator :: (IsNSSortDescriptor nsSortDescriptor, IsNSString key) => nsSortDescriptor -> key -> Bool -> Ptr () -> IO (Id NSSortDescriptor)
initWithKey_ascending_comparator nsSortDescriptor  key ascending cmptr =
withObjCPtr key $ \raw_key ->
    sendMsg nsSortDescriptor (mkSelector "initWithKey:ascending:comparator:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ()), argCULong (if ascending then 1 else 0), argPtr (castPtr cmptr :: Ptr ())] >>= ownedObject . castPtr

-- | @- compareObject:toObject:@
compareObject_toObject :: IsNSSortDescriptor nsSortDescriptor => nsSortDescriptor -> RawId -> RawId -> IO NSComparisonResult
compareObject_toObject nsSortDescriptor  object1 object2 =
  fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg nsSortDescriptor (mkSelector "compareObject:toObject:") retCLong [argPtr (castPtr (unRawId object1) :: Ptr ()), argPtr (castPtr (unRawId object2) :: Ptr ())]

-- | @- key@
key :: IsNSSortDescriptor nsSortDescriptor => nsSortDescriptor -> IO (Id NSString)
key nsSortDescriptor  =
  sendMsg nsSortDescriptor (mkSelector "key") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- ascending@
ascending :: IsNSSortDescriptor nsSortDescriptor => nsSortDescriptor -> IO Bool
ascending nsSortDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSortDescriptor (mkSelector "ascending") retCULong []

-- | @- selector@
selector :: IsNSSortDescriptor nsSortDescriptor => nsSortDescriptor -> IO Selector
selector nsSortDescriptor  =
  fmap (Selector . castPtr) $ sendMsg nsSortDescriptor (mkSelector "selector") (retPtr retVoid) []

-- | @- comparator@
comparator :: IsNSSortDescriptor nsSortDescriptor => nsSortDescriptor -> IO (Ptr ())
comparator nsSortDescriptor  =
  fmap castPtr $ sendMsg nsSortDescriptor (mkSelector "comparator") (retPtr retVoid) []

-- | @- reversedSortDescriptor@
reversedSortDescriptor :: IsNSSortDescriptor nsSortDescriptor => nsSortDescriptor -> IO RawId
reversedSortDescriptor nsSortDescriptor  =
  fmap (RawId . castPtr) $ sendMsg nsSortDescriptor (mkSelector "reversedSortDescriptor") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sortDescriptorWithKey:ascending:@
sortDescriptorWithKey_ascendingSelector :: Selector
sortDescriptorWithKey_ascendingSelector = mkSelector "sortDescriptorWithKey:ascending:"

-- | @Selector@ for @sortDescriptorWithKey:ascending:selector:@
sortDescriptorWithKey_ascending_selectorSelector :: Selector
sortDescriptorWithKey_ascending_selectorSelector = mkSelector "sortDescriptorWithKey:ascending:selector:"

-- | @Selector@ for @initWithKey:ascending:@
initWithKey_ascendingSelector :: Selector
initWithKey_ascendingSelector = mkSelector "initWithKey:ascending:"

-- | @Selector@ for @initWithKey:ascending:selector:@
initWithKey_ascending_selectorSelector :: Selector
initWithKey_ascending_selectorSelector = mkSelector "initWithKey:ascending:selector:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @allowEvaluation@
allowEvaluationSelector :: Selector
allowEvaluationSelector = mkSelector "allowEvaluation"

-- | @Selector@ for @sortDescriptorWithKey:ascending:comparator:@
sortDescriptorWithKey_ascending_comparatorSelector :: Selector
sortDescriptorWithKey_ascending_comparatorSelector = mkSelector "sortDescriptorWithKey:ascending:comparator:"

-- | @Selector@ for @initWithKey:ascending:comparator:@
initWithKey_ascending_comparatorSelector :: Selector
initWithKey_ascending_comparatorSelector = mkSelector "initWithKey:ascending:comparator:"

-- | @Selector@ for @compareObject:toObject:@
compareObject_toObjectSelector :: Selector
compareObject_toObjectSelector = mkSelector "compareObject:toObject:"

-- | @Selector@ for @key@
keySelector :: Selector
keySelector = mkSelector "key"

-- | @Selector@ for @ascending@
ascendingSelector :: Selector
ascendingSelector = mkSelector "ascending"

-- | @Selector@ for @selector@
selectorSelector :: Selector
selectorSelector = mkSelector "selector"

-- | @Selector@ for @comparator@
comparatorSelector :: Selector
comparatorSelector = mkSelector "comparator"

-- | @Selector@ for @reversedSortDescriptor@
reversedSortDescriptorSelector :: Selector
reversedSortDescriptorSelector = mkSelector "reversedSortDescriptor"

