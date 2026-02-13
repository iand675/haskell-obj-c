{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , allowEvaluationSelector
  , ascendingSelector
  , comparatorSelector
  , compareObject_toObjectSelector
  , initWithCoderSelector
  , initWithKey_ascendingSelector
  , initWithKey_ascending_comparatorSelector
  , initWithKey_ascending_selectorSelector
  , keySelector
  , reversedSortDescriptorSelector
  , selectorSelector
  , sortDescriptorWithKey_ascendingSelector
  , sortDescriptorWithKey_ascending_comparatorSelector
  , sortDescriptorWithKey_ascending_selectorSelector

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
import ObjC.Foundation.Internal.Enums

-- | @+ sortDescriptorWithKey:ascending:@
sortDescriptorWithKey_ascending :: IsNSString key => key -> Bool -> IO (Id NSSortDescriptor)
sortDescriptorWithKey_ascending key ascending =
  do
    cls' <- getRequiredClass "NSSortDescriptor"
    sendClassMessage cls' sortDescriptorWithKey_ascendingSelector (toNSString key) ascending

-- | @+ sortDescriptorWithKey:ascending:selector:@
sortDescriptorWithKey_ascending_selector :: IsNSString key => key -> Bool -> Sel -> IO (Id NSSortDescriptor)
sortDescriptorWithKey_ascending_selector key ascending selector =
  do
    cls' <- getRequiredClass "NSSortDescriptor"
    sendClassMessage cls' sortDescriptorWithKey_ascending_selectorSelector (toNSString key) ascending selector

-- | @- initWithKey:ascending:@
initWithKey_ascending :: (IsNSSortDescriptor nsSortDescriptor, IsNSString key) => nsSortDescriptor -> key -> Bool -> IO (Id NSSortDescriptor)
initWithKey_ascending nsSortDescriptor key ascending =
  sendOwnedMessage nsSortDescriptor initWithKey_ascendingSelector (toNSString key) ascending

-- | @- initWithKey:ascending:selector:@
initWithKey_ascending_selector :: (IsNSSortDescriptor nsSortDescriptor, IsNSString key) => nsSortDescriptor -> key -> Bool -> Sel -> IO (Id NSSortDescriptor)
initWithKey_ascending_selector nsSortDescriptor key ascending selector =
  sendOwnedMessage nsSortDescriptor initWithKey_ascending_selectorSelector (toNSString key) ascending selector

-- | @- initWithCoder:@
initWithCoder :: (IsNSSortDescriptor nsSortDescriptor, IsNSCoder coder) => nsSortDescriptor -> coder -> IO (Id NSSortDescriptor)
initWithCoder nsSortDescriptor coder =
  sendOwnedMessage nsSortDescriptor initWithCoderSelector (toNSCoder coder)

-- | @- allowEvaluation@
allowEvaluation :: IsNSSortDescriptor nsSortDescriptor => nsSortDescriptor -> IO ()
allowEvaluation nsSortDescriptor =
  sendMessage nsSortDescriptor allowEvaluationSelector

-- | @+ sortDescriptorWithKey:ascending:comparator:@
sortDescriptorWithKey_ascending_comparator :: IsNSString key => key -> Bool -> Ptr () -> IO (Id NSSortDescriptor)
sortDescriptorWithKey_ascending_comparator key ascending cmptr =
  do
    cls' <- getRequiredClass "NSSortDescriptor"
    sendClassMessage cls' sortDescriptorWithKey_ascending_comparatorSelector (toNSString key) ascending cmptr

-- | @- initWithKey:ascending:comparator:@
initWithKey_ascending_comparator :: (IsNSSortDescriptor nsSortDescriptor, IsNSString key) => nsSortDescriptor -> key -> Bool -> Ptr () -> IO (Id NSSortDescriptor)
initWithKey_ascending_comparator nsSortDescriptor key ascending cmptr =
  sendOwnedMessage nsSortDescriptor initWithKey_ascending_comparatorSelector (toNSString key) ascending cmptr

-- | @- compareObject:toObject:@
compareObject_toObject :: IsNSSortDescriptor nsSortDescriptor => nsSortDescriptor -> RawId -> RawId -> IO NSComparisonResult
compareObject_toObject nsSortDescriptor object1 object2 =
  sendMessage nsSortDescriptor compareObject_toObjectSelector object1 object2

-- | @- key@
key :: IsNSSortDescriptor nsSortDescriptor => nsSortDescriptor -> IO (Id NSString)
key nsSortDescriptor =
  sendMessage nsSortDescriptor keySelector

-- | @- ascending@
ascending :: IsNSSortDescriptor nsSortDescriptor => nsSortDescriptor -> IO Bool
ascending nsSortDescriptor =
  sendMessage nsSortDescriptor ascendingSelector

-- | @- selector@
selector :: IsNSSortDescriptor nsSortDescriptor => nsSortDescriptor -> IO Sel
selector nsSortDescriptor =
  sendMessage nsSortDescriptor selectorSelector

-- | @- comparator@
comparator :: IsNSSortDescriptor nsSortDescriptor => nsSortDescriptor -> IO (Ptr ())
comparator nsSortDescriptor =
  sendMessage nsSortDescriptor comparatorSelector

-- | @- reversedSortDescriptor@
reversedSortDescriptor :: IsNSSortDescriptor nsSortDescriptor => nsSortDescriptor -> IO RawId
reversedSortDescriptor nsSortDescriptor =
  sendMessage nsSortDescriptor reversedSortDescriptorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sortDescriptorWithKey:ascending:@
sortDescriptorWithKey_ascendingSelector :: Selector '[Id NSString, Bool] (Id NSSortDescriptor)
sortDescriptorWithKey_ascendingSelector = mkSelector "sortDescriptorWithKey:ascending:"

-- | @Selector@ for @sortDescriptorWithKey:ascending:selector:@
sortDescriptorWithKey_ascending_selectorSelector :: Selector '[Id NSString, Bool, Sel] (Id NSSortDescriptor)
sortDescriptorWithKey_ascending_selectorSelector = mkSelector "sortDescriptorWithKey:ascending:selector:"

-- | @Selector@ for @initWithKey:ascending:@
initWithKey_ascendingSelector :: Selector '[Id NSString, Bool] (Id NSSortDescriptor)
initWithKey_ascendingSelector = mkSelector "initWithKey:ascending:"

-- | @Selector@ for @initWithKey:ascending:selector:@
initWithKey_ascending_selectorSelector :: Selector '[Id NSString, Bool, Sel] (Id NSSortDescriptor)
initWithKey_ascending_selectorSelector = mkSelector "initWithKey:ascending:selector:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSSortDescriptor)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @allowEvaluation@
allowEvaluationSelector :: Selector '[] ()
allowEvaluationSelector = mkSelector "allowEvaluation"

-- | @Selector@ for @sortDescriptorWithKey:ascending:comparator:@
sortDescriptorWithKey_ascending_comparatorSelector :: Selector '[Id NSString, Bool, Ptr ()] (Id NSSortDescriptor)
sortDescriptorWithKey_ascending_comparatorSelector = mkSelector "sortDescriptorWithKey:ascending:comparator:"

-- | @Selector@ for @initWithKey:ascending:comparator:@
initWithKey_ascending_comparatorSelector :: Selector '[Id NSString, Bool, Ptr ()] (Id NSSortDescriptor)
initWithKey_ascending_comparatorSelector = mkSelector "initWithKey:ascending:comparator:"

-- | @Selector@ for @compareObject:toObject:@
compareObject_toObjectSelector :: Selector '[RawId, RawId] NSComparisonResult
compareObject_toObjectSelector = mkSelector "compareObject:toObject:"

-- | @Selector@ for @key@
keySelector :: Selector '[] (Id NSString)
keySelector = mkSelector "key"

-- | @Selector@ for @ascending@
ascendingSelector :: Selector '[] Bool
ascendingSelector = mkSelector "ascending"

-- | @Selector@ for @selector@
selectorSelector :: Selector '[] Sel
selectorSelector = mkSelector "selector"

-- | @Selector@ for @comparator@
comparatorSelector :: Selector '[] (Ptr ())
comparatorSelector = mkSelector "comparator"

-- | @Selector@ for @reversedSortDescriptor@
reversedSortDescriptorSelector :: Selector '[] RawId
reversedSortDescriptorSelector = mkSelector "reversedSortDescriptor"

