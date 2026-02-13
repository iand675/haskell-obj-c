{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMergePolicy@.
module ObjC.CoreData.NSMergePolicy
  ( NSMergePolicy
  , IsNSMergePolicy(..)
  , initWithMergeType
  , init_
  , resolveConflicts_error
  , resolveOptimisticLockingVersionConflicts_error
  , resolveConstraintConflicts_error
  , errorMergePolicy
  , rollbackMergePolicy
  , overwriteMergePolicy
  , mergeByPropertyObjectTrumpMergePolicy
  , mergeByPropertyStoreTrumpMergePolicy
  , mergeType
  , errorMergePolicySelector
  , initSelector
  , initWithMergeTypeSelector
  , mergeByPropertyObjectTrumpMergePolicySelector
  , mergeByPropertyStoreTrumpMergePolicySelector
  , mergeTypeSelector
  , overwriteMergePolicySelector
  , resolveConflicts_errorSelector
  , resolveConstraintConflicts_errorSelector
  , resolveOptimisticLockingVersionConflicts_errorSelector
  , rollbackMergePolicySelector

  -- * Enum types
  , NSMergePolicyType(NSMergePolicyType)
  , pattern NSErrorMergePolicyType
  , pattern NSMergeByPropertyStoreTrumpMergePolicyType
  , pattern NSMergeByPropertyObjectTrumpMergePolicyType
  , pattern NSOverwriteMergePolicyType
  , pattern NSRollbackMergePolicyType

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.CoreData.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithMergeType:@
initWithMergeType :: IsNSMergePolicy nsMergePolicy => nsMergePolicy -> NSMergePolicyType -> IO RawId
initWithMergeType nsMergePolicy ty =
  sendOwnedMessage nsMergePolicy initWithMergeTypeSelector ty

-- | @- init@
init_ :: IsNSMergePolicy nsMergePolicy => nsMergePolicy -> IO (Id NSMergePolicy)
init_ nsMergePolicy =
  sendOwnedMessage nsMergePolicy initSelector

-- | @- resolveConflicts:error:@
resolveConflicts_error :: (IsNSMergePolicy nsMergePolicy, IsNSArray list, IsNSError error_) => nsMergePolicy -> list -> error_ -> IO Bool
resolveConflicts_error nsMergePolicy list error_ =
  sendMessage nsMergePolicy resolveConflicts_errorSelector (toNSArray list) (toNSError error_)

-- | @- resolveOptimisticLockingVersionConflicts:error:@
resolveOptimisticLockingVersionConflicts_error :: (IsNSMergePolicy nsMergePolicy, IsNSArray list, IsNSError error_) => nsMergePolicy -> list -> error_ -> IO Bool
resolveOptimisticLockingVersionConflicts_error nsMergePolicy list error_ =
  sendMessage nsMergePolicy resolveOptimisticLockingVersionConflicts_errorSelector (toNSArray list) (toNSError error_)

-- | @- resolveConstraintConflicts:error:@
resolveConstraintConflicts_error :: (IsNSMergePolicy nsMergePolicy, IsNSArray list, IsNSError error_) => nsMergePolicy -> list -> error_ -> IO Bool
resolveConstraintConflicts_error nsMergePolicy list error_ =
  sendMessage nsMergePolicy resolveConstraintConflicts_errorSelector (toNSArray list) (toNSError error_)

-- | @+ errorMergePolicy@
errorMergePolicy :: IO (Id NSMergePolicy)
errorMergePolicy  =
  do
    cls' <- getRequiredClass "NSMergePolicy"
    sendClassMessage cls' errorMergePolicySelector

-- | @+ rollbackMergePolicy@
rollbackMergePolicy :: IO (Id NSMergePolicy)
rollbackMergePolicy  =
  do
    cls' <- getRequiredClass "NSMergePolicy"
    sendClassMessage cls' rollbackMergePolicySelector

-- | @+ overwriteMergePolicy@
overwriteMergePolicy :: IO (Id NSMergePolicy)
overwriteMergePolicy  =
  do
    cls' <- getRequiredClass "NSMergePolicy"
    sendClassMessage cls' overwriteMergePolicySelector

-- | @+ mergeByPropertyObjectTrumpMergePolicy@
mergeByPropertyObjectTrumpMergePolicy :: IO (Id NSMergePolicy)
mergeByPropertyObjectTrumpMergePolicy  =
  do
    cls' <- getRequiredClass "NSMergePolicy"
    sendClassMessage cls' mergeByPropertyObjectTrumpMergePolicySelector

-- | @+ mergeByPropertyStoreTrumpMergePolicy@
mergeByPropertyStoreTrumpMergePolicy :: IO (Id NSMergePolicy)
mergeByPropertyStoreTrumpMergePolicy  =
  do
    cls' <- getRequiredClass "NSMergePolicy"
    sendClassMessage cls' mergeByPropertyStoreTrumpMergePolicySelector

-- | @- mergeType@
mergeType :: IsNSMergePolicy nsMergePolicy => nsMergePolicy -> IO NSMergePolicyType
mergeType nsMergePolicy =
  sendMessage nsMergePolicy mergeTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMergeType:@
initWithMergeTypeSelector :: Selector '[NSMergePolicyType] RawId
initWithMergeTypeSelector = mkSelector "initWithMergeType:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSMergePolicy)
initSelector = mkSelector "init"

-- | @Selector@ for @resolveConflicts:error:@
resolveConflicts_errorSelector :: Selector '[Id NSArray, Id NSError] Bool
resolveConflicts_errorSelector = mkSelector "resolveConflicts:error:"

-- | @Selector@ for @resolveOptimisticLockingVersionConflicts:error:@
resolveOptimisticLockingVersionConflicts_errorSelector :: Selector '[Id NSArray, Id NSError] Bool
resolveOptimisticLockingVersionConflicts_errorSelector = mkSelector "resolveOptimisticLockingVersionConflicts:error:"

-- | @Selector@ for @resolveConstraintConflicts:error:@
resolveConstraintConflicts_errorSelector :: Selector '[Id NSArray, Id NSError] Bool
resolveConstraintConflicts_errorSelector = mkSelector "resolveConstraintConflicts:error:"

-- | @Selector@ for @errorMergePolicy@
errorMergePolicySelector :: Selector '[] (Id NSMergePolicy)
errorMergePolicySelector = mkSelector "errorMergePolicy"

-- | @Selector@ for @rollbackMergePolicy@
rollbackMergePolicySelector :: Selector '[] (Id NSMergePolicy)
rollbackMergePolicySelector = mkSelector "rollbackMergePolicy"

-- | @Selector@ for @overwriteMergePolicy@
overwriteMergePolicySelector :: Selector '[] (Id NSMergePolicy)
overwriteMergePolicySelector = mkSelector "overwriteMergePolicy"

-- | @Selector@ for @mergeByPropertyObjectTrumpMergePolicy@
mergeByPropertyObjectTrumpMergePolicySelector :: Selector '[] (Id NSMergePolicy)
mergeByPropertyObjectTrumpMergePolicySelector = mkSelector "mergeByPropertyObjectTrumpMergePolicy"

-- | @Selector@ for @mergeByPropertyStoreTrumpMergePolicy@
mergeByPropertyStoreTrumpMergePolicySelector :: Selector '[] (Id NSMergePolicy)
mergeByPropertyStoreTrumpMergePolicySelector = mkSelector "mergeByPropertyStoreTrumpMergePolicy"

-- | @Selector@ for @mergeType@
mergeTypeSelector :: Selector '[] NSMergePolicyType
mergeTypeSelector = mkSelector "mergeType"

