{-# LANGUAGE PatternSynonyms #-}
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
  , initWithMergeTypeSelector
  , initSelector
  , resolveConflicts_errorSelector
  , resolveOptimisticLockingVersionConflicts_errorSelector
  , resolveConstraintConflicts_errorSelector
  , errorMergePolicySelector
  , rollbackMergePolicySelector
  , overwriteMergePolicySelector
  , mergeByPropertyObjectTrumpMergePolicySelector
  , mergeByPropertyStoreTrumpMergePolicySelector
  , mergeTypeSelector

  -- * Enum types
  , NSMergePolicyType(NSMergePolicyType)
  , pattern NSErrorMergePolicyType
  , pattern NSMergeByPropertyStoreTrumpMergePolicyType
  , pattern NSMergeByPropertyObjectTrumpMergePolicyType
  , pattern NSOverwriteMergePolicyType
  , pattern NSRollbackMergePolicyType

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

import ObjC.CoreData.Internal.Classes
import ObjC.CoreData.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithMergeType:@
initWithMergeType :: IsNSMergePolicy nsMergePolicy => nsMergePolicy -> NSMergePolicyType -> IO RawId
initWithMergeType nsMergePolicy  ty =
  fmap (RawId . castPtr) $ sendMsg nsMergePolicy (mkSelector "initWithMergeType:") (retPtr retVoid) [argCULong (coerce ty)]

-- | @- init@
init_ :: IsNSMergePolicy nsMergePolicy => nsMergePolicy -> IO (Id NSMergePolicy)
init_ nsMergePolicy  =
  sendMsg nsMergePolicy (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- resolveConflicts:error:@
resolveConflicts_error :: (IsNSMergePolicy nsMergePolicy, IsNSArray list, IsNSError error_) => nsMergePolicy -> list -> error_ -> IO Bool
resolveConflicts_error nsMergePolicy  list error_ =
withObjCPtr list $ \raw_list ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMergePolicy (mkSelector "resolveConflicts:error:") retCULong [argPtr (castPtr raw_list :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- resolveOptimisticLockingVersionConflicts:error:@
resolveOptimisticLockingVersionConflicts_error :: (IsNSMergePolicy nsMergePolicy, IsNSArray list, IsNSError error_) => nsMergePolicy -> list -> error_ -> IO Bool
resolveOptimisticLockingVersionConflicts_error nsMergePolicy  list error_ =
withObjCPtr list $ \raw_list ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMergePolicy (mkSelector "resolveOptimisticLockingVersionConflicts:error:") retCULong [argPtr (castPtr raw_list :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- resolveConstraintConflicts:error:@
resolveConstraintConflicts_error :: (IsNSMergePolicy nsMergePolicy, IsNSArray list, IsNSError error_) => nsMergePolicy -> list -> error_ -> IO Bool
resolveConstraintConflicts_error nsMergePolicy  list error_ =
withObjCPtr list $ \raw_list ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMergePolicy (mkSelector "resolveConstraintConflicts:error:") retCULong [argPtr (castPtr raw_list :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ errorMergePolicy@
errorMergePolicy :: IO (Id NSMergePolicy)
errorMergePolicy  =
  do
    cls' <- getRequiredClass "NSMergePolicy"
    sendClassMsg cls' (mkSelector "errorMergePolicy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ rollbackMergePolicy@
rollbackMergePolicy :: IO (Id NSMergePolicy)
rollbackMergePolicy  =
  do
    cls' <- getRequiredClass "NSMergePolicy"
    sendClassMsg cls' (mkSelector "rollbackMergePolicy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ overwriteMergePolicy@
overwriteMergePolicy :: IO (Id NSMergePolicy)
overwriteMergePolicy  =
  do
    cls' <- getRequiredClass "NSMergePolicy"
    sendClassMsg cls' (mkSelector "overwriteMergePolicy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ mergeByPropertyObjectTrumpMergePolicy@
mergeByPropertyObjectTrumpMergePolicy :: IO (Id NSMergePolicy)
mergeByPropertyObjectTrumpMergePolicy  =
  do
    cls' <- getRequiredClass "NSMergePolicy"
    sendClassMsg cls' (mkSelector "mergeByPropertyObjectTrumpMergePolicy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ mergeByPropertyStoreTrumpMergePolicy@
mergeByPropertyStoreTrumpMergePolicy :: IO (Id NSMergePolicy)
mergeByPropertyStoreTrumpMergePolicy  =
  do
    cls' <- getRequiredClass "NSMergePolicy"
    sendClassMsg cls' (mkSelector "mergeByPropertyStoreTrumpMergePolicy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- mergeType@
mergeType :: IsNSMergePolicy nsMergePolicy => nsMergePolicy -> IO NSMergePolicyType
mergeType nsMergePolicy  =
  fmap (coerce :: CULong -> NSMergePolicyType) $ sendMsg nsMergePolicy (mkSelector "mergeType") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMergeType:@
initWithMergeTypeSelector :: Selector
initWithMergeTypeSelector = mkSelector "initWithMergeType:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @resolveConflicts:error:@
resolveConflicts_errorSelector :: Selector
resolveConflicts_errorSelector = mkSelector "resolveConflicts:error:"

-- | @Selector@ for @resolveOptimisticLockingVersionConflicts:error:@
resolveOptimisticLockingVersionConflicts_errorSelector :: Selector
resolveOptimisticLockingVersionConflicts_errorSelector = mkSelector "resolveOptimisticLockingVersionConflicts:error:"

-- | @Selector@ for @resolveConstraintConflicts:error:@
resolveConstraintConflicts_errorSelector :: Selector
resolveConstraintConflicts_errorSelector = mkSelector "resolveConstraintConflicts:error:"

-- | @Selector@ for @errorMergePolicy@
errorMergePolicySelector :: Selector
errorMergePolicySelector = mkSelector "errorMergePolicy"

-- | @Selector@ for @rollbackMergePolicy@
rollbackMergePolicySelector :: Selector
rollbackMergePolicySelector = mkSelector "rollbackMergePolicy"

-- | @Selector@ for @overwriteMergePolicy@
overwriteMergePolicySelector :: Selector
overwriteMergePolicySelector = mkSelector "overwriteMergePolicy"

-- | @Selector@ for @mergeByPropertyObjectTrumpMergePolicy@
mergeByPropertyObjectTrumpMergePolicySelector :: Selector
mergeByPropertyObjectTrumpMergePolicySelector = mkSelector "mergeByPropertyObjectTrumpMergePolicy"

-- | @Selector@ for @mergeByPropertyStoreTrumpMergePolicy@
mergeByPropertyStoreTrumpMergePolicySelector :: Selector
mergeByPropertyStoreTrumpMergePolicySelector = mkSelector "mergeByPropertyStoreTrumpMergePolicy"

-- | @Selector@ for @mergeType@
mergeTypeSelector :: Selector
mergeTypeSelector = mkSelector "mergeType"

