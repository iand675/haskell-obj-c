{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSObject@.
module ObjC.Foundation.NSObject
  ( NSObject
  , IsNSObject(..)
  , scriptingIsEqualTo
  , scriptingIsLessThanOrEqualTo
  , scriptingIsLessThan
  , scriptingIsGreaterThanOrEqualTo
  , scriptingIsGreaterThan
  , scriptingBeginsWith
  , scriptingEndsWith
  , scriptingContains
  , isEqualTo
  , isLessThanOrEqualTo
  , isLessThan
  , isGreaterThanOrEqualTo
  , isGreaterThan
  , isNotEqualTo
  , doesContain
  , isLike
  , isCaseInsensitiveLike
  , indicesOfObjectsByEvaluatingObjectSpecifier
  , valueAtIndex_inPropertyWithKey
  , valueWithName_inPropertyWithKey
  , valueWithUniqueID_inPropertyWithKey
  , insertValue_atIndex_inPropertyWithKey
  , removeValueAtIndex_fromPropertyWithKey
  , replaceValueAtIndex_inPropertyWithKey_withValue
  , insertValue_inPropertyWithKey
  , coerceValue_forKey
  , scriptingValueForSpecifier
  , copyScriptingValue_forKey_withProperties
  , newScriptingObjectOfClass_forValueForKey_withContentsValue_properties
  , inverseForRelationshipKey
  , replacementObjectForPortCoder
  , replacementObjectForArchiver
  , performSelectorOnMainThread_withObject_waitUntilDone_modes
  , performSelectorOnMainThread_withObject_waitUntilDone
  , performSelector_onThread_withObject_waitUntilDone_modes
  , performSelector_onThread_withObject_waitUntilDone
  , performSelectorInBackground_withObject
  , classForKeyedUnarchiver
  , replacementObjectForKeyedArchiver
  , classFallbacksForKeyedArchiver
  , setSharedObservers
  , setKeys_triggerChangeNotificationsForDependentKey
  , keyPathsForValuesAffectingValueForKey
  , automaticallyNotifiesObserversForKey
  , willChangeValueForKey
  , didChangeValueForKey
  , willChange_valuesAtIndexes_forKey
  , didChange_valuesAtIndexes_forKey
  , willChangeValueForKey_withSetMutation_usingObjects
  , didChangeValueForKey_withSetMutation_usingObjects
  , addObserver_forKeyPath_options_context
  , removeObserver_forKeyPath_context
  , removeObserver_forKeyPath
  , observeValueForKeyPath_ofObject_change_context
  , useStoredAccessor
  , storedValueForKey
  , takeStoredValue_forKey
  , takeValue_forKey
  , takeValue_forKeyPath
  , handleQueryWithUnboundKey
  , handleTakeValue_forUnboundKey
  , unableToSetNilForKey
  , valuesForKeys
  , takeValuesFromDictionary
  , valueForKey
  , setValue_forKey
  , validateValue_forKey_error
  , mutableArrayValueForKey
  , mutableOrderedSetValueForKey
  , mutableSetValueForKey
  , valueForKeyPath
  , setValue_forKeyPath
  , validateValue_forKeyPath_error
  , mutableArrayValueForKeyPath
  , mutableOrderedSetValueForKeyPath
  , mutableSetValueForKeyPath
  , valueForUndefinedKey
  , setValue_forUndefinedKey
  , setNilValueForKey
  , dictionaryWithValuesForKeys
  , setValuesForKeysWithDictionary
  , fileManager_shouldProceedAfterError
  , fileManager_willProcessPath
  , urL_resourceDataDidBecomeAvailable
  , urlResourceDidFinishLoading
  , urlResourceDidCancelLoading
  , urL_resourceDidFailLoadingWithReason
  , performSelector_withObject_afterDelay_inModes
  , performSelector_withObject_afterDelay
  , cancelPreviousPerformRequestsWithTarget_selector_object
  , cancelPreviousPerformRequestsWithTarget
  , attemptRecoveryFromError_optionIndex_delegate_didRecoverSelector_contextInfo
  , attemptRecoveryFromError_optionIndex
  , poseAsClass
  , version
  , setVersion
  , replacementObjectForCoder
  , awakeAfterUsingCoder
  , objectSpecifier
  , classCode
  , className
  , scriptingProperties
  , setScriptingProperties
  , classDescription
  , attributeKeys
  , toOneRelationshipKeys
  , toManyRelationshipKeys
  , classForPortCoder
  , classForArchiver
  , classForKeyedArchiver
  , observationInfo
  , setObservationInfo
  , accessInstanceVariablesDirectly
  , autoContentAccessingProxy
  , classForCoder
  , scriptingIsEqualToSelector
  , scriptingIsLessThanOrEqualToSelector
  , scriptingIsLessThanSelector
  , scriptingIsGreaterThanOrEqualToSelector
  , scriptingIsGreaterThanSelector
  , scriptingBeginsWithSelector
  , scriptingEndsWithSelector
  , scriptingContainsSelector
  , isEqualToSelector
  , isLessThanOrEqualToSelector
  , isLessThanSelector
  , isGreaterThanOrEqualToSelector
  , isGreaterThanSelector
  , isNotEqualToSelector
  , doesContainSelector
  , isLikeSelector
  , isCaseInsensitiveLikeSelector
  , indicesOfObjectsByEvaluatingObjectSpecifierSelector
  , valueAtIndex_inPropertyWithKeySelector
  , valueWithName_inPropertyWithKeySelector
  , valueWithUniqueID_inPropertyWithKeySelector
  , insertValue_atIndex_inPropertyWithKeySelector
  , removeValueAtIndex_fromPropertyWithKeySelector
  , replaceValueAtIndex_inPropertyWithKey_withValueSelector
  , insertValue_inPropertyWithKeySelector
  , coerceValue_forKeySelector
  , scriptingValueForSpecifierSelector
  , copyScriptingValue_forKey_withPropertiesSelector
  , newScriptingObjectOfClass_forValueForKey_withContentsValue_propertiesSelector
  , inverseForRelationshipKeySelector
  , replacementObjectForPortCoderSelector
  , replacementObjectForArchiverSelector
  , performSelectorOnMainThread_withObject_waitUntilDone_modesSelector
  , performSelectorOnMainThread_withObject_waitUntilDoneSelector
  , performSelector_onThread_withObject_waitUntilDone_modesSelector
  , performSelector_onThread_withObject_waitUntilDoneSelector
  , performSelectorInBackground_withObjectSelector
  , classForKeyedUnarchiverSelector
  , replacementObjectForKeyedArchiverSelector
  , classFallbacksForKeyedArchiverSelector
  , setSharedObserversSelector
  , setKeys_triggerChangeNotificationsForDependentKeySelector
  , keyPathsForValuesAffectingValueForKeySelector
  , automaticallyNotifiesObserversForKeySelector
  , willChangeValueForKeySelector
  , didChangeValueForKeySelector
  , willChange_valuesAtIndexes_forKeySelector
  , didChange_valuesAtIndexes_forKeySelector
  , willChangeValueForKey_withSetMutation_usingObjectsSelector
  , didChangeValueForKey_withSetMutation_usingObjectsSelector
  , addObserver_forKeyPath_options_contextSelector
  , removeObserver_forKeyPath_contextSelector
  , removeObserver_forKeyPathSelector
  , observeValueForKeyPath_ofObject_change_contextSelector
  , useStoredAccessorSelector
  , storedValueForKeySelector
  , takeStoredValue_forKeySelector
  , takeValue_forKeySelector
  , takeValue_forKeyPathSelector
  , handleQueryWithUnboundKeySelector
  , handleTakeValue_forUnboundKeySelector
  , unableToSetNilForKeySelector
  , valuesForKeysSelector
  , takeValuesFromDictionarySelector
  , valueForKeySelector
  , setValue_forKeySelector
  , validateValue_forKey_errorSelector
  , mutableArrayValueForKeySelector
  , mutableOrderedSetValueForKeySelector
  , mutableSetValueForKeySelector
  , valueForKeyPathSelector
  , setValue_forKeyPathSelector
  , validateValue_forKeyPath_errorSelector
  , mutableArrayValueForKeyPathSelector
  , mutableOrderedSetValueForKeyPathSelector
  , mutableSetValueForKeyPathSelector
  , valueForUndefinedKeySelector
  , setValue_forUndefinedKeySelector
  , setNilValueForKeySelector
  , dictionaryWithValuesForKeysSelector
  , setValuesForKeysWithDictionarySelector
  , fileManager_shouldProceedAfterErrorSelector
  , fileManager_willProcessPathSelector
  , urL_resourceDataDidBecomeAvailableSelector
  , urlResourceDidFinishLoadingSelector
  , urlResourceDidCancelLoadingSelector
  , urL_resourceDidFailLoadingWithReasonSelector
  , performSelector_withObject_afterDelay_inModesSelector
  , performSelector_withObject_afterDelaySelector
  , cancelPreviousPerformRequestsWithTarget_selector_objectSelector
  , cancelPreviousPerformRequestsWithTargetSelector
  , attemptRecoveryFromError_optionIndex_delegate_didRecoverSelector_contextInfoSelector
  , attemptRecoveryFromError_optionIndexSelector
  , poseAsClassSelector
  , versionSelector
  , setVersionSelector
  , replacementObjectForCoderSelector
  , awakeAfterUsingCoderSelector
  , objectSpecifierSelector
  , classCodeSelector
  , classNameSelector
  , scriptingPropertiesSelector
  , setScriptingPropertiesSelector
  , classDescriptionSelector
  , attributeKeysSelector
  , toOneRelationshipKeysSelector
  , toManyRelationshipKeysSelector
  , classForPortCoderSelector
  , classForArchiverSelector
  , classForKeyedArchiverSelector
  , observationInfoSelector
  , setObservationInfoSelector
  , accessInstanceVariablesDirectlySelector
  , autoContentAccessingProxySelector
  , classForCoderSelector

  -- * Enum types
  , NSKeyValueChange(NSKeyValueChange)
  , pattern NSKeyValueChangeSetting
  , pattern NSKeyValueChangeInsertion
  , pattern NSKeyValueChangeRemoval
  , pattern NSKeyValueChangeReplacement
  , NSKeyValueObservingOptions(NSKeyValueObservingOptions)
  , pattern NSKeyValueObservingOptionNew
  , pattern NSKeyValueObservingOptionOld
  , pattern NSKeyValueObservingOptionInitial
  , pattern NSKeyValueObservingOptionPrior
  , NSKeyValueSetMutationKind(NSKeyValueSetMutationKind)
  , pattern NSKeyValueUnionSetMutation
  , pattern NSKeyValueMinusSetMutation
  , pattern NSKeyValueIntersectSetMutation
  , pattern NSKeyValueSetSetMutation

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

-- | @- scriptingIsEqualTo:@
scriptingIsEqualTo :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
scriptingIsEqualTo nsObject  object =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "scriptingIsEqualTo:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- scriptingIsLessThanOrEqualTo:@
scriptingIsLessThanOrEqualTo :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
scriptingIsLessThanOrEqualTo nsObject  object =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "scriptingIsLessThanOrEqualTo:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- scriptingIsLessThan:@
scriptingIsLessThan :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
scriptingIsLessThan nsObject  object =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "scriptingIsLessThan:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- scriptingIsGreaterThanOrEqualTo:@
scriptingIsGreaterThanOrEqualTo :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
scriptingIsGreaterThanOrEqualTo nsObject  object =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "scriptingIsGreaterThanOrEqualTo:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- scriptingIsGreaterThan:@
scriptingIsGreaterThan :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
scriptingIsGreaterThan nsObject  object =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "scriptingIsGreaterThan:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- scriptingBeginsWith:@
scriptingBeginsWith :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
scriptingBeginsWith nsObject  object =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "scriptingBeginsWith:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- scriptingEndsWith:@
scriptingEndsWith :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
scriptingEndsWith nsObject  object =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "scriptingEndsWith:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- scriptingContains:@
scriptingContains :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
scriptingContains nsObject  object =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "scriptingContains:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- isEqualTo:@
isEqualTo :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
isEqualTo nsObject  object =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "isEqualTo:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- isLessThanOrEqualTo:@
isLessThanOrEqualTo :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
isLessThanOrEqualTo nsObject  object =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "isLessThanOrEqualTo:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- isLessThan:@
isLessThan :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
isLessThan nsObject  object =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "isLessThan:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- isGreaterThanOrEqualTo:@
isGreaterThanOrEqualTo :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
isGreaterThanOrEqualTo nsObject  object =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "isGreaterThanOrEqualTo:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- isGreaterThan:@
isGreaterThan :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
isGreaterThan nsObject  object =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "isGreaterThan:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- isNotEqualTo:@
isNotEqualTo :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
isNotEqualTo nsObject  object =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "isNotEqualTo:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- doesContain:@
doesContain :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
doesContain nsObject  object =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "doesContain:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- isLike:@
isLike :: (IsNSObject nsObject, IsNSString object) => nsObject -> object -> IO Bool
isLike nsObject  object =
  withObjCPtr object $ \raw_object ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "isLike:") retCULong [argPtr (castPtr raw_object :: Ptr ())]

-- | @- isCaseInsensitiveLike:@
isCaseInsensitiveLike :: (IsNSObject nsObject, IsNSString object) => nsObject -> object -> IO Bool
isCaseInsensitiveLike nsObject  object =
  withObjCPtr object $ \raw_object ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "isCaseInsensitiveLike:") retCULong [argPtr (castPtr raw_object :: Ptr ())]

-- | @- indicesOfObjectsByEvaluatingObjectSpecifier:@
indicesOfObjectsByEvaluatingObjectSpecifier :: (IsNSObject nsObject, IsNSScriptObjectSpecifier specifier) => nsObject -> specifier -> IO (Id NSArray)
indicesOfObjectsByEvaluatingObjectSpecifier nsObject  specifier =
  withObjCPtr specifier $ \raw_specifier ->
      sendMsg nsObject (mkSelector "indicesOfObjectsByEvaluatingObjectSpecifier:") (retPtr retVoid) [argPtr (castPtr raw_specifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- valueAtIndex:inPropertyWithKey:@
valueAtIndex_inPropertyWithKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> CULong -> key -> IO RawId
valueAtIndex_inPropertyWithKey nsObject  index key =
  withObjCPtr key $ \raw_key ->
      fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "valueAtIndex:inPropertyWithKey:") (retPtr retVoid) [argCULong index, argPtr (castPtr raw_key :: Ptr ())]

-- | @- valueWithName:inPropertyWithKey:@
valueWithName_inPropertyWithKey :: (IsNSObject nsObject, IsNSString name, IsNSString key) => nsObject -> name -> key -> IO RawId
valueWithName_inPropertyWithKey nsObject  name key =
  withObjCPtr name $ \raw_name ->
    withObjCPtr key $ \raw_key ->
        fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "valueWithName:inPropertyWithKey:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- valueWithUniqueID:inPropertyWithKey:@
valueWithUniqueID_inPropertyWithKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> RawId -> key -> IO RawId
valueWithUniqueID_inPropertyWithKey nsObject  uniqueID key =
  withObjCPtr key $ \raw_key ->
      fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "valueWithUniqueID:inPropertyWithKey:") (retPtr retVoid) [argPtr (castPtr (unRawId uniqueID) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- insertValue:atIndex:inPropertyWithKey:@
insertValue_atIndex_inPropertyWithKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> RawId -> CULong -> key -> IO ()
insertValue_atIndex_inPropertyWithKey nsObject  value index key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsObject (mkSelector "insertValue:atIndex:inPropertyWithKey:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argCULong index, argPtr (castPtr raw_key :: Ptr ())]

-- | @- removeValueAtIndex:fromPropertyWithKey:@
removeValueAtIndex_fromPropertyWithKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> CULong -> key -> IO ()
removeValueAtIndex_fromPropertyWithKey nsObject  index key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsObject (mkSelector "removeValueAtIndex:fromPropertyWithKey:") retVoid [argCULong index, argPtr (castPtr raw_key :: Ptr ())]

-- | @- replaceValueAtIndex:inPropertyWithKey:withValue:@
replaceValueAtIndex_inPropertyWithKey_withValue :: (IsNSObject nsObject, IsNSString key) => nsObject -> CULong -> key -> RawId -> IO ()
replaceValueAtIndex_inPropertyWithKey_withValue nsObject  index key value =
  withObjCPtr key $ \raw_key ->
      sendMsg nsObject (mkSelector "replaceValueAtIndex:inPropertyWithKey:withValue:") retVoid [argCULong index, argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- insertValue:inPropertyWithKey:@
insertValue_inPropertyWithKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> RawId -> key -> IO ()
insertValue_inPropertyWithKey nsObject  value key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsObject (mkSelector "insertValue:inPropertyWithKey:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- coerceValue:forKey:@
coerceValue_forKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> RawId -> key -> IO RawId
coerceValue_forKey nsObject  value key =
  withObjCPtr key $ \raw_key ->
      fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "coerceValue:forKey:") (retPtr retVoid) [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- scriptingValueForSpecifier:@
scriptingValueForSpecifier :: (IsNSObject nsObject, IsNSScriptObjectSpecifier objectSpecifier) => nsObject -> objectSpecifier -> IO RawId
scriptingValueForSpecifier nsObject  objectSpecifier =
  withObjCPtr objectSpecifier $ \raw_objectSpecifier ->
      fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "scriptingValueForSpecifier:") (retPtr retVoid) [argPtr (castPtr raw_objectSpecifier :: Ptr ())]

-- | @- copyScriptingValue:forKey:withProperties:@
copyScriptingValue_forKey_withProperties :: (IsNSObject nsObject, IsNSString key, IsNSDictionary properties) => nsObject -> RawId -> key -> properties -> IO RawId
copyScriptingValue_forKey_withProperties nsObject  value key properties =
  withObjCPtr key $ \raw_key ->
    withObjCPtr properties $ \raw_properties ->
        fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "copyScriptingValue:forKey:withProperties:") (retPtr retVoid) [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_properties :: Ptr ())]

-- | @- newScriptingObjectOfClass:forValueForKey:withContentsValue:properties:@
newScriptingObjectOfClass_forValueForKey_withContentsValue_properties :: (IsNSObject nsObject, IsNSString key, IsNSDictionary properties) => nsObject -> Class -> key -> RawId -> properties -> IO RawId
newScriptingObjectOfClass_forValueForKey_withContentsValue_properties nsObject  objectClass key contentsValue properties =
  withObjCPtr key $ \raw_key ->
    withObjCPtr properties $ \raw_properties ->
        fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "newScriptingObjectOfClass:forValueForKey:withContentsValue:properties:") (retPtr retVoid) [argPtr (unClass objectClass), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr (unRawId contentsValue) :: Ptr ()), argPtr (castPtr raw_properties :: Ptr ())]

-- | @- inverseForRelationshipKey:@
inverseForRelationshipKey :: (IsNSObject nsObject, IsNSString relationshipKey) => nsObject -> relationshipKey -> IO (Id NSString)
inverseForRelationshipKey nsObject  relationshipKey =
  withObjCPtr relationshipKey $ \raw_relationshipKey ->
      sendMsg nsObject (mkSelector "inverseForRelationshipKey:") (retPtr retVoid) [argPtr (castPtr raw_relationshipKey :: Ptr ())] >>= retainedObject . castPtr

-- | @- replacementObjectForPortCoder:@
replacementObjectForPortCoder :: (IsNSObject nsObject, IsNSPortCoder coder) => nsObject -> coder -> IO RawId
replacementObjectForPortCoder nsObject  coder =
  withObjCPtr coder $ \raw_coder ->
      fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "replacementObjectForPortCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())]

-- | @- replacementObjectForArchiver:@
replacementObjectForArchiver :: (IsNSObject nsObject, IsNSArchiver archiver) => nsObject -> archiver -> IO RawId
replacementObjectForArchiver nsObject  archiver =
  withObjCPtr archiver $ \raw_archiver ->
      fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "replacementObjectForArchiver:") (retPtr retVoid) [argPtr (castPtr raw_archiver :: Ptr ())]

-- | @- performSelectorOnMainThread:withObject:waitUntilDone:modes:@
performSelectorOnMainThread_withObject_waitUntilDone_modes :: (IsNSObject nsObject, IsNSArray array) => nsObject -> Selector -> RawId -> Bool -> array -> IO ()
performSelectorOnMainThread_withObject_waitUntilDone_modes nsObject  aSelector arg wait array =
  withObjCPtr array $ \raw_array ->
      sendMsg nsObject (mkSelector "performSelectorOnMainThread:withObject:waitUntilDone:modes:") retVoid [argPtr (unSelector aSelector), argPtr (castPtr (unRawId arg) :: Ptr ()), argCULong (if wait then 1 else 0), argPtr (castPtr raw_array :: Ptr ())]

-- | @- performSelectorOnMainThread:withObject:waitUntilDone:@
performSelectorOnMainThread_withObject_waitUntilDone :: IsNSObject nsObject => nsObject -> Selector -> RawId -> Bool -> IO ()
performSelectorOnMainThread_withObject_waitUntilDone nsObject  aSelector arg wait =
    sendMsg nsObject (mkSelector "performSelectorOnMainThread:withObject:waitUntilDone:") retVoid [argPtr (unSelector aSelector), argPtr (castPtr (unRawId arg) :: Ptr ()), argCULong (if wait then 1 else 0)]

-- | @- performSelector:onThread:withObject:waitUntilDone:modes:@
performSelector_onThread_withObject_waitUntilDone_modes :: (IsNSObject nsObject, IsNSThread thr, IsNSArray array) => nsObject -> Selector -> thr -> RawId -> Bool -> array -> IO ()
performSelector_onThread_withObject_waitUntilDone_modes nsObject  aSelector thr arg wait array =
  withObjCPtr thr $ \raw_thr ->
    withObjCPtr array $ \raw_array ->
        sendMsg nsObject (mkSelector "performSelector:onThread:withObject:waitUntilDone:modes:") retVoid [argPtr (unSelector aSelector), argPtr (castPtr raw_thr :: Ptr ()), argPtr (castPtr (unRawId arg) :: Ptr ()), argCULong (if wait then 1 else 0), argPtr (castPtr raw_array :: Ptr ())]

-- | @- performSelector:onThread:withObject:waitUntilDone:@
performSelector_onThread_withObject_waitUntilDone :: (IsNSObject nsObject, IsNSThread thr) => nsObject -> Selector -> thr -> RawId -> Bool -> IO ()
performSelector_onThread_withObject_waitUntilDone nsObject  aSelector thr arg wait =
  withObjCPtr thr $ \raw_thr ->
      sendMsg nsObject (mkSelector "performSelector:onThread:withObject:waitUntilDone:") retVoid [argPtr (unSelector aSelector), argPtr (castPtr raw_thr :: Ptr ()), argPtr (castPtr (unRawId arg) :: Ptr ()), argCULong (if wait then 1 else 0)]

-- | @- performSelectorInBackground:withObject:@
performSelectorInBackground_withObject :: IsNSObject nsObject => nsObject -> Selector -> RawId -> IO ()
performSelectorInBackground_withObject nsObject  aSelector arg =
    sendMsg nsObject (mkSelector "performSelectorInBackground:withObject:") retVoid [argPtr (unSelector aSelector), argPtr (castPtr (unRawId arg) :: Ptr ())]

-- | @+ classForKeyedUnarchiver@
classForKeyedUnarchiver :: IO Class
classForKeyedUnarchiver  =
  do
    cls' <- getRequiredClass "NSObject"
    fmap (Class . castPtr) $ sendClassMsg cls' (mkSelector "classForKeyedUnarchiver") (retPtr retVoid) []

-- | @- replacementObjectForKeyedArchiver:@
replacementObjectForKeyedArchiver :: (IsNSObject nsObject, IsNSKeyedArchiver archiver) => nsObject -> archiver -> IO RawId
replacementObjectForKeyedArchiver nsObject  archiver =
  withObjCPtr archiver $ \raw_archiver ->
      fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "replacementObjectForKeyedArchiver:") (retPtr retVoid) [argPtr (castPtr raw_archiver :: Ptr ())]

-- | @+ classFallbacksForKeyedArchiver@
classFallbacksForKeyedArchiver :: IO (Id NSArray)
classFallbacksForKeyedArchiver  =
  do
    cls' <- getRequiredClass "NSObject"
    sendClassMsg cls' (mkSelector "classFallbacksForKeyedArchiver") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Register shared observations.
--
-- A shared observation collection might be shared between multiple observables to minimise registration work. Shared observers remain registered throughout the object's lifetime and do not need to be removed using @removeObserver:@.
--
-- An observable may only have one set of shared observations. Subsequent calls to this method will replace existing shared observations.
--
-- - Parameter sharedObservers: shared observer collection that was initialized   with the class of this object - Invariant: @sharedObserers@ was initialized with the class of this object - Throws: Exception if the class of the receiving observable object does not   match the class with which @sharedObserers@ was initialized.
--
-- ObjC selector: @- setSharedObservers:@
setSharedObservers :: (IsNSObject nsObject, IsNSKeyValueSharedObserversSnapshot sharedObservers) => nsObject -> sharedObservers -> IO ()
setSharedObservers nsObject  sharedObservers =
  withObjCPtr sharedObservers $ \raw_sharedObservers ->
      sendMsg nsObject (mkSelector "setSharedObservers:") retVoid [argPtr (castPtr raw_sharedObservers :: Ptr ())]

-- | @+ setKeys:triggerChangeNotificationsForDependentKey:@
setKeys_triggerChangeNotificationsForDependentKey :: (IsNSArray keys, IsNSString dependentKey) => keys -> dependentKey -> IO ()
setKeys_triggerChangeNotificationsForDependentKey keys dependentKey =
  do
    cls' <- getRequiredClass "NSObject"
    withObjCPtr keys $ \raw_keys ->
      withObjCPtr dependentKey $ \raw_dependentKey ->
        sendClassMsg cls' (mkSelector "setKeys:triggerChangeNotificationsForDependentKey:") retVoid [argPtr (castPtr raw_keys :: Ptr ()), argPtr (castPtr raw_dependentKey :: Ptr ())]

-- | @+ keyPathsForValuesAffectingValueForKey:@
keyPathsForValuesAffectingValueForKey :: IsNSString key => key -> IO (Id NSSet)
keyPathsForValuesAffectingValueForKey key =
  do
    cls' <- getRequiredClass "NSObject"
    withObjCPtr key $ \raw_key ->
      sendClassMsg cls' (mkSelector "keyPathsForValuesAffectingValueForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | @+ automaticallyNotifiesObserversForKey:@
automaticallyNotifiesObserversForKey :: IsNSString key => key -> IO Bool
automaticallyNotifiesObserversForKey key =
  do
    cls' <- getRequiredClass "NSObject"
    withObjCPtr key $ \raw_key ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "automaticallyNotifiesObserversForKey:") retCULong [argPtr (castPtr raw_key :: Ptr ())]

-- | @- willChangeValueForKey:@
willChangeValueForKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> key -> IO ()
willChangeValueForKey nsObject  key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsObject (mkSelector "willChangeValueForKey:") retVoid [argPtr (castPtr raw_key :: Ptr ())]

-- | @- didChangeValueForKey:@
didChangeValueForKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> key -> IO ()
didChangeValueForKey nsObject  key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsObject (mkSelector "didChangeValueForKey:") retVoid [argPtr (castPtr raw_key :: Ptr ())]

-- | @- willChange:valuesAtIndexes:forKey:@
willChange_valuesAtIndexes_forKey :: (IsNSObject nsObject, IsNSIndexSet indexes, IsNSString key) => nsObject -> NSKeyValueChange -> indexes -> key -> IO ()
willChange_valuesAtIndexes_forKey nsObject  changeKind indexes key =
  withObjCPtr indexes $ \raw_indexes ->
    withObjCPtr key $ \raw_key ->
        sendMsg nsObject (mkSelector "willChange:valuesAtIndexes:forKey:") retVoid [argCULong (coerce changeKind), argPtr (castPtr raw_indexes :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- didChange:valuesAtIndexes:forKey:@
didChange_valuesAtIndexes_forKey :: (IsNSObject nsObject, IsNSIndexSet indexes, IsNSString key) => nsObject -> NSKeyValueChange -> indexes -> key -> IO ()
didChange_valuesAtIndexes_forKey nsObject  changeKind indexes key =
  withObjCPtr indexes $ \raw_indexes ->
    withObjCPtr key $ \raw_key ->
        sendMsg nsObject (mkSelector "didChange:valuesAtIndexes:forKey:") retVoid [argCULong (coerce changeKind), argPtr (castPtr raw_indexes :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- willChangeValueForKey:withSetMutation:usingObjects:@
willChangeValueForKey_withSetMutation_usingObjects :: (IsNSObject nsObject, IsNSString key, IsNSSet objects) => nsObject -> key -> NSKeyValueSetMutationKind -> objects -> IO ()
willChangeValueForKey_withSetMutation_usingObjects nsObject  key mutationKind objects =
  withObjCPtr key $ \raw_key ->
    withObjCPtr objects $ \raw_objects ->
        sendMsg nsObject (mkSelector "willChangeValueForKey:withSetMutation:usingObjects:") retVoid [argPtr (castPtr raw_key :: Ptr ()), argCULong (coerce mutationKind), argPtr (castPtr raw_objects :: Ptr ())]

-- | @- didChangeValueForKey:withSetMutation:usingObjects:@
didChangeValueForKey_withSetMutation_usingObjects :: (IsNSObject nsObject, IsNSString key, IsNSSet objects) => nsObject -> key -> NSKeyValueSetMutationKind -> objects -> IO ()
didChangeValueForKey_withSetMutation_usingObjects nsObject  key mutationKind objects =
  withObjCPtr key $ \raw_key ->
    withObjCPtr objects $ \raw_objects ->
        sendMsg nsObject (mkSelector "didChangeValueForKey:withSetMutation:usingObjects:") retVoid [argPtr (castPtr raw_key :: Ptr ()), argCULong (coerce mutationKind), argPtr (castPtr raw_objects :: Ptr ())]

-- | @- addObserver:forKeyPath:options:context:@
addObserver_forKeyPath_options_context :: (IsNSObject nsObject, IsNSObject observer, IsNSString keyPath) => nsObject -> observer -> keyPath -> NSKeyValueObservingOptions -> Ptr () -> IO ()
addObserver_forKeyPath_options_context nsObject  observer keyPath options context =
  withObjCPtr observer $ \raw_observer ->
    withObjCPtr keyPath $ \raw_keyPath ->
        sendMsg nsObject (mkSelector "addObserver:forKeyPath:options:context:") retVoid [argPtr (castPtr raw_observer :: Ptr ()), argPtr (castPtr raw_keyPath :: Ptr ()), argCULong (coerce options), argPtr context]

-- | @- removeObserver:forKeyPath:context:@
removeObserver_forKeyPath_context :: (IsNSObject nsObject, IsNSObject observer, IsNSString keyPath) => nsObject -> observer -> keyPath -> Ptr () -> IO ()
removeObserver_forKeyPath_context nsObject  observer keyPath context =
  withObjCPtr observer $ \raw_observer ->
    withObjCPtr keyPath $ \raw_keyPath ->
        sendMsg nsObject (mkSelector "removeObserver:forKeyPath:context:") retVoid [argPtr (castPtr raw_observer :: Ptr ()), argPtr (castPtr raw_keyPath :: Ptr ()), argPtr context]

-- | @- removeObserver:forKeyPath:@
removeObserver_forKeyPath :: (IsNSObject nsObject, IsNSObject observer, IsNSString keyPath) => nsObject -> observer -> keyPath -> IO ()
removeObserver_forKeyPath nsObject  observer keyPath =
  withObjCPtr observer $ \raw_observer ->
    withObjCPtr keyPath $ \raw_keyPath ->
        sendMsg nsObject (mkSelector "removeObserver:forKeyPath:") retVoid [argPtr (castPtr raw_observer :: Ptr ()), argPtr (castPtr raw_keyPath :: Ptr ())]

-- | @- observeValueForKeyPath:ofObject:change:context:@
observeValueForKeyPath_ofObject_change_context :: (IsNSObject nsObject, IsNSString keyPath, IsNSDictionary change) => nsObject -> keyPath -> RawId -> change -> Ptr () -> IO ()
observeValueForKeyPath_ofObject_change_context nsObject  keyPath object change context =
  withObjCPtr keyPath $ \raw_keyPath ->
    withObjCPtr change $ \raw_change ->
        sendMsg nsObject (mkSelector "observeValueForKeyPath:ofObject:change:context:") retVoid [argPtr (castPtr raw_keyPath :: Ptr ()), argPtr (castPtr (unRawId object) :: Ptr ()), argPtr (castPtr raw_change :: Ptr ()), argPtr context]

-- | @+ useStoredAccessor@
useStoredAccessor :: IO Bool
useStoredAccessor  =
  do
    cls' <- getRequiredClass "NSObject"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "useStoredAccessor") retCULong []

-- | @- storedValueForKey:@
storedValueForKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> key -> IO RawId
storedValueForKey nsObject  key =
  withObjCPtr key $ \raw_key ->
      fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "storedValueForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- takeStoredValue:forKey:@
takeStoredValue_forKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> RawId -> key -> IO ()
takeStoredValue_forKey nsObject  value key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsObject (mkSelector "takeStoredValue:forKey:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- takeValue:forKey:@
takeValue_forKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> RawId -> key -> IO ()
takeValue_forKey nsObject  value key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsObject (mkSelector "takeValue:forKey:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- takeValue:forKeyPath:@
takeValue_forKeyPath :: (IsNSObject nsObject, IsNSString keyPath) => nsObject -> RawId -> keyPath -> IO ()
takeValue_forKeyPath nsObject  value keyPath =
  withObjCPtr keyPath $ \raw_keyPath ->
      sendMsg nsObject (mkSelector "takeValue:forKeyPath:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_keyPath :: Ptr ())]

-- | @- handleQueryWithUnboundKey:@
handleQueryWithUnboundKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> key -> IO RawId
handleQueryWithUnboundKey nsObject  key =
  withObjCPtr key $ \raw_key ->
      fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "handleQueryWithUnboundKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- handleTakeValue:forUnboundKey:@
handleTakeValue_forUnboundKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> RawId -> key -> IO ()
handleTakeValue_forUnboundKey nsObject  value key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsObject (mkSelector "handleTakeValue:forUnboundKey:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- unableToSetNilForKey:@
unableToSetNilForKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> key -> IO ()
unableToSetNilForKey nsObject  key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsObject (mkSelector "unableToSetNilForKey:") retVoid [argPtr (castPtr raw_key :: Ptr ())]

-- | @- valuesForKeys:@
valuesForKeys :: (IsNSObject nsObject, IsNSArray keys) => nsObject -> keys -> IO (Id NSDictionary)
valuesForKeys nsObject  keys =
  withObjCPtr keys $ \raw_keys ->
      sendMsg nsObject (mkSelector "valuesForKeys:") (retPtr retVoid) [argPtr (castPtr raw_keys :: Ptr ())] >>= retainedObject . castPtr

-- | @- takeValuesFromDictionary:@
takeValuesFromDictionary :: (IsNSObject nsObject, IsNSDictionary properties) => nsObject -> properties -> IO ()
takeValuesFromDictionary nsObject  properties =
  withObjCPtr properties $ \raw_properties ->
      sendMsg nsObject (mkSelector "takeValuesFromDictionary:") retVoid [argPtr (castPtr raw_properties :: Ptr ())]

-- | @- valueForKey:@
valueForKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> key -> IO RawId
valueForKey nsObject  key =
  withObjCPtr key $ \raw_key ->
      fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "valueForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- setValue:forKey:@
setValue_forKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> RawId -> key -> IO ()
setValue_forKey nsObject  value key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsObject (mkSelector "setValue:forKey:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- validateValue:forKey:error:@
validateValue_forKey_error :: (IsNSObject nsObject, IsNSString inKey, IsNSError outError) => nsObject -> Ptr RawId -> inKey -> outError -> IO Bool
validateValue_forKey_error nsObject  ioValue inKey outError =
  withObjCPtr inKey $ \raw_inKey ->
    withObjCPtr outError $ \raw_outError ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "validateValue:forKey:error:") retCULong [argPtr ioValue, argPtr (castPtr raw_inKey :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- mutableArrayValueForKey:@
mutableArrayValueForKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> key -> IO (Id NSMutableArray)
mutableArrayValueForKey nsObject  key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsObject (mkSelector "mutableArrayValueForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | @- mutableOrderedSetValueForKey:@
mutableOrderedSetValueForKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> key -> IO (Id NSMutableOrderedSet)
mutableOrderedSetValueForKey nsObject  key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsObject (mkSelector "mutableOrderedSetValueForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | @- mutableSetValueForKey:@
mutableSetValueForKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> key -> IO (Id NSMutableSet)
mutableSetValueForKey nsObject  key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsObject (mkSelector "mutableSetValueForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | @- valueForKeyPath:@
valueForKeyPath :: (IsNSObject nsObject, IsNSString keyPath) => nsObject -> keyPath -> IO RawId
valueForKeyPath nsObject  keyPath =
  withObjCPtr keyPath $ \raw_keyPath ->
      fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "valueForKeyPath:") (retPtr retVoid) [argPtr (castPtr raw_keyPath :: Ptr ())]

-- | @- setValue:forKeyPath:@
setValue_forKeyPath :: (IsNSObject nsObject, IsNSString keyPath) => nsObject -> RawId -> keyPath -> IO ()
setValue_forKeyPath nsObject  value keyPath =
  withObjCPtr keyPath $ \raw_keyPath ->
      sendMsg nsObject (mkSelector "setValue:forKeyPath:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_keyPath :: Ptr ())]

-- | @- validateValue:forKeyPath:error:@
validateValue_forKeyPath_error :: (IsNSObject nsObject, IsNSString inKeyPath, IsNSError outError) => nsObject -> Ptr RawId -> inKeyPath -> outError -> IO Bool
validateValue_forKeyPath_error nsObject  ioValue inKeyPath outError =
  withObjCPtr inKeyPath $ \raw_inKeyPath ->
    withObjCPtr outError $ \raw_outError ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "validateValue:forKeyPath:error:") retCULong [argPtr ioValue, argPtr (castPtr raw_inKeyPath :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- mutableArrayValueForKeyPath:@
mutableArrayValueForKeyPath :: (IsNSObject nsObject, IsNSString keyPath) => nsObject -> keyPath -> IO (Id NSMutableArray)
mutableArrayValueForKeyPath nsObject  keyPath =
  withObjCPtr keyPath $ \raw_keyPath ->
      sendMsg nsObject (mkSelector "mutableArrayValueForKeyPath:") (retPtr retVoid) [argPtr (castPtr raw_keyPath :: Ptr ())] >>= retainedObject . castPtr

-- | @- mutableOrderedSetValueForKeyPath:@
mutableOrderedSetValueForKeyPath :: (IsNSObject nsObject, IsNSString keyPath) => nsObject -> keyPath -> IO (Id NSMutableOrderedSet)
mutableOrderedSetValueForKeyPath nsObject  keyPath =
  withObjCPtr keyPath $ \raw_keyPath ->
      sendMsg nsObject (mkSelector "mutableOrderedSetValueForKeyPath:") (retPtr retVoid) [argPtr (castPtr raw_keyPath :: Ptr ())] >>= retainedObject . castPtr

-- | @- mutableSetValueForKeyPath:@
mutableSetValueForKeyPath :: (IsNSObject nsObject, IsNSString keyPath) => nsObject -> keyPath -> IO (Id NSMutableSet)
mutableSetValueForKeyPath nsObject  keyPath =
  withObjCPtr keyPath $ \raw_keyPath ->
      sendMsg nsObject (mkSelector "mutableSetValueForKeyPath:") (retPtr retVoid) [argPtr (castPtr raw_keyPath :: Ptr ())] >>= retainedObject . castPtr

-- | @- valueForUndefinedKey:@
valueForUndefinedKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> key -> IO RawId
valueForUndefinedKey nsObject  key =
  withObjCPtr key $ \raw_key ->
      fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "valueForUndefinedKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- setValue:forUndefinedKey:@
setValue_forUndefinedKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> RawId -> key -> IO ()
setValue_forUndefinedKey nsObject  value key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsObject (mkSelector "setValue:forUndefinedKey:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- setNilValueForKey:@
setNilValueForKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> key -> IO ()
setNilValueForKey nsObject  key =
  withObjCPtr key $ \raw_key ->
      sendMsg nsObject (mkSelector "setNilValueForKey:") retVoid [argPtr (castPtr raw_key :: Ptr ())]

-- | @- dictionaryWithValuesForKeys:@
dictionaryWithValuesForKeys :: (IsNSObject nsObject, IsNSArray keys) => nsObject -> keys -> IO (Id NSDictionary)
dictionaryWithValuesForKeys nsObject  keys =
  withObjCPtr keys $ \raw_keys ->
      sendMsg nsObject (mkSelector "dictionaryWithValuesForKeys:") (retPtr retVoid) [argPtr (castPtr raw_keys :: Ptr ())] >>= retainedObject . castPtr

-- | @- setValuesForKeysWithDictionary:@
setValuesForKeysWithDictionary :: (IsNSObject nsObject, IsNSDictionary keyedValues) => nsObject -> keyedValues -> IO ()
setValuesForKeysWithDictionary nsObject  keyedValues =
  withObjCPtr keyedValues $ \raw_keyedValues ->
      sendMsg nsObject (mkSelector "setValuesForKeysWithDictionary:") retVoid [argPtr (castPtr raw_keyedValues :: Ptr ())]

-- | @- fileManager:shouldProceedAfterError:@
fileManager_shouldProceedAfterError :: (IsNSObject nsObject, IsNSFileManager fm, IsNSDictionary errorInfo) => nsObject -> fm -> errorInfo -> IO Bool
fileManager_shouldProceedAfterError nsObject  fm errorInfo =
  withObjCPtr fm $ \raw_fm ->
    withObjCPtr errorInfo $ \raw_errorInfo ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "fileManager:shouldProceedAfterError:") retCULong [argPtr (castPtr raw_fm :: Ptr ()), argPtr (castPtr raw_errorInfo :: Ptr ())]

-- | @- fileManager:willProcessPath:@
fileManager_willProcessPath :: (IsNSObject nsObject, IsNSFileManager fm, IsNSString path) => nsObject -> fm -> path -> IO ()
fileManager_willProcessPath nsObject  fm path =
  withObjCPtr fm $ \raw_fm ->
    withObjCPtr path $ \raw_path ->
        sendMsg nsObject (mkSelector "fileManager:willProcessPath:") retVoid [argPtr (castPtr raw_fm :: Ptr ()), argPtr (castPtr raw_path :: Ptr ())]

-- | @- URL:resourceDataDidBecomeAvailable:@
urL_resourceDataDidBecomeAvailable :: (IsNSObject nsObject, IsNSURL sender, IsNSData newBytes) => nsObject -> sender -> newBytes -> IO ()
urL_resourceDataDidBecomeAvailable nsObject  sender newBytes =
  withObjCPtr sender $ \raw_sender ->
    withObjCPtr newBytes $ \raw_newBytes ->
        sendMsg nsObject (mkSelector "URL:resourceDataDidBecomeAvailable:") retVoid [argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_newBytes :: Ptr ())]

-- | @- URLResourceDidFinishLoading:@
urlResourceDidFinishLoading :: (IsNSObject nsObject, IsNSURL sender) => nsObject -> sender -> IO ()
urlResourceDidFinishLoading nsObject  sender =
  withObjCPtr sender $ \raw_sender ->
      sendMsg nsObject (mkSelector "URLResourceDidFinishLoading:") retVoid [argPtr (castPtr raw_sender :: Ptr ())]

-- | @- URLResourceDidCancelLoading:@
urlResourceDidCancelLoading :: (IsNSObject nsObject, IsNSURL sender) => nsObject -> sender -> IO ()
urlResourceDidCancelLoading nsObject  sender =
  withObjCPtr sender $ \raw_sender ->
      sendMsg nsObject (mkSelector "URLResourceDidCancelLoading:") retVoid [argPtr (castPtr raw_sender :: Ptr ())]

-- | @- URL:resourceDidFailLoadingWithReason:@
urL_resourceDidFailLoadingWithReason :: (IsNSObject nsObject, IsNSURL sender, IsNSString reason) => nsObject -> sender -> reason -> IO ()
urL_resourceDidFailLoadingWithReason nsObject  sender reason =
  withObjCPtr sender $ \raw_sender ->
    withObjCPtr reason $ \raw_reason ->
        sendMsg nsObject (mkSelector "URL:resourceDidFailLoadingWithReason:") retVoid [argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_reason :: Ptr ())]

-- | @- performSelector:withObject:afterDelay:inModes:@
performSelector_withObject_afterDelay_inModes :: (IsNSObject nsObject, IsNSArray modes) => nsObject -> Selector -> RawId -> CDouble -> modes -> IO ()
performSelector_withObject_afterDelay_inModes nsObject  aSelector anArgument delay modes =
  withObjCPtr modes $ \raw_modes ->
      sendMsg nsObject (mkSelector "performSelector:withObject:afterDelay:inModes:") retVoid [argPtr (unSelector aSelector), argPtr (castPtr (unRawId anArgument) :: Ptr ()), argCDouble delay, argPtr (castPtr raw_modes :: Ptr ())]

-- | @- performSelector:withObject:afterDelay:@
performSelector_withObject_afterDelay :: IsNSObject nsObject => nsObject -> Selector -> RawId -> CDouble -> IO ()
performSelector_withObject_afterDelay nsObject  aSelector anArgument delay =
    sendMsg nsObject (mkSelector "performSelector:withObject:afterDelay:") retVoid [argPtr (unSelector aSelector), argPtr (castPtr (unRawId anArgument) :: Ptr ()), argCDouble delay]

-- | @+ cancelPreviousPerformRequestsWithTarget:selector:object:@
cancelPreviousPerformRequestsWithTarget_selector_object :: RawId -> Selector -> RawId -> IO ()
cancelPreviousPerformRequestsWithTarget_selector_object aTarget aSelector anArgument =
  do
    cls' <- getRequiredClass "NSObject"
    sendClassMsg cls' (mkSelector "cancelPreviousPerformRequestsWithTarget:selector:object:") retVoid [argPtr (castPtr (unRawId aTarget) :: Ptr ()), argPtr (unSelector aSelector), argPtr (castPtr (unRawId anArgument) :: Ptr ())]

-- | @+ cancelPreviousPerformRequestsWithTarget:@
cancelPreviousPerformRequestsWithTarget :: RawId -> IO ()
cancelPreviousPerformRequestsWithTarget aTarget =
  do
    cls' <- getRequiredClass "NSObject"
    sendClassMsg cls' (mkSelector "cancelPreviousPerformRequestsWithTarget:") retVoid [argPtr (castPtr (unRawId aTarget) :: Ptr ())]

-- | @- attemptRecoveryFromError:optionIndex:delegate:didRecoverSelector:contextInfo:@
attemptRecoveryFromError_optionIndex_delegate_didRecoverSelector_contextInfo :: (IsNSObject nsObject, IsNSError error_) => nsObject -> error_ -> CULong -> RawId -> Selector -> Ptr () -> IO ()
attemptRecoveryFromError_optionIndex_delegate_didRecoverSelector_contextInfo nsObject  error_ recoveryOptionIndex delegate didRecoverSelector contextInfo =
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsObject (mkSelector "attemptRecoveryFromError:optionIndex:delegate:didRecoverSelector:contextInfo:") retVoid [argPtr (castPtr raw_error_ :: Ptr ()), argCULong recoveryOptionIndex, argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didRecoverSelector), argPtr contextInfo]

-- | @- attemptRecoveryFromError:optionIndex:@
attemptRecoveryFromError_optionIndex :: (IsNSObject nsObject, IsNSError error_) => nsObject -> error_ -> CULong -> IO Bool
attemptRecoveryFromError_optionIndex nsObject  error_ recoveryOptionIndex =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "attemptRecoveryFromError:optionIndex:") retCULong [argPtr (castPtr raw_error_ :: Ptr ()), argCULong recoveryOptionIndex]

-- | @+ poseAsClass:@
poseAsClass :: Class -> IO ()
poseAsClass aClass =
  do
    cls' <- getRequiredClass "NSObject"
    sendClassMsg cls' (mkSelector "poseAsClass:") retVoid [argPtr (unClass aClass)]

-- | @+ version@
version :: IO CLong
version  =
  do
    cls' <- getRequiredClass "NSObject"
    sendClassMsg cls' (mkSelector "version") retCLong []

-- | @+ setVersion:@
setVersion :: CLong -> IO ()
setVersion aVersion =
  do
    cls' <- getRequiredClass "NSObject"
    sendClassMsg cls' (mkSelector "setVersion:") retVoid [argCLong aVersion]

-- | @- replacementObjectForCoder:@
replacementObjectForCoder :: (IsNSObject nsObject, IsNSCoder coder) => nsObject -> coder -> IO RawId
replacementObjectForCoder nsObject  coder =
  withObjCPtr coder $ \raw_coder ->
      fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "replacementObjectForCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())]

-- | @- awakeAfterUsingCoder:@
awakeAfterUsingCoder :: (IsNSObject nsObject, IsNSCoder coder) => nsObject -> coder -> IO RawId
awakeAfterUsingCoder nsObject  coder =
  withObjCPtr coder $ \raw_coder ->
      fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "awakeAfterUsingCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())]

-- | @- objectSpecifier@
objectSpecifier :: IsNSObject nsObject => nsObject -> IO (Id NSScriptObjectSpecifier)
objectSpecifier nsObject  =
    sendMsg nsObject (mkSelector "objectSpecifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- classCode@
classCode :: IsNSObject nsObject => nsObject -> IO CUInt
classCode nsObject  =
    sendMsg nsObject (mkSelector "classCode") retCUInt []

-- | @- className@
className :: IsNSObject nsObject => nsObject -> IO (Id NSString)
className nsObject  =
    sendMsg nsObject (mkSelector "className") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- scriptingProperties@
scriptingProperties :: IsNSObject nsObject => nsObject -> IO (Id NSDictionary)
scriptingProperties nsObject  =
    sendMsg nsObject (mkSelector "scriptingProperties") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setScriptingProperties:@
setScriptingProperties :: (IsNSObject nsObject, IsNSDictionary value) => nsObject -> value -> IO ()
setScriptingProperties nsObject  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsObject (mkSelector "setScriptingProperties:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- classDescription@
classDescription :: IsNSObject nsObject => nsObject -> IO (Id NSClassDescription)
classDescription nsObject  =
    sendMsg nsObject (mkSelector "classDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- attributeKeys@
attributeKeys :: IsNSObject nsObject => nsObject -> IO (Id NSArray)
attributeKeys nsObject  =
    sendMsg nsObject (mkSelector "attributeKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- toOneRelationshipKeys@
toOneRelationshipKeys :: IsNSObject nsObject => nsObject -> IO (Id NSArray)
toOneRelationshipKeys nsObject  =
    sendMsg nsObject (mkSelector "toOneRelationshipKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- toManyRelationshipKeys@
toManyRelationshipKeys :: IsNSObject nsObject => nsObject -> IO (Id NSArray)
toManyRelationshipKeys nsObject  =
    sendMsg nsObject (mkSelector "toManyRelationshipKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- classForPortCoder@
classForPortCoder :: IsNSObject nsObject => nsObject -> IO Class
classForPortCoder nsObject  =
    fmap (Class . castPtr) $ sendMsg nsObject (mkSelector "classForPortCoder") (retPtr retVoid) []

-- | @- classForArchiver@
classForArchiver :: IsNSObject nsObject => nsObject -> IO Class
classForArchiver nsObject  =
    fmap (Class . castPtr) $ sendMsg nsObject (mkSelector "classForArchiver") (retPtr retVoid) []

-- | @- classForKeyedArchiver@
classForKeyedArchiver :: IsNSObject nsObject => nsObject -> IO Class
classForKeyedArchiver nsObject  =
    fmap (Class . castPtr) $ sendMsg nsObject (mkSelector "classForKeyedArchiver") (retPtr retVoid) []

-- | @- observationInfo@
observationInfo :: IsNSObject nsObject => nsObject -> IO (Ptr ())
observationInfo nsObject  =
    fmap castPtr $ sendMsg nsObject (mkSelector "observationInfo") (retPtr retVoid) []

-- | @- setObservationInfo:@
setObservationInfo :: IsNSObject nsObject => nsObject -> Ptr () -> IO ()
setObservationInfo nsObject  value =
    sendMsg nsObject (mkSelector "setObservationInfo:") retVoid [argPtr value]

-- | @+ accessInstanceVariablesDirectly@
accessInstanceVariablesDirectly :: IO Bool
accessInstanceVariablesDirectly  =
  do
    cls' <- getRequiredClass "NSObject"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "accessInstanceVariablesDirectly") retCULong []

-- | @- autoContentAccessingProxy@
autoContentAccessingProxy :: IsNSObject nsObject => nsObject -> IO RawId
autoContentAccessingProxy nsObject  =
    fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "autoContentAccessingProxy") (retPtr retVoid) []

-- | @- classForCoder@
classForCoder :: IsNSObject nsObject => nsObject -> IO Class
classForCoder nsObject  =
    fmap (Class . castPtr) $ sendMsg nsObject (mkSelector "classForCoder") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scriptingIsEqualTo:@
scriptingIsEqualToSelector :: Selector
scriptingIsEqualToSelector = mkSelector "scriptingIsEqualTo:"

-- | @Selector@ for @scriptingIsLessThanOrEqualTo:@
scriptingIsLessThanOrEqualToSelector :: Selector
scriptingIsLessThanOrEqualToSelector = mkSelector "scriptingIsLessThanOrEqualTo:"

-- | @Selector@ for @scriptingIsLessThan:@
scriptingIsLessThanSelector :: Selector
scriptingIsLessThanSelector = mkSelector "scriptingIsLessThan:"

-- | @Selector@ for @scriptingIsGreaterThanOrEqualTo:@
scriptingIsGreaterThanOrEqualToSelector :: Selector
scriptingIsGreaterThanOrEqualToSelector = mkSelector "scriptingIsGreaterThanOrEqualTo:"

-- | @Selector@ for @scriptingIsGreaterThan:@
scriptingIsGreaterThanSelector :: Selector
scriptingIsGreaterThanSelector = mkSelector "scriptingIsGreaterThan:"

-- | @Selector@ for @scriptingBeginsWith:@
scriptingBeginsWithSelector :: Selector
scriptingBeginsWithSelector = mkSelector "scriptingBeginsWith:"

-- | @Selector@ for @scriptingEndsWith:@
scriptingEndsWithSelector :: Selector
scriptingEndsWithSelector = mkSelector "scriptingEndsWith:"

-- | @Selector@ for @scriptingContains:@
scriptingContainsSelector :: Selector
scriptingContainsSelector = mkSelector "scriptingContains:"

-- | @Selector@ for @isEqualTo:@
isEqualToSelector :: Selector
isEqualToSelector = mkSelector "isEqualTo:"

-- | @Selector@ for @isLessThanOrEqualTo:@
isLessThanOrEqualToSelector :: Selector
isLessThanOrEqualToSelector = mkSelector "isLessThanOrEqualTo:"

-- | @Selector@ for @isLessThan:@
isLessThanSelector :: Selector
isLessThanSelector = mkSelector "isLessThan:"

-- | @Selector@ for @isGreaterThanOrEqualTo:@
isGreaterThanOrEqualToSelector :: Selector
isGreaterThanOrEqualToSelector = mkSelector "isGreaterThanOrEqualTo:"

-- | @Selector@ for @isGreaterThan:@
isGreaterThanSelector :: Selector
isGreaterThanSelector = mkSelector "isGreaterThan:"

-- | @Selector@ for @isNotEqualTo:@
isNotEqualToSelector :: Selector
isNotEqualToSelector = mkSelector "isNotEqualTo:"

-- | @Selector@ for @doesContain:@
doesContainSelector :: Selector
doesContainSelector = mkSelector "doesContain:"

-- | @Selector@ for @isLike:@
isLikeSelector :: Selector
isLikeSelector = mkSelector "isLike:"

-- | @Selector@ for @isCaseInsensitiveLike:@
isCaseInsensitiveLikeSelector :: Selector
isCaseInsensitiveLikeSelector = mkSelector "isCaseInsensitiveLike:"

-- | @Selector@ for @indicesOfObjectsByEvaluatingObjectSpecifier:@
indicesOfObjectsByEvaluatingObjectSpecifierSelector :: Selector
indicesOfObjectsByEvaluatingObjectSpecifierSelector = mkSelector "indicesOfObjectsByEvaluatingObjectSpecifier:"

-- | @Selector@ for @valueAtIndex:inPropertyWithKey:@
valueAtIndex_inPropertyWithKeySelector :: Selector
valueAtIndex_inPropertyWithKeySelector = mkSelector "valueAtIndex:inPropertyWithKey:"

-- | @Selector@ for @valueWithName:inPropertyWithKey:@
valueWithName_inPropertyWithKeySelector :: Selector
valueWithName_inPropertyWithKeySelector = mkSelector "valueWithName:inPropertyWithKey:"

-- | @Selector@ for @valueWithUniqueID:inPropertyWithKey:@
valueWithUniqueID_inPropertyWithKeySelector :: Selector
valueWithUniqueID_inPropertyWithKeySelector = mkSelector "valueWithUniqueID:inPropertyWithKey:"

-- | @Selector@ for @insertValue:atIndex:inPropertyWithKey:@
insertValue_atIndex_inPropertyWithKeySelector :: Selector
insertValue_atIndex_inPropertyWithKeySelector = mkSelector "insertValue:atIndex:inPropertyWithKey:"

-- | @Selector@ for @removeValueAtIndex:fromPropertyWithKey:@
removeValueAtIndex_fromPropertyWithKeySelector :: Selector
removeValueAtIndex_fromPropertyWithKeySelector = mkSelector "removeValueAtIndex:fromPropertyWithKey:"

-- | @Selector@ for @replaceValueAtIndex:inPropertyWithKey:withValue:@
replaceValueAtIndex_inPropertyWithKey_withValueSelector :: Selector
replaceValueAtIndex_inPropertyWithKey_withValueSelector = mkSelector "replaceValueAtIndex:inPropertyWithKey:withValue:"

-- | @Selector@ for @insertValue:inPropertyWithKey:@
insertValue_inPropertyWithKeySelector :: Selector
insertValue_inPropertyWithKeySelector = mkSelector "insertValue:inPropertyWithKey:"

-- | @Selector@ for @coerceValue:forKey:@
coerceValue_forKeySelector :: Selector
coerceValue_forKeySelector = mkSelector "coerceValue:forKey:"

-- | @Selector@ for @scriptingValueForSpecifier:@
scriptingValueForSpecifierSelector :: Selector
scriptingValueForSpecifierSelector = mkSelector "scriptingValueForSpecifier:"

-- | @Selector@ for @copyScriptingValue:forKey:withProperties:@
copyScriptingValue_forKey_withPropertiesSelector :: Selector
copyScriptingValue_forKey_withPropertiesSelector = mkSelector "copyScriptingValue:forKey:withProperties:"

-- | @Selector@ for @newScriptingObjectOfClass:forValueForKey:withContentsValue:properties:@
newScriptingObjectOfClass_forValueForKey_withContentsValue_propertiesSelector :: Selector
newScriptingObjectOfClass_forValueForKey_withContentsValue_propertiesSelector = mkSelector "newScriptingObjectOfClass:forValueForKey:withContentsValue:properties:"

-- | @Selector@ for @inverseForRelationshipKey:@
inverseForRelationshipKeySelector :: Selector
inverseForRelationshipKeySelector = mkSelector "inverseForRelationshipKey:"

-- | @Selector@ for @replacementObjectForPortCoder:@
replacementObjectForPortCoderSelector :: Selector
replacementObjectForPortCoderSelector = mkSelector "replacementObjectForPortCoder:"

-- | @Selector@ for @replacementObjectForArchiver:@
replacementObjectForArchiverSelector :: Selector
replacementObjectForArchiverSelector = mkSelector "replacementObjectForArchiver:"

-- | @Selector@ for @performSelectorOnMainThread:withObject:waitUntilDone:modes:@
performSelectorOnMainThread_withObject_waitUntilDone_modesSelector :: Selector
performSelectorOnMainThread_withObject_waitUntilDone_modesSelector = mkSelector "performSelectorOnMainThread:withObject:waitUntilDone:modes:"

-- | @Selector@ for @performSelectorOnMainThread:withObject:waitUntilDone:@
performSelectorOnMainThread_withObject_waitUntilDoneSelector :: Selector
performSelectorOnMainThread_withObject_waitUntilDoneSelector = mkSelector "performSelectorOnMainThread:withObject:waitUntilDone:"

-- | @Selector@ for @performSelector:onThread:withObject:waitUntilDone:modes:@
performSelector_onThread_withObject_waitUntilDone_modesSelector :: Selector
performSelector_onThread_withObject_waitUntilDone_modesSelector = mkSelector "performSelector:onThread:withObject:waitUntilDone:modes:"

-- | @Selector@ for @performSelector:onThread:withObject:waitUntilDone:@
performSelector_onThread_withObject_waitUntilDoneSelector :: Selector
performSelector_onThread_withObject_waitUntilDoneSelector = mkSelector "performSelector:onThread:withObject:waitUntilDone:"

-- | @Selector@ for @performSelectorInBackground:withObject:@
performSelectorInBackground_withObjectSelector :: Selector
performSelectorInBackground_withObjectSelector = mkSelector "performSelectorInBackground:withObject:"

-- | @Selector@ for @classForKeyedUnarchiver@
classForKeyedUnarchiverSelector :: Selector
classForKeyedUnarchiverSelector = mkSelector "classForKeyedUnarchiver"

-- | @Selector@ for @replacementObjectForKeyedArchiver:@
replacementObjectForKeyedArchiverSelector :: Selector
replacementObjectForKeyedArchiverSelector = mkSelector "replacementObjectForKeyedArchiver:"

-- | @Selector@ for @classFallbacksForKeyedArchiver@
classFallbacksForKeyedArchiverSelector :: Selector
classFallbacksForKeyedArchiverSelector = mkSelector "classFallbacksForKeyedArchiver"

-- | @Selector@ for @setSharedObservers:@
setSharedObserversSelector :: Selector
setSharedObserversSelector = mkSelector "setSharedObservers:"

-- | @Selector@ for @setKeys:triggerChangeNotificationsForDependentKey:@
setKeys_triggerChangeNotificationsForDependentKeySelector :: Selector
setKeys_triggerChangeNotificationsForDependentKeySelector = mkSelector "setKeys:triggerChangeNotificationsForDependentKey:"

-- | @Selector@ for @keyPathsForValuesAffectingValueForKey:@
keyPathsForValuesAffectingValueForKeySelector :: Selector
keyPathsForValuesAffectingValueForKeySelector = mkSelector "keyPathsForValuesAffectingValueForKey:"

-- | @Selector@ for @automaticallyNotifiesObserversForKey:@
automaticallyNotifiesObserversForKeySelector :: Selector
automaticallyNotifiesObserversForKeySelector = mkSelector "automaticallyNotifiesObserversForKey:"

-- | @Selector@ for @willChangeValueForKey:@
willChangeValueForKeySelector :: Selector
willChangeValueForKeySelector = mkSelector "willChangeValueForKey:"

-- | @Selector@ for @didChangeValueForKey:@
didChangeValueForKeySelector :: Selector
didChangeValueForKeySelector = mkSelector "didChangeValueForKey:"

-- | @Selector@ for @willChange:valuesAtIndexes:forKey:@
willChange_valuesAtIndexes_forKeySelector :: Selector
willChange_valuesAtIndexes_forKeySelector = mkSelector "willChange:valuesAtIndexes:forKey:"

-- | @Selector@ for @didChange:valuesAtIndexes:forKey:@
didChange_valuesAtIndexes_forKeySelector :: Selector
didChange_valuesAtIndexes_forKeySelector = mkSelector "didChange:valuesAtIndexes:forKey:"

-- | @Selector@ for @willChangeValueForKey:withSetMutation:usingObjects:@
willChangeValueForKey_withSetMutation_usingObjectsSelector :: Selector
willChangeValueForKey_withSetMutation_usingObjectsSelector = mkSelector "willChangeValueForKey:withSetMutation:usingObjects:"

-- | @Selector@ for @didChangeValueForKey:withSetMutation:usingObjects:@
didChangeValueForKey_withSetMutation_usingObjectsSelector :: Selector
didChangeValueForKey_withSetMutation_usingObjectsSelector = mkSelector "didChangeValueForKey:withSetMutation:usingObjects:"

-- | @Selector@ for @addObserver:forKeyPath:options:context:@
addObserver_forKeyPath_options_contextSelector :: Selector
addObserver_forKeyPath_options_contextSelector = mkSelector "addObserver:forKeyPath:options:context:"

-- | @Selector@ for @removeObserver:forKeyPath:context:@
removeObserver_forKeyPath_contextSelector :: Selector
removeObserver_forKeyPath_contextSelector = mkSelector "removeObserver:forKeyPath:context:"

-- | @Selector@ for @removeObserver:forKeyPath:@
removeObserver_forKeyPathSelector :: Selector
removeObserver_forKeyPathSelector = mkSelector "removeObserver:forKeyPath:"

-- | @Selector@ for @observeValueForKeyPath:ofObject:change:context:@
observeValueForKeyPath_ofObject_change_contextSelector :: Selector
observeValueForKeyPath_ofObject_change_contextSelector = mkSelector "observeValueForKeyPath:ofObject:change:context:"

-- | @Selector@ for @useStoredAccessor@
useStoredAccessorSelector :: Selector
useStoredAccessorSelector = mkSelector "useStoredAccessor"

-- | @Selector@ for @storedValueForKey:@
storedValueForKeySelector :: Selector
storedValueForKeySelector = mkSelector "storedValueForKey:"

-- | @Selector@ for @takeStoredValue:forKey:@
takeStoredValue_forKeySelector :: Selector
takeStoredValue_forKeySelector = mkSelector "takeStoredValue:forKey:"

-- | @Selector@ for @takeValue:forKey:@
takeValue_forKeySelector :: Selector
takeValue_forKeySelector = mkSelector "takeValue:forKey:"

-- | @Selector@ for @takeValue:forKeyPath:@
takeValue_forKeyPathSelector :: Selector
takeValue_forKeyPathSelector = mkSelector "takeValue:forKeyPath:"

-- | @Selector@ for @handleQueryWithUnboundKey:@
handleQueryWithUnboundKeySelector :: Selector
handleQueryWithUnboundKeySelector = mkSelector "handleQueryWithUnboundKey:"

-- | @Selector@ for @handleTakeValue:forUnboundKey:@
handleTakeValue_forUnboundKeySelector :: Selector
handleTakeValue_forUnboundKeySelector = mkSelector "handleTakeValue:forUnboundKey:"

-- | @Selector@ for @unableToSetNilForKey:@
unableToSetNilForKeySelector :: Selector
unableToSetNilForKeySelector = mkSelector "unableToSetNilForKey:"

-- | @Selector@ for @valuesForKeys:@
valuesForKeysSelector :: Selector
valuesForKeysSelector = mkSelector "valuesForKeys:"

-- | @Selector@ for @takeValuesFromDictionary:@
takeValuesFromDictionarySelector :: Selector
takeValuesFromDictionarySelector = mkSelector "takeValuesFromDictionary:"

-- | @Selector@ for @valueForKey:@
valueForKeySelector :: Selector
valueForKeySelector = mkSelector "valueForKey:"

-- | @Selector@ for @setValue:forKey:@
setValue_forKeySelector :: Selector
setValue_forKeySelector = mkSelector "setValue:forKey:"

-- | @Selector@ for @validateValue:forKey:error:@
validateValue_forKey_errorSelector :: Selector
validateValue_forKey_errorSelector = mkSelector "validateValue:forKey:error:"

-- | @Selector@ for @mutableArrayValueForKey:@
mutableArrayValueForKeySelector :: Selector
mutableArrayValueForKeySelector = mkSelector "mutableArrayValueForKey:"

-- | @Selector@ for @mutableOrderedSetValueForKey:@
mutableOrderedSetValueForKeySelector :: Selector
mutableOrderedSetValueForKeySelector = mkSelector "mutableOrderedSetValueForKey:"

-- | @Selector@ for @mutableSetValueForKey:@
mutableSetValueForKeySelector :: Selector
mutableSetValueForKeySelector = mkSelector "mutableSetValueForKey:"

-- | @Selector@ for @valueForKeyPath:@
valueForKeyPathSelector :: Selector
valueForKeyPathSelector = mkSelector "valueForKeyPath:"

-- | @Selector@ for @setValue:forKeyPath:@
setValue_forKeyPathSelector :: Selector
setValue_forKeyPathSelector = mkSelector "setValue:forKeyPath:"

-- | @Selector@ for @validateValue:forKeyPath:error:@
validateValue_forKeyPath_errorSelector :: Selector
validateValue_forKeyPath_errorSelector = mkSelector "validateValue:forKeyPath:error:"

-- | @Selector@ for @mutableArrayValueForKeyPath:@
mutableArrayValueForKeyPathSelector :: Selector
mutableArrayValueForKeyPathSelector = mkSelector "mutableArrayValueForKeyPath:"

-- | @Selector@ for @mutableOrderedSetValueForKeyPath:@
mutableOrderedSetValueForKeyPathSelector :: Selector
mutableOrderedSetValueForKeyPathSelector = mkSelector "mutableOrderedSetValueForKeyPath:"

-- | @Selector@ for @mutableSetValueForKeyPath:@
mutableSetValueForKeyPathSelector :: Selector
mutableSetValueForKeyPathSelector = mkSelector "mutableSetValueForKeyPath:"

-- | @Selector@ for @valueForUndefinedKey:@
valueForUndefinedKeySelector :: Selector
valueForUndefinedKeySelector = mkSelector "valueForUndefinedKey:"

-- | @Selector@ for @setValue:forUndefinedKey:@
setValue_forUndefinedKeySelector :: Selector
setValue_forUndefinedKeySelector = mkSelector "setValue:forUndefinedKey:"

-- | @Selector@ for @setNilValueForKey:@
setNilValueForKeySelector :: Selector
setNilValueForKeySelector = mkSelector "setNilValueForKey:"

-- | @Selector@ for @dictionaryWithValuesForKeys:@
dictionaryWithValuesForKeysSelector :: Selector
dictionaryWithValuesForKeysSelector = mkSelector "dictionaryWithValuesForKeys:"

-- | @Selector@ for @setValuesForKeysWithDictionary:@
setValuesForKeysWithDictionarySelector :: Selector
setValuesForKeysWithDictionarySelector = mkSelector "setValuesForKeysWithDictionary:"

-- | @Selector@ for @fileManager:shouldProceedAfterError:@
fileManager_shouldProceedAfterErrorSelector :: Selector
fileManager_shouldProceedAfterErrorSelector = mkSelector "fileManager:shouldProceedAfterError:"

-- | @Selector@ for @fileManager:willProcessPath:@
fileManager_willProcessPathSelector :: Selector
fileManager_willProcessPathSelector = mkSelector "fileManager:willProcessPath:"

-- | @Selector@ for @URL:resourceDataDidBecomeAvailable:@
urL_resourceDataDidBecomeAvailableSelector :: Selector
urL_resourceDataDidBecomeAvailableSelector = mkSelector "URL:resourceDataDidBecomeAvailable:"

-- | @Selector@ for @URLResourceDidFinishLoading:@
urlResourceDidFinishLoadingSelector :: Selector
urlResourceDidFinishLoadingSelector = mkSelector "URLResourceDidFinishLoading:"

-- | @Selector@ for @URLResourceDidCancelLoading:@
urlResourceDidCancelLoadingSelector :: Selector
urlResourceDidCancelLoadingSelector = mkSelector "URLResourceDidCancelLoading:"

-- | @Selector@ for @URL:resourceDidFailLoadingWithReason:@
urL_resourceDidFailLoadingWithReasonSelector :: Selector
urL_resourceDidFailLoadingWithReasonSelector = mkSelector "URL:resourceDidFailLoadingWithReason:"

-- | @Selector@ for @performSelector:withObject:afterDelay:inModes:@
performSelector_withObject_afterDelay_inModesSelector :: Selector
performSelector_withObject_afterDelay_inModesSelector = mkSelector "performSelector:withObject:afterDelay:inModes:"

-- | @Selector@ for @performSelector:withObject:afterDelay:@
performSelector_withObject_afterDelaySelector :: Selector
performSelector_withObject_afterDelaySelector = mkSelector "performSelector:withObject:afterDelay:"

-- | @Selector@ for @cancelPreviousPerformRequestsWithTarget:selector:object:@
cancelPreviousPerformRequestsWithTarget_selector_objectSelector :: Selector
cancelPreviousPerformRequestsWithTarget_selector_objectSelector = mkSelector "cancelPreviousPerformRequestsWithTarget:selector:object:"

-- | @Selector@ for @cancelPreviousPerformRequestsWithTarget:@
cancelPreviousPerformRequestsWithTargetSelector :: Selector
cancelPreviousPerformRequestsWithTargetSelector = mkSelector "cancelPreviousPerformRequestsWithTarget:"

-- | @Selector@ for @attemptRecoveryFromError:optionIndex:delegate:didRecoverSelector:contextInfo:@
attemptRecoveryFromError_optionIndex_delegate_didRecoverSelector_contextInfoSelector :: Selector
attemptRecoveryFromError_optionIndex_delegate_didRecoverSelector_contextInfoSelector = mkSelector "attemptRecoveryFromError:optionIndex:delegate:didRecoverSelector:contextInfo:"

-- | @Selector@ for @attemptRecoveryFromError:optionIndex:@
attemptRecoveryFromError_optionIndexSelector :: Selector
attemptRecoveryFromError_optionIndexSelector = mkSelector "attemptRecoveryFromError:optionIndex:"

-- | @Selector@ for @poseAsClass:@
poseAsClassSelector :: Selector
poseAsClassSelector = mkSelector "poseAsClass:"

-- | @Selector@ for @version@
versionSelector :: Selector
versionSelector = mkSelector "version"

-- | @Selector@ for @setVersion:@
setVersionSelector :: Selector
setVersionSelector = mkSelector "setVersion:"

-- | @Selector@ for @replacementObjectForCoder:@
replacementObjectForCoderSelector :: Selector
replacementObjectForCoderSelector = mkSelector "replacementObjectForCoder:"

-- | @Selector@ for @awakeAfterUsingCoder:@
awakeAfterUsingCoderSelector :: Selector
awakeAfterUsingCoderSelector = mkSelector "awakeAfterUsingCoder:"

-- | @Selector@ for @objectSpecifier@
objectSpecifierSelector :: Selector
objectSpecifierSelector = mkSelector "objectSpecifier"

-- | @Selector@ for @classCode@
classCodeSelector :: Selector
classCodeSelector = mkSelector "classCode"

-- | @Selector@ for @className@
classNameSelector :: Selector
classNameSelector = mkSelector "className"

-- | @Selector@ for @scriptingProperties@
scriptingPropertiesSelector :: Selector
scriptingPropertiesSelector = mkSelector "scriptingProperties"

-- | @Selector@ for @setScriptingProperties:@
setScriptingPropertiesSelector :: Selector
setScriptingPropertiesSelector = mkSelector "setScriptingProperties:"

-- | @Selector@ for @classDescription@
classDescriptionSelector :: Selector
classDescriptionSelector = mkSelector "classDescription"

-- | @Selector@ for @attributeKeys@
attributeKeysSelector :: Selector
attributeKeysSelector = mkSelector "attributeKeys"

-- | @Selector@ for @toOneRelationshipKeys@
toOneRelationshipKeysSelector :: Selector
toOneRelationshipKeysSelector = mkSelector "toOneRelationshipKeys"

-- | @Selector@ for @toManyRelationshipKeys@
toManyRelationshipKeysSelector :: Selector
toManyRelationshipKeysSelector = mkSelector "toManyRelationshipKeys"

-- | @Selector@ for @classForPortCoder@
classForPortCoderSelector :: Selector
classForPortCoderSelector = mkSelector "classForPortCoder"

-- | @Selector@ for @classForArchiver@
classForArchiverSelector :: Selector
classForArchiverSelector = mkSelector "classForArchiver"

-- | @Selector@ for @classForKeyedArchiver@
classForKeyedArchiverSelector :: Selector
classForKeyedArchiverSelector = mkSelector "classForKeyedArchiver"

-- | @Selector@ for @observationInfo@
observationInfoSelector :: Selector
observationInfoSelector = mkSelector "observationInfo"

-- | @Selector@ for @setObservationInfo:@
setObservationInfoSelector :: Selector
setObservationInfoSelector = mkSelector "setObservationInfo:"

-- | @Selector@ for @accessInstanceVariablesDirectly@
accessInstanceVariablesDirectlySelector :: Selector
accessInstanceVariablesDirectlySelector = mkSelector "accessInstanceVariablesDirectly"

-- | @Selector@ for @autoContentAccessingProxy@
autoContentAccessingProxySelector :: Selector
autoContentAccessingProxySelector = mkSelector "autoContentAccessingProxy"

-- | @Selector@ for @classForCoder@
classForCoderSelector :: Selector
classForCoderSelector = mkSelector "classForCoder"

