{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , accessInstanceVariablesDirectlySelector
  , addObserver_forKeyPath_options_contextSelector
  , attemptRecoveryFromError_optionIndexSelector
  , attemptRecoveryFromError_optionIndex_delegate_didRecoverSelector_contextInfoSelector
  , attributeKeysSelector
  , autoContentAccessingProxySelector
  , automaticallyNotifiesObserversForKeySelector
  , awakeAfterUsingCoderSelector
  , cancelPreviousPerformRequestsWithTargetSelector
  , cancelPreviousPerformRequestsWithTarget_selector_objectSelector
  , classCodeSelector
  , classDescriptionSelector
  , classFallbacksForKeyedArchiverSelector
  , classForArchiverSelector
  , classForCoderSelector
  , classForKeyedArchiverSelector
  , classForKeyedUnarchiverSelector
  , classForPortCoderSelector
  , classNameSelector
  , coerceValue_forKeySelector
  , copyScriptingValue_forKey_withPropertiesSelector
  , dictionaryWithValuesForKeysSelector
  , didChangeValueForKeySelector
  , didChangeValueForKey_withSetMutation_usingObjectsSelector
  , didChange_valuesAtIndexes_forKeySelector
  , doesContainSelector
  , fileManager_shouldProceedAfterErrorSelector
  , fileManager_willProcessPathSelector
  , handleQueryWithUnboundKeySelector
  , handleTakeValue_forUnboundKeySelector
  , indicesOfObjectsByEvaluatingObjectSpecifierSelector
  , insertValue_atIndex_inPropertyWithKeySelector
  , insertValue_inPropertyWithKeySelector
  , inverseForRelationshipKeySelector
  , isCaseInsensitiveLikeSelector
  , isEqualToSelector
  , isGreaterThanOrEqualToSelector
  , isGreaterThanSelector
  , isLessThanOrEqualToSelector
  , isLessThanSelector
  , isLikeSelector
  , isNotEqualToSelector
  , keyPathsForValuesAffectingValueForKeySelector
  , mutableArrayValueForKeyPathSelector
  , mutableArrayValueForKeySelector
  , mutableOrderedSetValueForKeyPathSelector
  , mutableOrderedSetValueForKeySelector
  , mutableSetValueForKeyPathSelector
  , mutableSetValueForKeySelector
  , newScriptingObjectOfClass_forValueForKey_withContentsValue_propertiesSelector
  , objectSpecifierSelector
  , observationInfoSelector
  , observeValueForKeyPath_ofObject_change_contextSelector
  , performSelectorInBackground_withObjectSelector
  , performSelectorOnMainThread_withObject_waitUntilDoneSelector
  , performSelectorOnMainThread_withObject_waitUntilDone_modesSelector
  , performSelector_onThread_withObject_waitUntilDoneSelector
  , performSelector_onThread_withObject_waitUntilDone_modesSelector
  , performSelector_withObject_afterDelaySelector
  , performSelector_withObject_afterDelay_inModesSelector
  , poseAsClassSelector
  , removeObserver_forKeyPathSelector
  , removeObserver_forKeyPath_contextSelector
  , removeValueAtIndex_fromPropertyWithKeySelector
  , replaceValueAtIndex_inPropertyWithKey_withValueSelector
  , replacementObjectForArchiverSelector
  , replacementObjectForCoderSelector
  , replacementObjectForKeyedArchiverSelector
  , replacementObjectForPortCoderSelector
  , scriptingBeginsWithSelector
  , scriptingContainsSelector
  , scriptingEndsWithSelector
  , scriptingIsEqualToSelector
  , scriptingIsGreaterThanOrEqualToSelector
  , scriptingIsGreaterThanSelector
  , scriptingIsLessThanOrEqualToSelector
  , scriptingIsLessThanSelector
  , scriptingPropertiesSelector
  , scriptingValueForSpecifierSelector
  , setKeys_triggerChangeNotificationsForDependentKeySelector
  , setNilValueForKeySelector
  , setObservationInfoSelector
  , setScriptingPropertiesSelector
  , setSharedObserversSelector
  , setValue_forKeyPathSelector
  , setValue_forKeySelector
  , setValue_forUndefinedKeySelector
  , setValuesForKeysWithDictionarySelector
  , setVersionSelector
  , storedValueForKeySelector
  , takeStoredValue_forKeySelector
  , takeValue_forKeyPathSelector
  , takeValue_forKeySelector
  , takeValuesFromDictionarySelector
  , toManyRelationshipKeysSelector
  , toOneRelationshipKeysSelector
  , unableToSetNilForKeySelector
  , urL_resourceDataDidBecomeAvailableSelector
  , urL_resourceDidFailLoadingWithReasonSelector
  , urlResourceDidCancelLoadingSelector
  , urlResourceDidFinishLoadingSelector
  , useStoredAccessorSelector
  , validateValue_forKeyPath_errorSelector
  , validateValue_forKey_errorSelector
  , valueAtIndex_inPropertyWithKeySelector
  , valueForKeyPathSelector
  , valueForKeySelector
  , valueForUndefinedKeySelector
  , valueWithName_inPropertyWithKeySelector
  , valueWithUniqueID_inPropertyWithKeySelector
  , valuesForKeysSelector
  , versionSelector
  , willChangeValueForKeySelector
  , willChangeValueForKey_withSetMutation_usingObjectsSelector
  , willChange_valuesAtIndexes_forKeySelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- scriptingIsEqualTo:@
scriptingIsEqualTo :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
scriptingIsEqualTo nsObject object =
  sendMessage nsObject scriptingIsEqualToSelector object

-- | @- scriptingIsLessThanOrEqualTo:@
scriptingIsLessThanOrEqualTo :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
scriptingIsLessThanOrEqualTo nsObject object =
  sendMessage nsObject scriptingIsLessThanOrEqualToSelector object

-- | @- scriptingIsLessThan:@
scriptingIsLessThan :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
scriptingIsLessThan nsObject object =
  sendMessage nsObject scriptingIsLessThanSelector object

-- | @- scriptingIsGreaterThanOrEqualTo:@
scriptingIsGreaterThanOrEqualTo :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
scriptingIsGreaterThanOrEqualTo nsObject object =
  sendMessage nsObject scriptingIsGreaterThanOrEqualToSelector object

-- | @- scriptingIsGreaterThan:@
scriptingIsGreaterThan :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
scriptingIsGreaterThan nsObject object =
  sendMessage nsObject scriptingIsGreaterThanSelector object

-- | @- scriptingBeginsWith:@
scriptingBeginsWith :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
scriptingBeginsWith nsObject object =
  sendMessage nsObject scriptingBeginsWithSelector object

-- | @- scriptingEndsWith:@
scriptingEndsWith :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
scriptingEndsWith nsObject object =
  sendMessage nsObject scriptingEndsWithSelector object

-- | @- scriptingContains:@
scriptingContains :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
scriptingContains nsObject object =
  sendMessage nsObject scriptingContainsSelector object

-- | @- isEqualTo:@
isEqualTo :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
isEqualTo nsObject object =
  sendMessage nsObject isEqualToSelector object

-- | @- isLessThanOrEqualTo:@
isLessThanOrEqualTo :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
isLessThanOrEqualTo nsObject object =
  sendMessage nsObject isLessThanOrEqualToSelector object

-- | @- isLessThan:@
isLessThan :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
isLessThan nsObject object =
  sendMessage nsObject isLessThanSelector object

-- | @- isGreaterThanOrEqualTo:@
isGreaterThanOrEqualTo :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
isGreaterThanOrEqualTo nsObject object =
  sendMessage nsObject isGreaterThanOrEqualToSelector object

-- | @- isGreaterThan:@
isGreaterThan :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
isGreaterThan nsObject object =
  sendMessage nsObject isGreaterThanSelector object

-- | @- isNotEqualTo:@
isNotEqualTo :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
isNotEqualTo nsObject object =
  sendMessage nsObject isNotEqualToSelector object

-- | @- doesContain:@
doesContain :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
doesContain nsObject object =
  sendMessage nsObject doesContainSelector object

-- | @- isLike:@
isLike :: (IsNSObject nsObject, IsNSString object) => nsObject -> object -> IO Bool
isLike nsObject object =
  sendMessage nsObject isLikeSelector (toNSString object)

-- | @- isCaseInsensitiveLike:@
isCaseInsensitiveLike :: (IsNSObject nsObject, IsNSString object) => nsObject -> object -> IO Bool
isCaseInsensitiveLike nsObject object =
  sendMessage nsObject isCaseInsensitiveLikeSelector (toNSString object)

-- | @- indicesOfObjectsByEvaluatingObjectSpecifier:@
indicesOfObjectsByEvaluatingObjectSpecifier :: (IsNSObject nsObject, IsNSScriptObjectSpecifier specifier) => nsObject -> specifier -> IO (Id NSArray)
indicesOfObjectsByEvaluatingObjectSpecifier nsObject specifier =
  sendMessage nsObject indicesOfObjectsByEvaluatingObjectSpecifierSelector (toNSScriptObjectSpecifier specifier)

-- | @- valueAtIndex:inPropertyWithKey:@
valueAtIndex_inPropertyWithKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> CULong -> key -> IO RawId
valueAtIndex_inPropertyWithKey nsObject index key =
  sendMessage nsObject valueAtIndex_inPropertyWithKeySelector index (toNSString key)

-- | @- valueWithName:inPropertyWithKey:@
valueWithName_inPropertyWithKey :: (IsNSObject nsObject, IsNSString name, IsNSString key) => nsObject -> name -> key -> IO RawId
valueWithName_inPropertyWithKey nsObject name key =
  sendMessage nsObject valueWithName_inPropertyWithKeySelector (toNSString name) (toNSString key)

-- | @- valueWithUniqueID:inPropertyWithKey:@
valueWithUniqueID_inPropertyWithKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> RawId -> key -> IO RawId
valueWithUniqueID_inPropertyWithKey nsObject uniqueID key =
  sendMessage nsObject valueWithUniqueID_inPropertyWithKeySelector uniqueID (toNSString key)

-- | @- insertValue:atIndex:inPropertyWithKey:@
insertValue_atIndex_inPropertyWithKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> RawId -> CULong -> key -> IO ()
insertValue_atIndex_inPropertyWithKey nsObject value index key =
  sendMessage nsObject insertValue_atIndex_inPropertyWithKeySelector value index (toNSString key)

-- | @- removeValueAtIndex:fromPropertyWithKey:@
removeValueAtIndex_fromPropertyWithKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> CULong -> key -> IO ()
removeValueAtIndex_fromPropertyWithKey nsObject index key =
  sendMessage nsObject removeValueAtIndex_fromPropertyWithKeySelector index (toNSString key)

-- | @- replaceValueAtIndex:inPropertyWithKey:withValue:@
replaceValueAtIndex_inPropertyWithKey_withValue :: (IsNSObject nsObject, IsNSString key) => nsObject -> CULong -> key -> RawId -> IO ()
replaceValueAtIndex_inPropertyWithKey_withValue nsObject index key value =
  sendMessage nsObject replaceValueAtIndex_inPropertyWithKey_withValueSelector index (toNSString key) value

-- | @- insertValue:inPropertyWithKey:@
insertValue_inPropertyWithKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> RawId -> key -> IO ()
insertValue_inPropertyWithKey nsObject value key =
  sendMessage nsObject insertValue_inPropertyWithKeySelector value (toNSString key)

-- | @- coerceValue:forKey:@
coerceValue_forKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> RawId -> key -> IO RawId
coerceValue_forKey nsObject value key =
  sendMessage nsObject coerceValue_forKeySelector value (toNSString key)

-- | @- scriptingValueForSpecifier:@
scriptingValueForSpecifier :: (IsNSObject nsObject, IsNSScriptObjectSpecifier objectSpecifier) => nsObject -> objectSpecifier -> IO RawId
scriptingValueForSpecifier nsObject objectSpecifier =
  sendMessage nsObject scriptingValueForSpecifierSelector (toNSScriptObjectSpecifier objectSpecifier)

-- | @- copyScriptingValue:forKey:withProperties:@
copyScriptingValue_forKey_withProperties :: (IsNSObject nsObject, IsNSString key, IsNSDictionary properties) => nsObject -> RawId -> key -> properties -> IO RawId
copyScriptingValue_forKey_withProperties nsObject value key properties =
  sendOwnedMessage nsObject copyScriptingValue_forKey_withPropertiesSelector value (toNSString key) (toNSDictionary properties)

-- | @- newScriptingObjectOfClass:forValueForKey:withContentsValue:properties:@
newScriptingObjectOfClass_forValueForKey_withContentsValue_properties :: (IsNSObject nsObject, IsNSString key, IsNSDictionary properties) => nsObject -> Class -> key -> RawId -> properties -> IO RawId
newScriptingObjectOfClass_forValueForKey_withContentsValue_properties nsObject objectClass key contentsValue properties =
  sendOwnedMessage nsObject newScriptingObjectOfClass_forValueForKey_withContentsValue_propertiesSelector objectClass (toNSString key) contentsValue (toNSDictionary properties)

-- | @- inverseForRelationshipKey:@
inverseForRelationshipKey :: (IsNSObject nsObject, IsNSString relationshipKey) => nsObject -> relationshipKey -> IO (Id NSString)
inverseForRelationshipKey nsObject relationshipKey =
  sendMessage nsObject inverseForRelationshipKeySelector (toNSString relationshipKey)

-- | @- replacementObjectForPortCoder:@
replacementObjectForPortCoder :: (IsNSObject nsObject, IsNSPortCoder coder) => nsObject -> coder -> IO RawId
replacementObjectForPortCoder nsObject coder =
  sendMessage nsObject replacementObjectForPortCoderSelector (toNSPortCoder coder)

-- | @- replacementObjectForArchiver:@
replacementObjectForArchiver :: (IsNSObject nsObject, IsNSArchiver archiver) => nsObject -> archiver -> IO RawId
replacementObjectForArchiver nsObject archiver =
  sendMessage nsObject replacementObjectForArchiverSelector (toNSArchiver archiver)

-- | @- performSelectorOnMainThread:withObject:waitUntilDone:modes:@
performSelectorOnMainThread_withObject_waitUntilDone_modes :: (IsNSObject nsObject, IsNSArray array) => nsObject -> Sel -> RawId -> Bool -> array -> IO ()
performSelectorOnMainThread_withObject_waitUntilDone_modes nsObject aSelector arg wait array =
  sendMessage nsObject performSelectorOnMainThread_withObject_waitUntilDone_modesSelector aSelector arg wait (toNSArray array)

-- | @- performSelectorOnMainThread:withObject:waitUntilDone:@
performSelectorOnMainThread_withObject_waitUntilDone :: IsNSObject nsObject => nsObject -> Sel -> RawId -> Bool -> IO ()
performSelectorOnMainThread_withObject_waitUntilDone nsObject aSelector arg wait =
  sendMessage nsObject performSelectorOnMainThread_withObject_waitUntilDoneSelector aSelector arg wait

-- | @- performSelector:onThread:withObject:waitUntilDone:modes:@
performSelector_onThread_withObject_waitUntilDone_modes :: (IsNSObject nsObject, IsNSThread thr, IsNSArray array) => nsObject -> Sel -> thr -> RawId -> Bool -> array -> IO ()
performSelector_onThread_withObject_waitUntilDone_modes nsObject aSelector thr arg wait array =
  sendMessage nsObject performSelector_onThread_withObject_waitUntilDone_modesSelector aSelector (toNSThread thr) arg wait (toNSArray array)

-- | @- performSelector:onThread:withObject:waitUntilDone:@
performSelector_onThread_withObject_waitUntilDone :: (IsNSObject nsObject, IsNSThread thr) => nsObject -> Sel -> thr -> RawId -> Bool -> IO ()
performSelector_onThread_withObject_waitUntilDone nsObject aSelector thr arg wait =
  sendMessage nsObject performSelector_onThread_withObject_waitUntilDoneSelector aSelector (toNSThread thr) arg wait

-- | @- performSelectorInBackground:withObject:@
performSelectorInBackground_withObject :: IsNSObject nsObject => nsObject -> Sel -> RawId -> IO ()
performSelectorInBackground_withObject nsObject aSelector arg =
  sendMessage nsObject performSelectorInBackground_withObjectSelector aSelector arg

-- | @+ classForKeyedUnarchiver@
classForKeyedUnarchiver :: IO Class
classForKeyedUnarchiver  =
  do
    cls' <- getRequiredClass "NSObject"
    sendClassMessage cls' classForKeyedUnarchiverSelector

-- | @- replacementObjectForKeyedArchiver:@
replacementObjectForKeyedArchiver :: (IsNSObject nsObject, IsNSKeyedArchiver archiver) => nsObject -> archiver -> IO RawId
replacementObjectForKeyedArchiver nsObject archiver =
  sendMessage nsObject replacementObjectForKeyedArchiverSelector (toNSKeyedArchiver archiver)

-- | @+ classFallbacksForKeyedArchiver@
classFallbacksForKeyedArchiver :: IO (Id NSArray)
classFallbacksForKeyedArchiver  =
  do
    cls' <- getRequiredClass "NSObject"
    sendClassMessage cls' classFallbacksForKeyedArchiverSelector

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
setSharedObservers nsObject sharedObservers =
  sendMessage nsObject setSharedObserversSelector (toNSKeyValueSharedObserversSnapshot sharedObservers)

-- | @+ setKeys:triggerChangeNotificationsForDependentKey:@
setKeys_triggerChangeNotificationsForDependentKey :: (IsNSArray keys, IsNSString dependentKey) => keys -> dependentKey -> IO ()
setKeys_triggerChangeNotificationsForDependentKey keys dependentKey =
  do
    cls' <- getRequiredClass "NSObject"
    sendClassMessage cls' setKeys_triggerChangeNotificationsForDependentKeySelector (toNSArray keys) (toNSString dependentKey)

-- | @+ keyPathsForValuesAffectingValueForKey:@
keyPathsForValuesAffectingValueForKey :: IsNSString key => key -> IO (Id NSSet)
keyPathsForValuesAffectingValueForKey key =
  do
    cls' <- getRequiredClass "NSObject"
    sendClassMessage cls' keyPathsForValuesAffectingValueForKeySelector (toNSString key)

-- | @+ automaticallyNotifiesObserversForKey:@
automaticallyNotifiesObserversForKey :: IsNSString key => key -> IO Bool
automaticallyNotifiesObserversForKey key =
  do
    cls' <- getRequiredClass "NSObject"
    sendClassMessage cls' automaticallyNotifiesObserversForKeySelector (toNSString key)

-- | @- willChangeValueForKey:@
willChangeValueForKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> key -> IO ()
willChangeValueForKey nsObject key =
  sendMessage nsObject willChangeValueForKeySelector (toNSString key)

-- | @- didChangeValueForKey:@
didChangeValueForKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> key -> IO ()
didChangeValueForKey nsObject key =
  sendMessage nsObject didChangeValueForKeySelector (toNSString key)

-- | @- willChange:valuesAtIndexes:forKey:@
willChange_valuesAtIndexes_forKey :: (IsNSObject nsObject, IsNSIndexSet indexes, IsNSString key) => nsObject -> NSKeyValueChange -> indexes -> key -> IO ()
willChange_valuesAtIndexes_forKey nsObject changeKind indexes key =
  sendMessage nsObject willChange_valuesAtIndexes_forKeySelector changeKind (toNSIndexSet indexes) (toNSString key)

-- | @- didChange:valuesAtIndexes:forKey:@
didChange_valuesAtIndexes_forKey :: (IsNSObject nsObject, IsNSIndexSet indexes, IsNSString key) => nsObject -> NSKeyValueChange -> indexes -> key -> IO ()
didChange_valuesAtIndexes_forKey nsObject changeKind indexes key =
  sendMessage nsObject didChange_valuesAtIndexes_forKeySelector changeKind (toNSIndexSet indexes) (toNSString key)

-- | @- willChangeValueForKey:withSetMutation:usingObjects:@
willChangeValueForKey_withSetMutation_usingObjects :: (IsNSObject nsObject, IsNSString key, IsNSSet objects) => nsObject -> key -> NSKeyValueSetMutationKind -> objects -> IO ()
willChangeValueForKey_withSetMutation_usingObjects nsObject key mutationKind objects =
  sendMessage nsObject willChangeValueForKey_withSetMutation_usingObjectsSelector (toNSString key) mutationKind (toNSSet objects)

-- | @- didChangeValueForKey:withSetMutation:usingObjects:@
didChangeValueForKey_withSetMutation_usingObjects :: (IsNSObject nsObject, IsNSString key, IsNSSet objects) => nsObject -> key -> NSKeyValueSetMutationKind -> objects -> IO ()
didChangeValueForKey_withSetMutation_usingObjects nsObject key mutationKind objects =
  sendMessage nsObject didChangeValueForKey_withSetMutation_usingObjectsSelector (toNSString key) mutationKind (toNSSet objects)

-- | @- addObserver:forKeyPath:options:context:@
addObserver_forKeyPath_options_context :: (IsNSObject nsObject, IsNSObject observer, IsNSString keyPath) => nsObject -> observer -> keyPath -> NSKeyValueObservingOptions -> Ptr () -> IO ()
addObserver_forKeyPath_options_context nsObject observer keyPath options context =
  sendMessage nsObject addObserver_forKeyPath_options_contextSelector (toNSObject observer) (toNSString keyPath) options context

-- | @- removeObserver:forKeyPath:context:@
removeObserver_forKeyPath_context :: (IsNSObject nsObject, IsNSObject observer, IsNSString keyPath) => nsObject -> observer -> keyPath -> Ptr () -> IO ()
removeObserver_forKeyPath_context nsObject observer keyPath context =
  sendMessage nsObject removeObserver_forKeyPath_contextSelector (toNSObject observer) (toNSString keyPath) context

-- | @- removeObserver:forKeyPath:@
removeObserver_forKeyPath :: (IsNSObject nsObject, IsNSObject observer, IsNSString keyPath) => nsObject -> observer -> keyPath -> IO ()
removeObserver_forKeyPath nsObject observer keyPath =
  sendMessage nsObject removeObserver_forKeyPathSelector (toNSObject observer) (toNSString keyPath)

-- | @- observeValueForKeyPath:ofObject:change:context:@
observeValueForKeyPath_ofObject_change_context :: (IsNSObject nsObject, IsNSString keyPath, IsNSDictionary change) => nsObject -> keyPath -> RawId -> change -> Ptr () -> IO ()
observeValueForKeyPath_ofObject_change_context nsObject keyPath object change context =
  sendMessage nsObject observeValueForKeyPath_ofObject_change_contextSelector (toNSString keyPath) object (toNSDictionary change) context

-- | @+ useStoredAccessor@
useStoredAccessor :: IO Bool
useStoredAccessor  =
  do
    cls' <- getRequiredClass "NSObject"
    sendClassMessage cls' useStoredAccessorSelector

-- | @- storedValueForKey:@
storedValueForKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> key -> IO RawId
storedValueForKey nsObject key =
  sendMessage nsObject storedValueForKeySelector (toNSString key)

-- | @- takeStoredValue:forKey:@
takeStoredValue_forKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> RawId -> key -> IO ()
takeStoredValue_forKey nsObject value key =
  sendMessage nsObject takeStoredValue_forKeySelector value (toNSString key)

-- | @- takeValue:forKey:@
takeValue_forKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> RawId -> key -> IO ()
takeValue_forKey nsObject value key =
  sendMessage nsObject takeValue_forKeySelector value (toNSString key)

-- | @- takeValue:forKeyPath:@
takeValue_forKeyPath :: (IsNSObject nsObject, IsNSString keyPath) => nsObject -> RawId -> keyPath -> IO ()
takeValue_forKeyPath nsObject value keyPath =
  sendMessage nsObject takeValue_forKeyPathSelector value (toNSString keyPath)

-- | @- handleQueryWithUnboundKey:@
handleQueryWithUnboundKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> key -> IO RawId
handleQueryWithUnboundKey nsObject key =
  sendMessage nsObject handleQueryWithUnboundKeySelector (toNSString key)

-- | @- handleTakeValue:forUnboundKey:@
handleTakeValue_forUnboundKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> RawId -> key -> IO ()
handleTakeValue_forUnboundKey nsObject value key =
  sendMessage nsObject handleTakeValue_forUnboundKeySelector value (toNSString key)

-- | @- unableToSetNilForKey:@
unableToSetNilForKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> key -> IO ()
unableToSetNilForKey nsObject key =
  sendMessage nsObject unableToSetNilForKeySelector (toNSString key)

-- | @- valuesForKeys:@
valuesForKeys :: (IsNSObject nsObject, IsNSArray keys) => nsObject -> keys -> IO (Id NSDictionary)
valuesForKeys nsObject keys =
  sendMessage nsObject valuesForKeysSelector (toNSArray keys)

-- | @- takeValuesFromDictionary:@
takeValuesFromDictionary :: (IsNSObject nsObject, IsNSDictionary properties) => nsObject -> properties -> IO ()
takeValuesFromDictionary nsObject properties =
  sendMessage nsObject takeValuesFromDictionarySelector (toNSDictionary properties)

-- | @- valueForKey:@
valueForKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> key -> IO RawId
valueForKey nsObject key =
  sendMessage nsObject valueForKeySelector (toNSString key)

-- | @- setValue:forKey:@
setValue_forKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> RawId -> key -> IO ()
setValue_forKey nsObject value key =
  sendMessage nsObject setValue_forKeySelector value (toNSString key)

-- | @- validateValue:forKey:error:@
validateValue_forKey_error :: (IsNSObject nsObject, IsNSString inKey, IsNSError outError) => nsObject -> Ptr RawId -> inKey -> outError -> IO Bool
validateValue_forKey_error nsObject ioValue inKey outError =
  sendMessage nsObject validateValue_forKey_errorSelector ioValue (toNSString inKey) (toNSError outError)

-- | @- mutableArrayValueForKey:@
mutableArrayValueForKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> key -> IO (Id NSMutableArray)
mutableArrayValueForKey nsObject key =
  sendMessage nsObject mutableArrayValueForKeySelector (toNSString key)

-- | @- mutableOrderedSetValueForKey:@
mutableOrderedSetValueForKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> key -> IO (Id NSMutableOrderedSet)
mutableOrderedSetValueForKey nsObject key =
  sendMessage nsObject mutableOrderedSetValueForKeySelector (toNSString key)

-- | @- mutableSetValueForKey:@
mutableSetValueForKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> key -> IO (Id NSMutableSet)
mutableSetValueForKey nsObject key =
  sendMessage nsObject mutableSetValueForKeySelector (toNSString key)

-- | @- valueForKeyPath:@
valueForKeyPath :: (IsNSObject nsObject, IsNSString keyPath) => nsObject -> keyPath -> IO RawId
valueForKeyPath nsObject keyPath =
  sendMessage nsObject valueForKeyPathSelector (toNSString keyPath)

-- | @- setValue:forKeyPath:@
setValue_forKeyPath :: (IsNSObject nsObject, IsNSString keyPath) => nsObject -> RawId -> keyPath -> IO ()
setValue_forKeyPath nsObject value keyPath =
  sendMessage nsObject setValue_forKeyPathSelector value (toNSString keyPath)

-- | @- validateValue:forKeyPath:error:@
validateValue_forKeyPath_error :: (IsNSObject nsObject, IsNSString inKeyPath, IsNSError outError) => nsObject -> Ptr RawId -> inKeyPath -> outError -> IO Bool
validateValue_forKeyPath_error nsObject ioValue inKeyPath outError =
  sendMessage nsObject validateValue_forKeyPath_errorSelector ioValue (toNSString inKeyPath) (toNSError outError)

-- | @- mutableArrayValueForKeyPath:@
mutableArrayValueForKeyPath :: (IsNSObject nsObject, IsNSString keyPath) => nsObject -> keyPath -> IO (Id NSMutableArray)
mutableArrayValueForKeyPath nsObject keyPath =
  sendMessage nsObject mutableArrayValueForKeyPathSelector (toNSString keyPath)

-- | @- mutableOrderedSetValueForKeyPath:@
mutableOrderedSetValueForKeyPath :: (IsNSObject nsObject, IsNSString keyPath) => nsObject -> keyPath -> IO (Id NSMutableOrderedSet)
mutableOrderedSetValueForKeyPath nsObject keyPath =
  sendMessage nsObject mutableOrderedSetValueForKeyPathSelector (toNSString keyPath)

-- | @- mutableSetValueForKeyPath:@
mutableSetValueForKeyPath :: (IsNSObject nsObject, IsNSString keyPath) => nsObject -> keyPath -> IO (Id NSMutableSet)
mutableSetValueForKeyPath nsObject keyPath =
  sendMessage nsObject mutableSetValueForKeyPathSelector (toNSString keyPath)

-- | @- valueForUndefinedKey:@
valueForUndefinedKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> key -> IO RawId
valueForUndefinedKey nsObject key =
  sendMessage nsObject valueForUndefinedKeySelector (toNSString key)

-- | @- setValue:forUndefinedKey:@
setValue_forUndefinedKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> RawId -> key -> IO ()
setValue_forUndefinedKey nsObject value key =
  sendMessage nsObject setValue_forUndefinedKeySelector value (toNSString key)

-- | @- setNilValueForKey:@
setNilValueForKey :: (IsNSObject nsObject, IsNSString key) => nsObject -> key -> IO ()
setNilValueForKey nsObject key =
  sendMessage nsObject setNilValueForKeySelector (toNSString key)

-- | @- dictionaryWithValuesForKeys:@
dictionaryWithValuesForKeys :: (IsNSObject nsObject, IsNSArray keys) => nsObject -> keys -> IO (Id NSDictionary)
dictionaryWithValuesForKeys nsObject keys =
  sendMessage nsObject dictionaryWithValuesForKeysSelector (toNSArray keys)

-- | @- setValuesForKeysWithDictionary:@
setValuesForKeysWithDictionary :: (IsNSObject nsObject, IsNSDictionary keyedValues) => nsObject -> keyedValues -> IO ()
setValuesForKeysWithDictionary nsObject keyedValues =
  sendMessage nsObject setValuesForKeysWithDictionarySelector (toNSDictionary keyedValues)

-- | @- fileManager:shouldProceedAfterError:@
fileManager_shouldProceedAfterError :: (IsNSObject nsObject, IsNSFileManager fm, IsNSDictionary errorInfo) => nsObject -> fm -> errorInfo -> IO Bool
fileManager_shouldProceedAfterError nsObject fm errorInfo =
  sendMessage nsObject fileManager_shouldProceedAfterErrorSelector (toNSFileManager fm) (toNSDictionary errorInfo)

-- | @- fileManager:willProcessPath:@
fileManager_willProcessPath :: (IsNSObject nsObject, IsNSFileManager fm, IsNSString path) => nsObject -> fm -> path -> IO ()
fileManager_willProcessPath nsObject fm path =
  sendMessage nsObject fileManager_willProcessPathSelector (toNSFileManager fm) (toNSString path)

-- | @- URL:resourceDataDidBecomeAvailable:@
urL_resourceDataDidBecomeAvailable :: (IsNSObject nsObject, IsNSURL sender, IsNSData newBytes) => nsObject -> sender -> newBytes -> IO ()
urL_resourceDataDidBecomeAvailable nsObject sender newBytes =
  sendMessage nsObject urL_resourceDataDidBecomeAvailableSelector (toNSURL sender) (toNSData newBytes)

-- | @- URLResourceDidFinishLoading:@
urlResourceDidFinishLoading :: (IsNSObject nsObject, IsNSURL sender) => nsObject -> sender -> IO ()
urlResourceDidFinishLoading nsObject sender =
  sendMessage nsObject urlResourceDidFinishLoadingSelector (toNSURL sender)

-- | @- URLResourceDidCancelLoading:@
urlResourceDidCancelLoading :: (IsNSObject nsObject, IsNSURL sender) => nsObject -> sender -> IO ()
urlResourceDidCancelLoading nsObject sender =
  sendMessage nsObject urlResourceDidCancelLoadingSelector (toNSURL sender)

-- | @- URL:resourceDidFailLoadingWithReason:@
urL_resourceDidFailLoadingWithReason :: (IsNSObject nsObject, IsNSURL sender, IsNSString reason) => nsObject -> sender -> reason -> IO ()
urL_resourceDidFailLoadingWithReason nsObject sender reason =
  sendMessage nsObject urL_resourceDidFailLoadingWithReasonSelector (toNSURL sender) (toNSString reason)

-- | @- performSelector:withObject:afterDelay:inModes:@
performSelector_withObject_afterDelay_inModes :: (IsNSObject nsObject, IsNSArray modes) => nsObject -> Sel -> RawId -> CDouble -> modes -> IO ()
performSelector_withObject_afterDelay_inModes nsObject aSelector anArgument delay modes =
  sendMessage nsObject performSelector_withObject_afterDelay_inModesSelector aSelector anArgument delay (toNSArray modes)

-- | @- performSelector:withObject:afterDelay:@
performSelector_withObject_afterDelay :: IsNSObject nsObject => nsObject -> Sel -> RawId -> CDouble -> IO ()
performSelector_withObject_afterDelay nsObject aSelector anArgument delay =
  sendMessage nsObject performSelector_withObject_afterDelaySelector aSelector anArgument delay

-- | @+ cancelPreviousPerformRequestsWithTarget:selector:object:@
cancelPreviousPerformRequestsWithTarget_selector_object :: RawId -> Sel -> RawId -> IO ()
cancelPreviousPerformRequestsWithTarget_selector_object aTarget aSelector anArgument =
  do
    cls' <- getRequiredClass "NSObject"
    sendClassMessage cls' cancelPreviousPerformRequestsWithTarget_selector_objectSelector aTarget aSelector anArgument

-- | @+ cancelPreviousPerformRequestsWithTarget:@
cancelPreviousPerformRequestsWithTarget :: RawId -> IO ()
cancelPreviousPerformRequestsWithTarget aTarget =
  do
    cls' <- getRequiredClass "NSObject"
    sendClassMessage cls' cancelPreviousPerformRequestsWithTargetSelector aTarget

-- | @- attemptRecoveryFromError:optionIndex:delegate:didRecoverSelector:contextInfo:@
attemptRecoveryFromError_optionIndex_delegate_didRecoverSelector_contextInfo :: (IsNSObject nsObject, IsNSError error_) => nsObject -> error_ -> CULong -> RawId -> Sel -> Ptr () -> IO ()
attemptRecoveryFromError_optionIndex_delegate_didRecoverSelector_contextInfo nsObject error_ recoveryOptionIndex delegate didRecoverSelector contextInfo =
  sendMessage nsObject attemptRecoveryFromError_optionIndex_delegate_didRecoverSelector_contextInfoSelector (toNSError error_) recoveryOptionIndex delegate didRecoverSelector contextInfo

-- | @- attemptRecoveryFromError:optionIndex:@
attemptRecoveryFromError_optionIndex :: (IsNSObject nsObject, IsNSError error_) => nsObject -> error_ -> CULong -> IO Bool
attemptRecoveryFromError_optionIndex nsObject error_ recoveryOptionIndex =
  sendMessage nsObject attemptRecoveryFromError_optionIndexSelector (toNSError error_) recoveryOptionIndex

-- | @+ poseAsClass:@
poseAsClass :: Class -> IO ()
poseAsClass aClass =
  do
    cls' <- getRequiredClass "NSObject"
    sendClassMessage cls' poseAsClassSelector aClass

-- | @+ version@
version :: IO CLong
version  =
  do
    cls' <- getRequiredClass "NSObject"
    sendClassMessage cls' versionSelector

-- | @+ setVersion:@
setVersion :: CLong -> IO ()
setVersion aVersion =
  do
    cls' <- getRequiredClass "NSObject"
    sendClassMessage cls' setVersionSelector aVersion

-- | @- replacementObjectForCoder:@
replacementObjectForCoder :: (IsNSObject nsObject, IsNSCoder coder) => nsObject -> coder -> IO RawId
replacementObjectForCoder nsObject coder =
  sendMessage nsObject replacementObjectForCoderSelector (toNSCoder coder)

-- | @- awakeAfterUsingCoder:@
awakeAfterUsingCoder :: (IsNSObject nsObject, IsNSCoder coder) => nsObject -> coder -> IO RawId
awakeAfterUsingCoder nsObject coder =
  sendMessage nsObject awakeAfterUsingCoderSelector (toNSCoder coder)

-- | @- objectSpecifier@
objectSpecifier :: IsNSObject nsObject => nsObject -> IO (Id NSScriptObjectSpecifier)
objectSpecifier nsObject =
  sendMessage nsObject objectSpecifierSelector

-- | @- classCode@
classCode :: IsNSObject nsObject => nsObject -> IO CUInt
classCode nsObject =
  sendMessage nsObject classCodeSelector

-- | @- className@
className :: IsNSObject nsObject => nsObject -> IO (Id NSString)
className nsObject =
  sendMessage nsObject classNameSelector

-- | @- scriptingProperties@
scriptingProperties :: IsNSObject nsObject => nsObject -> IO (Id NSDictionary)
scriptingProperties nsObject =
  sendMessage nsObject scriptingPropertiesSelector

-- | @- setScriptingProperties:@
setScriptingProperties :: (IsNSObject nsObject, IsNSDictionary value) => nsObject -> value -> IO ()
setScriptingProperties nsObject value =
  sendMessage nsObject setScriptingPropertiesSelector (toNSDictionary value)

-- | @- classDescription@
classDescription :: IsNSObject nsObject => nsObject -> IO (Id NSClassDescription)
classDescription nsObject =
  sendMessage nsObject classDescriptionSelector

-- | @- attributeKeys@
attributeKeys :: IsNSObject nsObject => nsObject -> IO (Id NSArray)
attributeKeys nsObject =
  sendMessage nsObject attributeKeysSelector

-- | @- toOneRelationshipKeys@
toOneRelationshipKeys :: IsNSObject nsObject => nsObject -> IO (Id NSArray)
toOneRelationshipKeys nsObject =
  sendMessage nsObject toOneRelationshipKeysSelector

-- | @- toManyRelationshipKeys@
toManyRelationshipKeys :: IsNSObject nsObject => nsObject -> IO (Id NSArray)
toManyRelationshipKeys nsObject =
  sendMessage nsObject toManyRelationshipKeysSelector

-- | @- classForPortCoder@
classForPortCoder :: IsNSObject nsObject => nsObject -> IO Class
classForPortCoder nsObject =
  sendMessage nsObject classForPortCoderSelector

-- | @- classForArchiver@
classForArchiver :: IsNSObject nsObject => nsObject -> IO Class
classForArchiver nsObject =
  sendMessage nsObject classForArchiverSelector

-- | @- classForKeyedArchiver@
classForKeyedArchiver :: IsNSObject nsObject => nsObject -> IO Class
classForKeyedArchiver nsObject =
  sendMessage nsObject classForKeyedArchiverSelector

-- | @- observationInfo@
observationInfo :: IsNSObject nsObject => nsObject -> IO (Ptr ())
observationInfo nsObject =
  sendMessage nsObject observationInfoSelector

-- | @- setObservationInfo:@
setObservationInfo :: IsNSObject nsObject => nsObject -> Ptr () -> IO ()
setObservationInfo nsObject value =
  sendMessage nsObject setObservationInfoSelector value

-- | @+ accessInstanceVariablesDirectly@
accessInstanceVariablesDirectly :: IO Bool
accessInstanceVariablesDirectly  =
  do
    cls' <- getRequiredClass "NSObject"
    sendClassMessage cls' accessInstanceVariablesDirectlySelector

-- | @- autoContentAccessingProxy@
autoContentAccessingProxy :: IsNSObject nsObject => nsObject -> IO RawId
autoContentAccessingProxy nsObject =
  sendMessage nsObject autoContentAccessingProxySelector

-- | @- classForCoder@
classForCoder :: IsNSObject nsObject => nsObject -> IO Class
classForCoder nsObject =
  sendMessage nsObject classForCoderSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scriptingIsEqualTo:@
scriptingIsEqualToSelector :: Selector '[RawId] Bool
scriptingIsEqualToSelector = mkSelector "scriptingIsEqualTo:"

-- | @Selector@ for @scriptingIsLessThanOrEqualTo:@
scriptingIsLessThanOrEqualToSelector :: Selector '[RawId] Bool
scriptingIsLessThanOrEqualToSelector = mkSelector "scriptingIsLessThanOrEqualTo:"

-- | @Selector@ for @scriptingIsLessThan:@
scriptingIsLessThanSelector :: Selector '[RawId] Bool
scriptingIsLessThanSelector = mkSelector "scriptingIsLessThan:"

-- | @Selector@ for @scriptingIsGreaterThanOrEqualTo:@
scriptingIsGreaterThanOrEqualToSelector :: Selector '[RawId] Bool
scriptingIsGreaterThanOrEqualToSelector = mkSelector "scriptingIsGreaterThanOrEqualTo:"

-- | @Selector@ for @scriptingIsGreaterThan:@
scriptingIsGreaterThanSelector :: Selector '[RawId] Bool
scriptingIsGreaterThanSelector = mkSelector "scriptingIsGreaterThan:"

-- | @Selector@ for @scriptingBeginsWith:@
scriptingBeginsWithSelector :: Selector '[RawId] Bool
scriptingBeginsWithSelector = mkSelector "scriptingBeginsWith:"

-- | @Selector@ for @scriptingEndsWith:@
scriptingEndsWithSelector :: Selector '[RawId] Bool
scriptingEndsWithSelector = mkSelector "scriptingEndsWith:"

-- | @Selector@ for @scriptingContains:@
scriptingContainsSelector :: Selector '[RawId] Bool
scriptingContainsSelector = mkSelector "scriptingContains:"

-- | @Selector@ for @isEqualTo:@
isEqualToSelector :: Selector '[RawId] Bool
isEqualToSelector = mkSelector "isEqualTo:"

-- | @Selector@ for @isLessThanOrEqualTo:@
isLessThanOrEqualToSelector :: Selector '[RawId] Bool
isLessThanOrEqualToSelector = mkSelector "isLessThanOrEqualTo:"

-- | @Selector@ for @isLessThan:@
isLessThanSelector :: Selector '[RawId] Bool
isLessThanSelector = mkSelector "isLessThan:"

-- | @Selector@ for @isGreaterThanOrEqualTo:@
isGreaterThanOrEqualToSelector :: Selector '[RawId] Bool
isGreaterThanOrEqualToSelector = mkSelector "isGreaterThanOrEqualTo:"

-- | @Selector@ for @isGreaterThan:@
isGreaterThanSelector :: Selector '[RawId] Bool
isGreaterThanSelector = mkSelector "isGreaterThan:"

-- | @Selector@ for @isNotEqualTo:@
isNotEqualToSelector :: Selector '[RawId] Bool
isNotEqualToSelector = mkSelector "isNotEqualTo:"

-- | @Selector@ for @doesContain:@
doesContainSelector :: Selector '[RawId] Bool
doesContainSelector = mkSelector "doesContain:"

-- | @Selector@ for @isLike:@
isLikeSelector :: Selector '[Id NSString] Bool
isLikeSelector = mkSelector "isLike:"

-- | @Selector@ for @isCaseInsensitiveLike:@
isCaseInsensitiveLikeSelector :: Selector '[Id NSString] Bool
isCaseInsensitiveLikeSelector = mkSelector "isCaseInsensitiveLike:"

-- | @Selector@ for @indicesOfObjectsByEvaluatingObjectSpecifier:@
indicesOfObjectsByEvaluatingObjectSpecifierSelector :: Selector '[Id NSScriptObjectSpecifier] (Id NSArray)
indicesOfObjectsByEvaluatingObjectSpecifierSelector = mkSelector "indicesOfObjectsByEvaluatingObjectSpecifier:"

-- | @Selector@ for @valueAtIndex:inPropertyWithKey:@
valueAtIndex_inPropertyWithKeySelector :: Selector '[CULong, Id NSString] RawId
valueAtIndex_inPropertyWithKeySelector = mkSelector "valueAtIndex:inPropertyWithKey:"

-- | @Selector@ for @valueWithName:inPropertyWithKey:@
valueWithName_inPropertyWithKeySelector :: Selector '[Id NSString, Id NSString] RawId
valueWithName_inPropertyWithKeySelector = mkSelector "valueWithName:inPropertyWithKey:"

-- | @Selector@ for @valueWithUniqueID:inPropertyWithKey:@
valueWithUniqueID_inPropertyWithKeySelector :: Selector '[RawId, Id NSString] RawId
valueWithUniqueID_inPropertyWithKeySelector = mkSelector "valueWithUniqueID:inPropertyWithKey:"

-- | @Selector@ for @insertValue:atIndex:inPropertyWithKey:@
insertValue_atIndex_inPropertyWithKeySelector :: Selector '[RawId, CULong, Id NSString] ()
insertValue_atIndex_inPropertyWithKeySelector = mkSelector "insertValue:atIndex:inPropertyWithKey:"

-- | @Selector@ for @removeValueAtIndex:fromPropertyWithKey:@
removeValueAtIndex_fromPropertyWithKeySelector :: Selector '[CULong, Id NSString] ()
removeValueAtIndex_fromPropertyWithKeySelector = mkSelector "removeValueAtIndex:fromPropertyWithKey:"

-- | @Selector@ for @replaceValueAtIndex:inPropertyWithKey:withValue:@
replaceValueAtIndex_inPropertyWithKey_withValueSelector :: Selector '[CULong, Id NSString, RawId] ()
replaceValueAtIndex_inPropertyWithKey_withValueSelector = mkSelector "replaceValueAtIndex:inPropertyWithKey:withValue:"

-- | @Selector@ for @insertValue:inPropertyWithKey:@
insertValue_inPropertyWithKeySelector :: Selector '[RawId, Id NSString] ()
insertValue_inPropertyWithKeySelector = mkSelector "insertValue:inPropertyWithKey:"

-- | @Selector@ for @coerceValue:forKey:@
coerceValue_forKeySelector :: Selector '[RawId, Id NSString] RawId
coerceValue_forKeySelector = mkSelector "coerceValue:forKey:"

-- | @Selector@ for @scriptingValueForSpecifier:@
scriptingValueForSpecifierSelector :: Selector '[Id NSScriptObjectSpecifier] RawId
scriptingValueForSpecifierSelector = mkSelector "scriptingValueForSpecifier:"

-- | @Selector@ for @copyScriptingValue:forKey:withProperties:@
copyScriptingValue_forKey_withPropertiesSelector :: Selector '[RawId, Id NSString, Id NSDictionary] RawId
copyScriptingValue_forKey_withPropertiesSelector = mkSelector "copyScriptingValue:forKey:withProperties:"

-- | @Selector@ for @newScriptingObjectOfClass:forValueForKey:withContentsValue:properties:@
newScriptingObjectOfClass_forValueForKey_withContentsValue_propertiesSelector :: Selector '[Class, Id NSString, RawId, Id NSDictionary] RawId
newScriptingObjectOfClass_forValueForKey_withContentsValue_propertiesSelector = mkSelector "newScriptingObjectOfClass:forValueForKey:withContentsValue:properties:"

-- | @Selector@ for @inverseForRelationshipKey:@
inverseForRelationshipKeySelector :: Selector '[Id NSString] (Id NSString)
inverseForRelationshipKeySelector = mkSelector "inverseForRelationshipKey:"

-- | @Selector@ for @replacementObjectForPortCoder:@
replacementObjectForPortCoderSelector :: Selector '[Id NSPortCoder] RawId
replacementObjectForPortCoderSelector = mkSelector "replacementObjectForPortCoder:"

-- | @Selector@ for @replacementObjectForArchiver:@
replacementObjectForArchiverSelector :: Selector '[Id NSArchiver] RawId
replacementObjectForArchiverSelector = mkSelector "replacementObjectForArchiver:"

-- | @Selector@ for @performSelectorOnMainThread:withObject:waitUntilDone:modes:@
performSelectorOnMainThread_withObject_waitUntilDone_modesSelector :: Selector '[Sel, RawId, Bool, Id NSArray] ()
performSelectorOnMainThread_withObject_waitUntilDone_modesSelector = mkSelector "performSelectorOnMainThread:withObject:waitUntilDone:modes:"

-- | @Selector@ for @performSelectorOnMainThread:withObject:waitUntilDone:@
performSelectorOnMainThread_withObject_waitUntilDoneSelector :: Selector '[Sel, RawId, Bool] ()
performSelectorOnMainThread_withObject_waitUntilDoneSelector = mkSelector "performSelectorOnMainThread:withObject:waitUntilDone:"

-- | @Selector@ for @performSelector:onThread:withObject:waitUntilDone:modes:@
performSelector_onThread_withObject_waitUntilDone_modesSelector :: Selector '[Sel, Id NSThread, RawId, Bool, Id NSArray] ()
performSelector_onThread_withObject_waitUntilDone_modesSelector = mkSelector "performSelector:onThread:withObject:waitUntilDone:modes:"

-- | @Selector@ for @performSelector:onThread:withObject:waitUntilDone:@
performSelector_onThread_withObject_waitUntilDoneSelector :: Selector '[Sel, Id NSThread, RawId, Bool] ()
performSelector_onThread_withObject_waitUntilDoneSelector = mkSelector "performSelector:onThread:withObject:waitUntilDone:"

-- | @Selector@ for @performSelectorInBackground:withObject:@
performSelectorInBackground_withObjectSelector :: Selector '[Sel, RawId] ()
performSelectorInBackground_withObjectSelector = mkSelector "performSelectorInBackground:withObject:"

-- | @Selector@ for @classForKeyedUnarchiver@
classForKeyedUnarchiverSelector :: Selector '[] Class
classForKeyedUnarchiverSelector = mkSelector "classForKeyedUnarchiver"

-- | @Selector@ for @replacementObjectForKeyedArchiver:@
replacementObjectForKeyedArchiverSelector :: Selector '[Id NSKeyedArchiver] RawId
replacementObjectForKeyedArchiverSelector = mkSelector "replacementObjectForKeyedArchiver:"

-- | @Selector@ for @classFallbacksForKeyedArchiver@
classFallbacksForKeyedArchiverSelector :: Selector '[] (Id NSArray)
classFallbacksForKeyedArchiverSelector = mkSelector "classFallbacksForKeyedArchiver"

-- | @Selector@ for @setSharedObservers:@
setSharedObserversSelector :: Selector '[Id NSKeyValueSharedObserversSnapshot] ()
setSharedObserversSelector = mkSelector "setSharedObservers:"

-- | @Selector@ for @setKeys:triggerChangeNotificationsForDependentKey:@
setKeys_triggerChangeNotificationsForDependentKeySelector :: Selector '[Id NSArray, Id NSString] ()
setKeys_triggerChangeNotificationsForDependentKeySelector = mkSelector "setKeys:triggerChangeNotificationsForDependentKey:"

-- | @Selector@ for @keyPathsForValuesAffectingValueForKey:@
keyPathsForValuesAffectingValueForKeySelector :: Selector '[Id NSString] (Id NSSet)
keyPathsForValuesAffectingValueForKeySelector = mkSelector "keyPathsForValuesAffectingValueForKey:"

-- | @Selector@ for @automaticallyNotifiesObserversForKey:@
automaticallyNotifiesObserversForKeySelector :: Selector '[Id NSString] Bool
automaticallyNotifiesObserversForKeySelector = mkSelector "automaticallyNotifiesObserversForKey:"

-- | @Selector@ for @willChangeValueForKey:@
willChangeValueForKeySelector :: Selector '[Id NSString] ()
willChangeValueForKeySelector = mkSelector "willChangeValueForKey:"

-- | @Selector@ for @didChangeValueForKey:@
didChangeValueForKeySelector :: Selector '[Id NSString] ()
didChangeValueForKeySelector = mkSelector "didChangeValueForKey:"

-- | @Selector@ for @willChange:valuesAtIndexes:forKey:@
willChange_valuesAtIndexes_forKeySelector :: Selector '[NSKeyValueChange, Id NSIndexSet, Id NSString] ()
willChange_valuesAtIndexes_forKeySelector = mkSelector "willChange:valuesAtIndexes:forKey:"

-- | @Selector@ for @didChange:valuesAtIndexes:forKey:@
didChange_valuesAtIndexes_forKeySelector :: Selector '[NSKeyValueChange, Id NSIndexSet, Id NSString] ()
didChange_valuesAtIndexes_forKeySelector = mkSelector "didChange:valuesAtIndexes:forKey:"

-- | @Selector@ for @willChangeValueForKey:withSetMutation:usingObjects:@
willChangeValueForKey_withSetMutation_usingObjectsSelector :: Selector '[Id NSString, NSKeyValueSetMutationKind, Id NSSet] ()
willChangeValueForKey_withSetMutation_usingObjectsSelector = mkSelector "willChangeValueForKey:withSetMutation:usingObjects:"

-- | @Selector@ for @didChangeValueForKey:withSetMutation:usingObjects:@
didChangeValueForKey_withSetMutation_usingObjectsSelector :: Selector '[Id NSString, NSKeyValueSetMutationKind, Id NSSet] ()
didChangeValueForKey_withSetMutation_usingObjectsSelector = mkSelector "didChangeValueForKey:withSetMutation:usingObjects:"

-- | @Selector@ for @addObserver:forKeyPath:options:context:@
addObserver_forKeyPath_options_contextSelector :: Selector '[Id NSObject, Id NSString, NSKeyValueObservingOptions, Ptr ()] ()
addObserver_forKeyPath_options_contextSelector = mkSelector "addObserver:forKeyPath:options:context:"

-- | @Selector@ for @removeObserver:forKeyPath:context:@
removeObserver_forKeyPath_contextSelector :: Selector '[Id NSObject, Id NSString, Ptr ()] ()
removeObserver_forKeyPath_contextSelector = mkSelector "removeObserver:forKeyPath:context:"

-- | @Selector@ for @removeObserver:forKeyPath:@
removeObserver_forKeyPathSelector :: Selector '[Id NSObject, Id NSString] ()
removeObserver_forKeyPathSelector = mkSelector "removeObserver:forKeyPath:"

-- | @Selector@ for @observeValueForKeyPath:ofObject:change:context:@
observeValueForKeyPath_ofObject_change_contextSelector :: Selector '[Id NSString, RawId, Id NSDictionary, Ptr ()] ()
observeValueForKeyPath_ofObject_change_contextSelector = mkSelector "observeValueForKeyPath:ofObject:change:context:"

-- | @Selector@ for @useStoredAccessor@
useStoredAccessorSelector :: Selector '[] Bool
useStoredAccessorSelector = mkSelector "useStoredAccessor"

-- | @Selector@ for @storedValueForKey:@
storedValueForKeySelector :: Selector '[Id NSString] RawId
storedValueForKeySelector = mkSelector "storedValueForKey:"

-- | @Selector@ for @takeStoredValue:forKey:@
takeStoredValue_forKeySelector :: Selector '[RawId, Id NSString] ()
takeStoredValue_forKeySelector = mkSelector "takeStoredValue:forKey:"

-- | @Selector@ for @takeValue:forKey:@
takeValue_forKeySelector :: Selector '[RawId, Id NSString] ()
takeValue_forKeySelector = mkSelector "takeValue:forKey:"

-- | @Selector@ for @takeValue:forKeyPath:@
takeValue_forKeyPathSelector :: Selector '[RawId, Id NSString] ()
takeValue_forKeyPathSelector = mkSelector "takeValue:forKeyPath:"

-- | @Selector@ for @handleQueryWithUnboundKey:@
handleQueryWithUnboundKeySelector :: Selector '[Id NSString] RawId
handleQueryWithUnboundKeySelector = mkSelector "handleQueryWithUnboundKey:"

-- | @Selector@ for @handleTakeValue:forUnboundKey:@
handleTakeValue_forUnboundKeySelector :: Selector '[RawId, Id NSString] ()
handleTakeValue_forUnboundKeySelector = mkSelector "handleTakeValue:forUnboundKey:"

-- | @Selector@ for @unableToSetNilForKey:@
unableToSetNilForKeySelector :: Selector '[Id NSString] ()
unableToSetNilForKeySelector = mkSelector "unableToSetNilForKey:"

-- | @Selector@ for @valuesForKeys:@
valuesForKeysSelector :: Selector '[Id NSArray] (Id NSDictionary)
valuesForKeysSelector = mkSelector "valuesForKeys:"

-- | @Selector@ for @takeValuesFromDictionary:@
takeValuesFromDictionarySelector :: Selector '[Id NSDictionary] ()
takeValuesFromDictionarySelector = mkSelector "takeValuesFromDictionary:"

-- | @Selector@ for @valueForKey:@
valueForKeySelector :: Selector '[Id NSString] RawId
valueForKeySelector = mkSelector "valueForKey:"

-- | @Selector@ for @setValue:forKey:@
setValue_forKeySelector :: Selector '[RawId, Id NSString] ()
setValue_forKeySelector = mkSelector "setValue:forKey:"

-- | @Selector@ for @validateValue:forKey:error:@
validateValue_forKey_errorSelector :: Selector '[Ptr RawId, Id NSString, Id NSError] Bool
validateValue_forKey_errorSelector = mkSelector "validateValue:forKey:error:"

-- | @Selector@ for @mutableArrayValueForKey:@
mutableArrayValueForKeySelector :: Selector '[Id NSString] (Id NSMutableArray)
mutableArrayValueForKeySelector = mkSelector "mutableArrayValueForKey:"

-- | @Selector@ for @mutableOrderedSetValueForKey:@
mutableOrderedSetValueForKeySelector :: Selector '[Id NSString] (Id NSMutableOrderedSet)
mutableOrderedSetValueForKeySelector = mkSelector "mutableOrderedSetValueForKey:"

-- | @Selector@ for @mutableSetValueForKey:@
mutableSetValueForKeySelector :: Selector '[Id NSString] (Id NSMutableSet)
mutableSetValueForKeySelector = mkSelector "mutableSetValueForKey:"

-- | @Selector@ for @valueForKeyPath:@
valueForKeyPathSelector :: Selector '[Id NSString] RawId
valueForKeyPathSelector = mkSelector "valueForKeyPath:"

-- | @Selector@ for @setValue:forKeyPath:@
setValue_forKeyPathSelector :: Selector '[RawId, Id NSString] ()
setValue_forKeyPathSelector = mkSelector "setValue:forKeyPath:"

-- | @Selector@ for @validateValue:forKeyPath:error:@
validateValue_forKeyPath_errorSelector :: Selector '[Ptr RawId, Id NSString, Id NSError] Bool
validateValue_forKeyPath_errorSelector = mkSelector "validateValue:forKeyPath:error:"

-- | @Selector@ for @mutableArrayValueForKeyPath:@
mutableArrayValueForKeyPathSelector :: Selector '[Id NSString] (Id NSMutableArray)
mutableArrayValueForKeyPathSelector = mkSelector "mutableArrayValueForKeyPath:"

-- | @Selector@ for @mutableOrderedSetValueForKeyPath:@
mutableOrderedSetValueForKeyPathSelector :: Selector '[Id NSString] (Id NSMutableOrderedSet)
mutableOrderedSetValueForKeyPathSelector = mkSelector "mutableOrderedSetValueForKeyPath:"

-- | @Selector@ for @mutableSetValueForKeyPath:@
mutableSetValueForKeyPathSelector :: Selector '[Id NSString] (Id NSMutableSet)
mutableSetValueForKeyPathSelector = mkSelector "mutableSetValueForKeyPath:"

-- | @Selector@ for @valueForUndefinedKey:@
valueForUndefinedKeySelector :: Selector '[Id NSString] RawId
valueForUndefinedKeySelector = mkSelector "valueForUndefinedKey:"

-- | @Selector@ for @setValue:forUndefinedKey:@
setValue_forUndefinedKeySelector :: Selector '[RawId, Id NSString] ()
setValue_forUndefinedKeySelector = mkSelector "setValue:forUndefinedKey:"

-- | @Selector@ for @setNilValueForKey:@
setNilValueForKeySelector :: Selector '[Id NSString] ()
setNilValueForKeySelector = mkSelector "setNilValueForKey:"

-- | @Selector@ for @dictionaryWithValuesForKeys:@
dictionaryWithValuesForKeysSelector :: Selector '[Id NSArray] (Id NSDictionary)
dictionaryWithValuesForKeysSelector = mkSelector "dictionaryWithValuesForKeys:"

-- | @Selector@ for @setValuesForKeysWithDictionary:@
setValuesForKeysWithDictionarySelector :: Selector '[Id NSDictionary] ()
setValuesForKeysWithDictionarySelector = mkSelector "setValuesForKeysWithDictionary:"

-- | @Selector@ for @fileManager:shouldProceedAfterError:@
fileManager_shouldProceedAfterErrorSelector :: Selector '[Id NSFileManager, Id NSDictionary] Bool
fileManager_shouldProceedAfterErrorSelector = mkSelector "fileManager:shouldProceedAfterError:"

-- | @Selector@ for @fileManager:willProcessPath:@
fileManager_willProcessPathSelector :: Selector '[Id NSFileManager, Id NSString] ()
fileManager_willProcessPathSelector = mkSelector "fileManager:willProcessPath:"

-- | @Selector@ for @URL:resourceDataDidBecomeAvailable:@
urL_resourceDataDidBecomeAvailableSelector :: Selector '[Id NSURL, Id NSData] ()
urL_resourceDataDidBecomeAvailableSelector = mkSelector "URL:resourceDataDidBecomeAvailable:"

-- | @Selector@ for @URLResourceDidFinishLoading:@
urlResourceDidFinishLoadingSelector :: Selector '[Id NSURL] ()
urlResourceDidFinishLoadingSelector = mkSelector "URLResourceDidFinishLoading:"

-- | @Selector@ for @URLResourceDidCancelLoading:@
urlResourceDidCancelLoadingSelector :: Selector '[Id NSURL] ()
urlResourceDidCancelLoadingSelector = mkSelector "URLResourceDidCancelLoading:"

-- | @Selector@ for @URL:resourceDidFailLoadingWithReason:@
urL_resourceDidFailLoadingWithReasonSelector :: Selector '[Id NSURL, Id NSString] ()
urL_resourceDidFailLoadingWithReasonSelector = mkSelector "URL:resourceDidFailLoadingWithReason:"

-- | @Selector@ for @performSelector:withObject:afterDelay:inModes:@
performSelector_withObject_afterDelay_inModesSelector :: Selector '[Sel, RawId, CDouble, Id NSArray] ()
performSelector_withObject_afterDelay_inModesSelector = mkSelector "performSelector:withObject:afterDelay:inModes:"

-- | @Selector@ for @performSelector:withObject:afterDelay:@
performSelector_withObject_afterDelaySelector :: Selector '[Sel, RawId, CDouble] ()
performSelector_withObject_afterDelaySelector = mkSelector "performSelector:withObject:afterDelay:"

-- | @Selector@ for @cancelPreviousPerformRequestsWithTarget:selector:object:@
cancelPreviousPerformRequestsWithTarget_selector_objectSelector :: Selector '[RawId, Sel, RawId] ()
cancelPreviousPerformRequestsWithTarget_selector_objectSelector = mkSelector "cancelPreviousPerformRequestsWithTarget:selector:object:"

-- | @Selector@ for @cancelPreviousPerformRequestsWithTarget:@
cancelPreviousPerformRequestsWithTargetSelector :: Selector '[RawId] ()
cancelPreviousPerformRequestsWithTargetSelector = mkSelector "cancelPreviousPerformRequestsWithTarget:"

-- | @Selector@ for @attemptRecoveryFromError:optionIndex:delegate:didRecoverSelector:contextInfo:@
attemptRecoveryFromError_optionIndex_delegate_didRecoverSelector_contextInfoSelector :: Selector '[Id NSError, CULong, RawId, Sel, Ptr ()] ()
attemptRecoveryFromError_optionIndex_delegate_didRecoverSelector_contextInfoSelector = mkSelector "attemptRecoveryFromError:optionIndex:delegate:didRecoverSelector:contextInfo:"

-- | @Selector@ for @attemptRecoveryFromError:optionIndex:@
attemptRecoveryFromError_optionIndexSelector :: Selector '[Id NSError, CULong] Bool
attemptRecoveryFromError_optionIndexSelector = mkSelector "attemptRecoveryFromError:optionIndex:"

-- | @Selector@ for @poseAsClass:@
poseAsClassSelector :: Selector '[Class] ()
poseAsClassSelector = mkSelector "poseAsClass:"

-- | @Selector@ for @version@
versionSelector :: Selector '[] CLong
versionSelector = mkSelector "version"

-- | @Selector@ for @setVersion:@
setVersionSelector :: Selector '[CLong] ()
setVersionSelector = mkSelector "setVersion:"

-- | @Selector@ for @replacementObjectForCoder:@
replacementObjectForCoderSelector :: Selector '[Id NSCoder] RawId
replacementObjectForCoderSelector = mkSelector "replacementObjectForCoder:"

-- | @Selector@ for @awakeAfterUsingCoder:@
awakeAfterUsingCoderSelector :: Selector '[Id NSCoder] RawId
awakeAfterUsingCoderSelector = mkSelector "awakeAfterUsingCoder:"

-- | @Selector@ for @objectSpecifier@
objectSpecifierSelector :: Selector '[] (Id NSScriptObjectSpecifier)
objectSpecifierSelector = mkSelector "objectSpecifier"

-- | @Selector@ for @classCode@
classCodeSelector :: Selector '[] CUInt
classCodeSelector = mkSelector "classCode"

-- | @Selector@ for @className@
classNameSelector :: Selector '[] (Id NSString)
classNameSelector = mkSelector "className"

-- | @Selector@ for @scriptingProperties@
scriptingPropertiesSelector :: Selector '[] (Id NSDictionary)
scriptingPropertiesSelector = mkSelector "scriptingProperties"

-- | @Selector@ for @setScriptingProperties:@
setScriptingPropertiesSelector :: Selector '[Id NSDictionary] ()
setScriptingPropertiesSelector = mkSelector "setScriptingProperties:"

-- | @Selector@ for @classDescription@
classDescriptionSelector :: Selector '[] (Id NSClassDescription)
classDescriptionSelector = mkSelector "classDescription"

-- | @Selector@ for @attributeKeys@
attributeKeysSelector :: Selector '[] (Id NSArray)
attributeKeysSelector = mkSelector "attributeKeys"

-- | @Selector@ for @toOneRelationshipKeys@
toOneRelationshipKeysSelector :: Selector '[] (Id NSArray)
toOneRelationshipKeysSelector = mkSelector "toOneRelationshipKeys"

-- | @Selector@ for @toManyRelationshipKeys@
toManyRelationshipKeysSelector :: Selector '[] (Id NSArray)
toManyRelationshipKeysSelector = mkSelector "toManyRelationshipKeys"

-- | @Selector@ for @classForPortCoder@
classForPortCoderSelector :: Selector '[] Class
classForPortCoderSelector = mkSelector "classForPortCoder"

-- | @Selector@ for @classForArchiver@
classForArchiverSelector :: Selector '[] Class
classForArchiverSelector = mkSelector "classForArchiver"

-- | @Selector@ for @classForKeyedArchiver@
classForKeyedArchiverSelector :: Selector '[] Class
classForKeyedArchiverSelector = mkSelector "classForKeyedArchiver"

-- | @Selector@ for @observationInfo@
observationInfoSelector :: Selector '[] (Ptr ())
observationInfoSelector = mkSelector "observationInfo"

-- | @Selector@ for @setObservationInfo:@
setObservationInfoSelector :: Selector '[Ptr ()] ()
setObservationInfoSelector = mkSelector "setObservationInfo:"

-- | @Selector@ for @accessInstanceVariablesDirectly@
accessInstanceVariablesDirectlySelector :: Selector '[] Bool
accessInstanceVariablesDirectlySelector = mkSelector "accessInstanceVariablesDirectly"

-- | @Selector@ for @autoContentAccessingProxy@
autoContentAccessingProxySelector :: Selector '[] RawId
autoContentAccessingProxySelector = mkSelector "autoContentAccessingProxy"

-- | @Selector@ for @classForCoder@
classForCoderSelector :: Selector '[] Class
classForCoderSelector = mkSelector "classForCoder"

