{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSScriptObjectSpecifier@.
module ObjC.Foundation.NSScriptObjectSpecifier
  ( NSScriptObjectSpecifier
  , IsNSScriptObjectSpecifier(..)
  , objectSpecifierWithDescriptor
  , initWithContainerSpecifier_key
  , initWithContainerClassDescription_containerSpecifier_key
  , initWithCoder
  , indicesOfObjectsByEvaluatingWithContainer_count
  , objectsByEvaluatingWithContainers
  , childSpecifier
  , setChildSpecifier
  , containerSpecifier
  , setContainerSpecifier
  , containerIsObjectBeingTested
  , setContainerIsObjectBeingTested
  , containerIsRangeContainerObject
  , setContainerIsRangeContainerObject
  , key
  , setKey
  , containerClassDescription
  , setContainerClassDescription
  , keyClassDescription
  , objectsByEvaluatingSpecifier
  , evaluationErrorNumber
  , setEvaluationErrorNumber
  , evaluationErrorSpecifier
  , descriptor
  , childSpecifierSelector
  , containerClassDescriptionSelector
  , containerIsObjectBeingTestedSelector
  , containerIsRangeContainerObjectSelector
  , containerSpecifierSelector
  , descriptorSelector
  , evaluationErrorNumberSelector
  , evaluationErrorSpecifierSelector
  , indicesOfObjectsByEvaluatingWithContainer_countSelector
  , initWithCoderSelector
  , initWithContainerClassDescription_containerSpecifier_keySelector
  , initWithContainerSpecifier_keySelector
  , keyClassDescriptionSelector
  , keySelector
  , objectSpecifierWithDescriptorSelector
  , objectsByEvaluatingSpecifierSelector
  , objectsByEvaluatingWithContainersSelector
  , setChildSpecifierSelector
  , setContainerClassDescriptionSelector
  , setContainerIsObjectBeingTestedSelector
  , setContainerIsRangeContainerObjectSelector
  , setContainerSpecifierSelector
  , setEvaluationErrorNumberSelector
  , setKeySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ objectSpecifierWithDescriptor:@
objectSpecifierWithDescriptor :: IsNSAppleEventDescriptor descriptor => descriptor -> IO (Id NSScriptObjectSpecifier)
objectSpecifierWithDescriptor descriptor =
  do
    cls' <- getRequiredClass "NSScriptObjectSpecifier"
    sendClassMessage cls' objectSpecifierWithDescriptorSelector (toNSAppleEventDescriptor descriptor)

-- | @- initWithContainerSpecifier:key:@
initWithContainerSpecifier_key :: (IsNSScriptObjectSpecifier nsScriptObjectSpecifier, IsNSScriptObjectSpecifier container, IsNSString property) => nsScriptObjectSpecifier -> container -> property -> IO (Id NSScriptObjectSpecifier)
initWithContainerSpecifier_key nsScriptObjectSpecifier container property =
  sendOwnedMessage nsScriptObjectSpecifier initWithContainerSpecifier_keySelector (toNSScriptObjectSpecifier container) (toNSString property)

-- | @- initWithContainerClassDescription:containerSpecifier:key:@
initWithContainerClassDescription_containerSpecifier_key :: (IsNSScriptObjectSpecifier nsScriptObjectSpecifier, IsNSScriptClassDescription classDesc, IsNSScriptObjectSpecifier container, IsNSString property) => nsScriptObjectSpecifier -> classDesc -> container -> property -> IO (Id NSScriptObjectSpecifier)
initWithContainerClassDescription_containerSpecifier_key nsScriptObjectSpecifier classDesc container property =
  sendOwnedMessage nsScriptObjectSpecifier initWithContainerClassDescription_containerSpecifier_keySelector (toNSScriptClassDescription classDesc) (toNSScriptObjectSpecifier container) (toNSString property)

-- | @- initWithCoder:@
initWithCoder :: (IsNSScriptObjectSpecifier nsScriptObjectSpecifier, IsNSCoder inCoder) => nsScriptObjectSpecifier -> inCoder -> IO (Id NSScriptObjectSpecifier)
initWithCoder nsScriptObjectSpecifier inCoder =
  sendOwnedMessage nsScriptObjectSpecifier initWithCoderSelector (toNSCoder inCoder)

-- | @- indicesOfObjectsByEvaluatingWithContainer:count:@
indicesOfObjectsByEvaluatingWithContainer_count :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> RawId -> Ptr CLong -> IO (Ptr CLong)
indicesOfObjectsByEvaluatingWithContainer_count nsScriptObjectSpecifier container count =
  sendMessage nsScriptObjectSpecifier indicesOfObjectsByEvaluatingWithContainer_countSelector container count

-- | @- objectsByEvaluatingWithContainers:@
objectsByEvaluatingWithContainers :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> RawId -> IO RawId
objectsByEvaluatingWithContainers nsScriptObjectSpecifier containers =
  sendMessage nsScriptObjectSpecifier objectsByEvaluatingWithContainersSelector containers

-- | @- childSpecifier@
childSpecifier :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> IO (Id NSScriptObjectSpecifier)
childSpecifier nsScriptObjectSpecifier =
  sendMessage nsScriptObjectSpecifier childSpecifierSelector

-- | @- setChildSpecifier:@
setChildSpecifier :: (IsNSScriptObjectSpecifier nsScriptObjectSpecifier, IsNSScriptObjectSpecifier value) => nsScriptObjectSpecifier -> value -> IO ()
setChildSpecifier nsScriptObjectSpecifier value =
  sendMessage nsScriptObjectSpecifier setChildSpecifierSelector (toNSScriptObjectSpecifier value)

-- | @- containerSpecifier@
containerSpecifier :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> IO (Id NSScriptObjectSpecifier)
containerSpecifier nsScriptObjectSpecifier =
  sendMessage nsScriptObjectSpecifier containerSpecifierSelector

-- | @- setContainerSpecifier:@
setContainerSpecifier :: (IsNSScriptObjectSpecifier nsScriptObjectSpecifier, IsNSScriptObjectSpecifier value) => nsScriptObjectSpecifier -> value -> IO ()
setContainerSpecifier nsScriptObjectSpecifier value =
  sendMessage nsScriptObjectSpecifier setContainerSpecifierSelector (toNSScriptObjectSpecifier value)

-- | @- containerIsObjectBeingTested@
containerIsObjectBeingTested :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> IO Bool
containerIsObjectBeingTested nsScriptObjectSpecifier =
  sendMessage nsScriptObjectSpecifier containerIsObjectBeingTestedSelector

-- | @- setContainerIsObjectBeingTested:@
setContainerIsObjectBeingTested :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> Bool -> IO ()
setContainerIsObjectBeingTested nsScriptObjectSpecifier value =
  sendMessage nsScriptObjectSpecifier setContainerIsObjectBeingTestedSelector value

-- | @- containerIsRangeContainerObject@
containerIsRangeContainerObject :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> IO Bool
containerIsRangeContainerObject nsScriptObjectSpecifier =
  sendMessage nsScriptObjectSpecifier containerIsRangeContainerObjectSelector

-- | @- setContainerIsRangeContainerObject:@
setContainerIsRangeContainerObject :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> Bool -> IO ()
setContainerIsRangeContainerObject nsScriptObjectSpecifier value =
  sendMessage nsScriptObjectSpecifier setContainerIsRangeContainerObjectSelector value

-- | @- key@
key :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> IO (Id NSString)
key nsScriptObjectSpecifier =
  sendMessage nsScriptObjectSpecifier keySelector

-- | @- setKey:@
setKey :: (IsNSScriptObjectSpecifier nsScriptObjectSpecifier, IsNSString value) => nsScriptObjectSpecifier -> value -> IO ()
setKey nsScriptObjectSpecifier value =
  sendMessage nsScriptObjectSpecifier setKeySelector (toNSString value)

-- | @- containerClassDescription@
containerClassDescription :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> IO (Id NSScriptClassDescription)
containerClassDescription nsScriptObjectSpecifier =
  sendMessage nsScriptObjectSpecifier containerClassDescriptionSelector

-- | @- setContainerClassDescription:@
setContainerClassDescription :: (IsNSScriptObjectSpecifier nsScriptObjectSpecifier, IsNSScriptClassDescription value) => nsScriptObjectSpecifier -> value -> IO ()
setContainerClassDescription nsScriptObjectSpecifier value =
  sendMessage nsScriptObjectSpecifier setContainerClassDescriptionSelector (toNSScriptClassDescription value)

-- | @- keyClassDescription@
keyClassDescription :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> IO (Id NSScriptClassDescription)
keyClassDescription nsScriptObjectSpecifier =
  sendMessage nsScriptObjectSpecifier keyClassDescriptionSelector

-- | @- objectsByEvaluatingSpecifier@
objectsByEvaluatingSpecifier :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> IO RawId
objectsByEvaluatingSpecifier nsScriptObjectSpecifier =
  sendMessage nsScriptObjectSpecifier objectsByEvaluatingSpecifierSelector

-- | @- evaluationErrorNumber@
evaluationErrorNumber :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> IO CLong
evaluationErrorNumber nsScriptObjectSpecifier =
  sendMessage nsScriptObjectSpecifier evaluationErrorNumberSelector

-- | @- setEvaluationErrorNumber:@
setEvaluationErrorNumber :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> CLong -> IO ()
setEvaluationErrorNumber nsScriptObjectSpecifier value =
  sendMessage nsScriptObjectSpecifier setEvaluationErrorNumberSelector value

-- | @- evaluationErrorSpecifier@
evaluationErrorSpecifier :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> IO (Id NSScriptObjectSpecifier)
evaluationErrorSpecifier nsScriptObjectSpecifier =
  sendMessage nsScriptObjectSpecifier evaluationErrorSpecifierSelector

-- | @- descriptor@
descriptor :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> IO (Id NSAppleEventDescriptor)
descriptor nsScriptObjectSpecifier =
  sendMessage nsScriptObjectSpecifier descriptorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectSpecifierWithDescriptor:@
objectSpecifierWithDescriptorSelector :: Selector '[Id NSAppleEventDescriptor] (Id NSScriptObjectSpecifier)
objectSpecifierWithDescriptorSelector = mkSelector "objectSpecifierWithDescriptor:"

-- | @Selector@ for @initWithContainerSpecifier:key:@
initWithContainerSpecifier_keySelector :: Selector '[Id NSScriptObjectSpecifier, Id NSString] (Id NSScriptObjectSpecifier)
initWithContainerSpecifier_keySelector = mkSelector "initWithContainerSpecifier:key:"

-- | @Selector@ for @initWithContainerClassDescription:containerSpecifier:key:@
initWithContainerClassDescription_containerSpecifier_keySelector :: Selector '[Id NSScriptClassDescription, Id NSScriptObjectSpecifier, Id NSString] (Id NSScriptObjectSpecifier)
initWithContainerClassDescription_containerSpecifier_keySelector = mkSelector "initWithContainerClassDescription:containerSpecifier:key:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSScriptObjectSpecifier)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @indicesOfObjectsByEvaluatingWithContainer:count:@
indicesOfObjectsByEvaluatingWithContainer_countSelector :: Selector '[RawId, Ptr CLong] (Ptr CLong)
indicesOfObjectsByEvaluatingWithContainer_countSelector = mkSelector "indicesOfObjectsByEvaluatingWithContainer:count:"

-- | @Selector@ for @objectsByEvaluatingWithContainers:@
objectsByEvaluatingWithContainersSelector :: Selector '[RawId] RawId
objectsByEvaluatingWithContainersSelector = mkSelector "objectsByEvaluatingWithContainers:"

-- | @Selector@ for @childSpecifier@
childSpecifierSelector :: Selector '[] (Id NSScriptObjectSpecifier)
childSpecifierSelector = mkSelector "childSpecifier"

-- | @Selector@ for @setChildSpecifier:@
setChildSpecifierSelector :: Selector '[Id NSScriptObjectSpecifier] ()
setChildSpecifierSelector = mkSelector "setChildSpecifier:"

-- | @Selector@ for @containerSpecifier@
containerSpecifierSelector :: Selector '[] (Id NSScriptObjectSpecifier)
containerSpecifierSelector = mkSelector "containerSpecifier"

-- | @Selector@ for @setContainerSpecifier:@
setContainerSpecifierSelector :: Selector '[Id NSScriptObjectSpecifier] ()
setContainerSpecifierSelector = mkSelector "setContainerSpecifier:"

-- | @Selector@ for @containerIsObjectBeingTested@
containerIsObjectBeingTestedSelector :: Selector '[] Bool
containerIsObjectBeingTestedSelector = mkSelector "containerIsObjectBeingTested"

-- | @Selector@ for @setContainerIsObjectBeingTested:@
setContainerIsObjectBeingTestedSelector :: Selector '[Bool] ()
setContainerIsObjectBeingTestedSelector = mkSelector "setContainerIsObjectBeingTested:"

-- | @Selector@ for @containerIsRangeContainerObject@
containerIsRangeContainerObjectSelector :: Selector '[] Bool
containerIsRangeContainerObjectSelector = mkSelector "containerIsRangeContainerObject"

-- | @Selector@ for @setContainerIsRangeContainerObject:@
setContainerIsRangeContainerObjectSelector :: Selector '[Bool] ()
setContainerIsRangeContainerObjectSelector = mkSelector "setContainerIsRangeContainerObject:"

-- | @Selector@ for @key@
keySelector :: Selector '[] (Id NSString)
keySelector = mkSelector "key"

-- | @Selector@ for @setKey:@
setKeySelector :: Selector '[Id NSString] ()
setKeySelector = mkSelector "setKey:"

-- | @Selector@ for @containerClassDescription@
containerClassDescriptionSelector :: Selector '[] (Id NSScriptClassDescription)
containerClassDescriptionSelector = mkSelector "containerClassDescription"

-- | @Selector@ for @setContainerClassDescription:@
setContainerClassDescriptionSelector :: Selector '[Id NSScriptClassDescription] ()
setContainerClassDescriptionSelector = mkSelector "setContainerClassDescription:"

-- | @Selector@ for @keyClassDescription@
keyClassDescriptionSelector :: Selector '[] (Id NSScriptClassDescription)
keyClassDescriptionSelector = mkSelector "keyClassDescription"

-- | @Selector@ for @objectsByEvaluatingSpecifier@
objectsByEvaluatingSpecifierSelector :: Selector '[] RawId
objectsByEvaluatingSpecifierSelector = mkSelector "objectsByEvaluatingSpecifier"

-- | @Selector@ for @evaluationErrorNumber@
evaluationErrorNumberSelector :: Selector '[] CLong
evaluationErrorNumberSelector = mkSelector "evaluationErrorNumber"

-- | @Selector@ for @setEvaluationErrorNumber:@
setEvaluationErrorNumberSelector :: Selector '[CLong] ()
setEvaluationErrorNumberSelector = mkSelector "setEvaluationErrorNumber:"

-- | @Selector@ for @evaluationErrorSpecifier@
evaluationErrorSpecifierSelector :: Selector '[] (Id NSScriptObjectSpecifier)
evaluationErrorSpecifierSelector = mkSelector "evaluationErrorSpecifier"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id NSAppleEventDescriptor)
descriptorSelector = mkSelector "descriptor"

