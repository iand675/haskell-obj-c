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
  , objectSpecifierWithDescriptorSelector
  , initWithContainerSpecifier_keySelector
  , initWithContainerClassDescription_containerSpecifier_keySelector
  , initWithCoderSelector
  , indicesOfObjectsByEvaluatingWithContainer_countSelector
  , objectsByEvaluatingWithContainersSelector
  , childSpecifierSelector
  , setChildSpecifierSelector
  , containerSpecifierSelector
  , setContainerSpecifierSelector
  , containerIsObjectBeingTestedSelector
  , setContainerIsObjectBeingTestedSelector
  , containerIsRangeContainerObjectSelector
  , setContainerIsRangeContainerObjectSelector
  , keySelector
  , setKeySelector
  , containerClassDescriptionSelector
  , setContainerClassDescriptionSelector
  , keyClassDescriptionSelector
  , objectsByEvaluatingSpecifierSelector
  , evaluationErrorNumberSelector
  , setEvaluationErrorNumberSelector
  , evaluationErrorSpecifierSelector


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

-- | @+ objectSpecifierWithDescriptor:@
objectSpecifierWithDescriptor :: IsNSAppleEventDescriptor descriptor => descriptor -> IO (Id NSScriptObjectSpecifier)
objectSpecifierWithDescriptor descriptor =
  do
    cls' <- getRequiredClass "NSScriptObjectSpecifier"
    withObjCPtr descriptor $ \raw_descriptor ->
      sendClassMsg cls' (mkSelector "objectSpecifierWithDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_descriptor :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithContainerSpecifier:key:@
initWithContainerSpecifier_key :: (IsNSScriptObjectSpecifier nsScriptObjectSpecifier, IsNSScriptObjectSpecifier container, IsNSString property) => nsScriptObjectSpecifier -> container -> property -> IO (Id NSScriptObjectSpecifier)
initWithContainerSpecifier_key nsScriptObjectSpecifier  container property =
withObjCPtr container $ \raw_container ->
  withObjCPtr property $ \raw_property ->
      sendMsg nsScriptObjectSpecifier (mkSelector "initWithContainerSpecifier:key:") (retPtr retVoid) [argPtr (castPtr raw_container :: Ptr ()), argPtr (castPtr raw_property :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContainerClassDescription:containerSpecifier:key:@
initWithContainerClassDescription_containerSpecifier_key :: (IsNSScriptObjectSpecifier nsScriptObjectSpecifier, IsNSScriptClassDescription classDesc, IsNSScriptObjectSpecifier container, IsNSString property) => nsScriptObjectSpecifier -> classDesc -> container -> property -> IO (Id NSScriptObjectSpecifier)
initWithContainerClassDescription_containerSpecifier_key nsScriptObjectSpecifier  classDesc container property =
withObjCPtr classDesc $ \raw_classDesc ->
  withObjCPtr container $ \raw_container ->
    withObjCPtr property $ \raw_property ->
        sendMsg nsScriptObjectSpecifier (mkSelector "initWithContainerClassDescription:containerSpecifier:key:") (retPtr retVoid) [argPtr (castPtr raw_classDesc :: Ptr ()), argPtr (castPtr raw_container :: Ptr ()), argPtr (castPtr raw_property :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSScriptObjectSpecifier nsScriptObjectSpecifier, IsNSCoder inCoder) => nsScriptObjectSpecifier -> inCoder -> IO (Id NSScriptObjectSpecifier)
initWithCoder nsScriptObjectSpecifier  inCoder =
withObjCPtr inCoder $ \raw_inCoder ->
    sendMsg nsScriptObjectSpecifier (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_inCoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- indicesOfObjectsByEvaluatingWithContainer:count:@
indicesOfObjectsByEvaluatingWithContainer_count :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> RawId -> Ptr CLong -> IO (Ptr CLong)
indicesOfObjectsByEvaluatingWithContainer_count nsScriptObjectSpecifier  container count =
  fmap castPtr $ sendMsg nsScriptObjectSpecifier (mkSelector "indicesOfObjectsByEvaluatingWithContainer:count:") (retPtr retVoid) [argPtr (castPtr (unRawId container) :: Ptr ()), argPtr count]

-- | @- objectsByEvaluatingWithContainers:@
objectsByEvaluatingWithContainers :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> RawId -> IO RawId
objectsByEvaluatingWithContainers nsScriptObjectSpecifier  containers =
  fmap (RawId . castPtr) $ sendMsg nsScriptObjectSpecifier (mkSelector "objectsByEvaluatingWithContainers:") (retPtr retVoid) [argPtr (castPtr (unRawId containers) :: Ptr ())]

-- | @- childSpecifier@
childSpecifier :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> IO (Id NSScriptObjectSpecifier)
childSpecifier nsScriptObjectSpecifier  =
  sendMsg nsScriptObjectSpecifier (mkSelector "childSpecifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChildSpecifier:@
setChildSpecifier :: (IsNSScriptObjectSpecifier nsScriptObjectSpecifier, IsNSScriptObjectSpecifier value) => nsScriptObjectSpecifier -> value -> IO ()
setChildSpecifier nsScriptObjectSpecifier  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsScriptObjectSpecifier (mkSelector "setChildSpecifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- containerSpecifier@
containerSpecifier :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> IO (Id NSScriptObjectSpecifier)
containerSpecifier nsScriptObjectSpecifier  =
  sendMsg nsScriptObjectSpecifier (mkSelector "containerSpecifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContainerSpecifier:@
setContainerSpecifier :: (IsNSScriptObjectSpecifier nsScriptObjectSpecifier, IsNSScriptObjectSpecifier value) => nsScriptObjectSpecifier -> value -> IO ()
setContainerSpecifier nsScriptObjectSpecifier  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsScriptObjectSpecifier (mkSelector "setContainerSpecifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- containerIsObjectBeingTested@
containerIsObjectBeingTested :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> IO Bool
containerIsObjectBeingTested nsScriptObjectSpecifier  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScriptObjectSpecifier (mkSelector "containerIsObjectBeingTested") retCULong []

-- | @- setContainerIsObjectBeingTested:@
setContainerIsObjectBeingTested :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> Bool -> IO ()
setContainerIsObjectBeingTested nsScriptObjectSpecifier  value =
  sendMsg nsScriptObjectSpecifier (mkSelector "setContainerIsObjectBeingTested:") retVoid [argCULong (if value then 1 else 0)]

-- | @- containerIsRangeContainerObject@
containerIsRangeContainerObject :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> IO Bool
containerIsRangeContainerObject nsScriptObjectSpecifier  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScriptObjectSpecifier (mkSelector "containerIsRangeContainerObject") retCULong []

-- | @- setContainerIsRangeContainerObject:@
setContainerIsRangeContainerObject :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> Bool -> IO ()
setContainerIsRangeContainerObject nsScriptObjectSpecifier  value =
  sendMsg nsScriptObjectSpecifier (mkSelector "setContainerIsRangeContainerObject:") retVoid [argCULong (if value then 1 else 0)]

-- | @- key@
key :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> IO (Id NSString)
key nsScriptObjectSpecifier  =
  sendMsg nsScriptObjectSpecifier (mkSelector "key") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKey:@
setKey :: (IsNSScriptObjectSpecifier nsScriptObjectSpecifier, IsNSString value) => nsScriptObjectSpecifier -> value -> IO ()
setKey nsScriptObjectSpecifier  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsScriptObjectSpecifier (mkSelector "setKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- containerClassDescription@
containerClassDescription :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> IO (Id NSScriptClassDescription)
containerClassDescription nsScriptObjectSpecifier  =
  sendMsg nsScriptObjectSpecifier (mkSelector "containerClassDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContainerClassDescription:@
setContainerClassDescription :: (IsNSScriptObjectSpecifier nsScriptObjectSpecifier, IsNSScriptClassDescription value) => nsScriptObjectSpecifier -> value -> IO ()
setContainerClassDescription nsScriptObjectSpecifier  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsScriptObjectSpecifier (mkSelector "setContainerClassDescription:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- keyClassDescription@
keyClassDescription :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> IO (Id NSScriptClassDescription)
keyClassDescription nsScriptObjectSpecifier  =
  sendMsg nsScriptObjectSpecifier (mkSelector "keyClassDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- objectsByEvaluatingSpecifier@
objectsByEvaluatingSpecifier :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> IO RawId
objectsByEvaluatingSpecifier nsScriptObjectSpecifier  =
  fmap (RawId . castPtr) $ sendMsg nsScriptObjectSpecifier (mkSelector "objectsByEvaluatingSpecifier") (retPtr retVoid) []

-- | @- evaluationErrorNumber@
evaluationErrorNumber :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> IO CLong
evaluationErrorNumber nsScriptObjectSpecifier  =
  sendMsg nsScriptObjectSpecifier (mkSelector "evaluationErrorNumber") retCLong []

-- | @- setEvaluationErrorNumber:@
setEvaluationErrorNumber :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> CLong -> IO ()
setEvaluationErrorNumber nsScriptObjectSpecifier  value =
  sendMsg nsScriptObjectSpecifier (mkSelector "setEvaluationErrorNumber:") retVoid [argCLong (fromIntegral value)]

-- | @- evaluationErrorSpecifier@
evaluationErrorSpecifier :: IsNSScriptObjectSpecifier nsScriptObjectSpecifier => nsScriptObjectSpecifier -> IO (Id NSScriptObjectSpecifier)
evaluationErrorSpecifier nsScriptObjectSpecifier  =
  sendMsg nsScriptObjectSpecifier (mkSelector "evaluationErrorSpecifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectSpecifierWithDescriptor:@
objectSpecifierWithDescriptorSelector :: Selector
objectSpecifierWithDescriptorSelector = mkSelector "objectSpecifierWithDescriptor:"

-- | @Selector@ for @initWithContainerSpecifier:key:@
initWithContainerSpecifier_keySelector :: Selector
initWithContainerSpecifier_keySelector = mkSelector "initWithContainerSpecifier:key:"

-- | @Selector@ for @initWithContainerClassDescription:containerSpecifier:key:@
initWithContainerClassDescription_containerSpecifier_keySelector :: Selector
initWithContainerClassDescription_containerSpecifier_keySelector = mkSelector "initWithContainerClassDescription:containerSpecifier:key:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @indicesOfObjectsByEvaluatingWithContainer:count:@
indicesOfObjectsByEvaluatingWithContainer_countSelector :: Selector
indicesOfObjectsByEvaluatingWithContainer_countSelector = mkSelector "indicesOfObjectsByEvaluatingWithContainer:count:"

-- | @Selector@ for @objectsByEvaluatingWithContainers:@
objectsByEvaluatingWithContainersSelector :: Selector
objectsByEvaluatingWithContainersSelector = mkSelector "objectsByEvaluatingWithContainers:"

-- | @Selector@ for @childSpecifier@
childSpecifierSelector :: Selector
childSpecifierSelector = mkSelector "childSpecifier"

-- | @Selector@ for @setChildSpecifier:@
setChildSpecifierSelector :: Selector
setChildSpecifierSelector = mkSelector "setChildSpecifier:"

-- | @Selector@ for @containerSpecifier@
containerSpecifierSelector :: Selector
containerSpecifierSelector = mkSelector "containerSpecifier"

-- | @Selector@ for @setContainerSpecifier:@
setContainerSpecifierSelector :: Selector
setContainerSpecifierSelector = mkSelector "setContainerSpecifier:"

-- | @Selector@ for @containerIsObjectBeingTested@
containerIsObjectBeingTestedSelector :: Selector
containerIsObjectBeingTestedSelector = mkSelector "containerIsObjectBeingTested"

-- | @Selector@ for @setContainerIsObjectBeingTested:@
setContainerIsObjectBeingTestedSelector :: Selector
setContainerIsObjectBeingTestedSelector = mkSelector "setContainerIsObjectBeingTested:"

-- | @Selector@ for @containerIsRangeContainerObject@
containerIsRangeContainerObjectSelector :: Selector
containerIsRangeContainerObjectSelector = mkSelector "containerIsRangeContainerObject"

-- | @Selector@ for @setContainerIsRangeContainerObject:@
setContainerIsRangeContainerObjectSelector :: Selector
setContainerIsRangeContainerObjectSelector = mkSelector "setContainerIsRangeContainerObject:"

-- | @Selector@ for @key@
keySelector :: Selector
keySelector = mkSelector "key"

-- | @Selector@ for @setKey:@
setKeySelector :: Selector
setKeySelector = mkSelector "setKey:"

-- | @Selector@ for @containerClassDescription@
containerClassDescriptionSelector :: Selector
containerClassDescriptionSelector = mkSelector "containerClassDescription"

-- | @Selector@ for @setContainerClassDescription:@
setContainerClassDescriptionSelector :: Selector
setContainerClassDescriptionSelector = mkSelector "setContainerClassDescription:"

-- | @Selector@ for @keyClassDescription@
keyClassDescriptionSelector :: Selector
keyClassDescriptionSelector = mkSelector "keyClassDescription"

-- | @Selector@ for @objectsByEvaluatingSpecifier@
objectsByEvaluatingSpecifierSelector :: Selector
objectsByEvaluatingSpecifierSelector = mkSelector "objectsByEvaluatingSpecifier"

-- | @Selector@ for @evaluationErrorNumber@
evaluationErrorNumberSelector :: Selector
evaluationErrorNumberSelector = mkSelector "evaluationErrorNumber"

-- | @Selector@ for @setEvaluationErrorNumber:@
setEvaluationErrorNumberSelector :: Selector
setEvaluationErrorNumberSelector = mkSelector "setEvaluationErrorNumber:"

-- | @Selector@ for @evaluationErrorSpecifier@
evaluationErrorSpecifierSelector :: Selector
evaluationErrorSpecifierSelector = mkSelector "evaluationErrorSpecifier"

