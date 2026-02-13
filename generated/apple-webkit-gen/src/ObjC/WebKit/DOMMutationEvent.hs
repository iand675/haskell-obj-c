{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMMutationEvent@.
module ObjC.WebKit.DOMMutationEvent
  ( DOMMutationEvent
  , IsDOMMutationEvent(..)
  , newValue
  , initMutationEvent_canBubble_cancelable_relatedNode_prevValue_newValue_attrName_attrChange
  , initMutationEvent
  , relatedNode
  , prevValue
  , attrName
  , attrChange
  , attrChangeSelector
  , attrNameSelector
  , initMutationEventSelector
  , initMutationEvent_canBubble_cancelable_relatedNode_prevValue_newValue_attrName_attrChangeSelector
  , newValueSelector
  , prevValueSelector
  , relatedNodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- newValue@
newValue :: IsDOMMutationEvent domMutationEvent => domMutationEvent -> IO (Id NSString)
newValue domMutationEvent =
  sendOwnedMessage domMutationEvent newValueSelector

-- | @- initMutationEvent:canBubble:cancelable:relatedNode:prevValue:newValue:attrName:attrChange:@
initMutationEvent_canBubble_cancelable_relatedNode_prevValue_newValue_attrName_attrChange :: (IsDOMMutationEvent domMutationEvent, IsNSString type_, IsDOMNode relatedNode, IsNSString prevValue, IsNSString newValue, IsNSString attrName) => domMutationEvent -> type_ -> Bool -> Bool -> relatedNode -> prevValue -> newValue -> attrName -> CUShort -> IO ()
initMutationEvent_canBubble_cancelable_relatedNode_prevValue_newValue_attrName_attrChange domMutationEvent type_ canBubble cancelable relatedNode prevValue newValue attrName attrChange =
  sendOwnedMessage domMutationEvent initMutationEvent_canBubble_cancelable_relatedNode_prevValue_newValue_attrName_attrChangeSelector (toNSString type_) canBubble cancelable (toDOMNode relatedNode) (toNSString prevValue) (toNSString newValue) (toNSString attrName) attrChange

-- | @- initMutationEvent::::::::@
initMutationEvent :: (IsDOMMutationEvent domMutationEvent, IsNSString type_, IsDOMNode relatedNode, IsNSString prevValue, IsNSString newValue, IsNSString attrName) => domMutationEvent -> type_ -> Bool -> Bool -> relatedNode -> prevValue -> newValue -> attrName -> CUShort -> IO ()
initMutationEvent domMutationEvent type_ canBubble cancelable relatedNode prevValue newValue attrName attrChange =
  sendOwnedMessage domMutationEvent initMutationEventSelector (toNSString type_) canBubble cancelable (toDOMNode relatedNode) (toNSString prevValue) (toNSString newValue) (toNSString attrName) attrChange

-- | @- relatedNode@
relatedNode :: IsDOMMutationEvent domMutationEvent => domMutationEvent -> IO (Id DOMNode)
relatedNode domMutationEvent =
  sendMessage domMutationEvent relatedNodeSelector

-- | @- prevValue@
prevValue :: IsDOMMutationEvent domMutationEvent => domMutationEvent -> IO (Id NSString)
prevValue domMutationEvent =
  sendMessage domMutationEvent prevValueSelector

-- | @- attrName@
attrName :: IsDOMMutationEvent domMutationEvent => domMutationEvent -> IO (Id NSString)
attrName domMutationEvent =
  sendMessage domMutationEvent attrNameSelector

-- | @- attrChange@
attrChange :: IsDOMMutationEvent domMutationEvent => domMutationEvent -> IO CUShort
attrChange domMutationEvent =
  sendMessage domMutationEvent attrChangeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newValue@
newValueSelector :: Selector '[] (Id NSString)
newValueSelector = mkSelector "newValue"

-- | @Selector@ for @initMutationEvent:canBubble:cancelable:relatedNode:prevValue:newValue:attrName:attrChange:@
initMutationEvent_canBubble_cancelable_relatedNode_prevValue_newValue_attrName_attrChangeSelector :: Selector '[Id NSString, Bool, Bool, Id DOMNode, Id NSString, Id NSString, Id NSString, CUShort] ()
initMutationEvent_canBubble_cancelable_relatedNode_prevValue_newValue_attrName_attrChangeSelector = mkSelector "initMutationEvent:canBubble:cancelable:relatedNode:prevValue:newValue:attrName:attrChange:"

-- | @Selector@ for @initMutationEvent::::::::@
initMutationEventSelector :: Selector '[Id NSString, Bool, Bool, Id DOMNode, Id NSString, Id NSString, Id NSString, CUShort] ()
initMutationEventSelector = mkSelector "initMutationEvent::::::::"

-- | @Selector@ for @relatedNode@
relatedNodeSelector :: Selector '[] (Id DOMNode)
relatedNodeSelector = mkSelector "relatedNode"

-- | @Selector@ for @prevValue@
prevValueSelector :: Selector '[] (Id NSString)
prevValueSelector = mkSelector "prevValue"

-- | @Selector@ for @attrName@
attrNameSelector :: Selector '[] (Id NSString)
attrNameSelector = mkSelector "attrName"

-- | @Selector@ for @attrChange@
attrChangeSelector :: Selector '[] CUShort
attrChangeSelector = mkSelector "attrChange"

