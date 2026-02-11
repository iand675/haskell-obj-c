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
  , newValueSelector
  , initMutationEvent_canBubble_cancelable_relatedNode_prevValue_newValue_attrName_attrChangeSelector
  , initMutationEventSelector
  , relatedNodeSelector
  , prevValueSelector
  , attrNameSelector
  , attrChangeSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- newValue@
newValue :: IsDOMMutationEvent domMutationEvent => domMutationEvent -> IO (Id NSString)
newValue domMutationEvent  =
  sendMsg domMutationEvent (mkSelector "newValue") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initMutationEvent:canBubble:cancelable:relatedNode:prevValue:newValue:attrName:attrChange:@
initMutationEvent_canBubble_cancelable_relatedNode_prevValue_newValue_attrName_attrChange :: (IsDOMMutationEvent domMutationEvent, IsNSString type_, IsDOMNode relatedNode, IsNSString prevValue, IsNSString newValue, IsNSString attrName) => domMutationEvent -> type_ -> Bool -> Bool -> relatedNode -> prevValue -> newValue -> attrName -> CUShort -> IO ()
initMutationEvent_canBubble_cancelable_relatedNode_prevValue_newValue_attrName_attrChange domMutationEvent  type_ canBubble cancelable relatedNode prevValue newValue attrName attrChange =
withObjCPtr type_ $ \raw_type_ ->
  withObjCPtr relatedNode $ \raw_relatedNode ->
    withObjCPtr prevValue $ \raw_prevValue ->
      withObjCPtr newValue $ \raw_newValue ->
        withObjCPtr attrName $ \raw_attrName ->
            sendMsg domMutationEvent (mkSelector "initMutationEvent:canBubble:cancelable:relatedNode:prevValue:newValue:attrName:attrChange:") retVoid [argPtr (castPtr raw_type_ :: Ptr ()), argCULong (if canBubble then 1 else 0), argCULong (if cancelable then 1 else 0), argPtr (castPtr raw_relatedNode :: Ptr ()), argPtr (castPtr raw_prevValue :: Ptr ()), argPtr (castPtr raw_newValue :: Ptr ()), argPtr (castPtr raw_attrName :: Ptr ()), argCUInt (fromIntegral attrChange)]

-- | @- initMutationEvent::::::::@
initMutationEvent :: (IsDOMMutationEvent domMutationEvent, IsNSString type_, IsDOMNode relatedNode, IsNSString prevValue, IsNSString newValue, IsNSString attrName) => domMutationEvent -> type_ -> Bool -> Bool -> relatedNode -> prevValue -> newValue -> attrName -> CUShort -> IO ()
initMutationEvent domMutationEvent  type_ canBubble cancelable relatedNode prevValue newValue attrName attrChange =
withObjCPtr type_ $ \raw_type_ ->
  withObjCPtr relatedNode $ \raw_relatedNode ->
    withObjCPtr prevValue $ \raw_prevValue ->
      withObjCPtr newValue $ \raw_newValue ->
        withObjCPtr attrName $ \raw_attrName ->
            sendMsg domMutationEvent (mkSelector "initMutationEvent::::::::") retVoid [argPtr (castPtr raw_type_ :: Ptr ()), argCULong (if canBubble then 1 else 0), argCULong (if cancelable then 1 else 0), argPtr (castPtr raw_relatedNode :: Ptr ()), argPtr (castPtr raw_prevValue :: Ptr ()), argPtr (castPtr raw_newValue :: Ptr ()), argPtr (castPtr raw_attrName :: Ptr ()), argCUInt (fromIntegral attrChange)]

-- | @- relatedNode@
relatedNode :: IsDOMMutationEvent domMutationEvent => domMutationEvent -> IO (Id DOMNode)
relatedNode domMutationEvent  =
  sendMsg domMutationEvent (mkSelector "relatedNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- prevValue@
prevValue :: IsDOMMutationEvent domMutationEvent => domMutationEvent -> IO (Id NSString)
prevValue domMutationEvent  =
  sendMsg domMutationEvent (mkSelector "prevValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- attrName@
attrName :: IsDOMMutationEvent domMutationEvent => domMutationEvent -> IO (Id NSString)
attrName domMutationEvent  =
  sendMsg domMutationEvent (mkSelector "attrName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- attrChange@
attrChange :: IsDOMMutationEvent domMutationEvent => domMutationEvent -> IO CUShort
attrChange domMutationEvent  =
  fmap fromIntegral $ sendMsg domMutationEvent (mkSelector "attrChange") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newValue@
newValueSelector :: Selector
newValueSelector = mkSelector "newValue"

-- | @Selector@ for @initMutationEvent:canBubble:cancelable:relatedNode:prevValue:newValue:attrName:attrChange:@
initMutationEvent_canBubble_cancelable_relatedNode_prevValue_newValue_attrName_attrChangeSelector :: Selector
initMutationEvent_canBubble_cancelable_relatedNode_prevValue_newValue_attrName_attrChangeSelector = mkSelector "initMutationEvent:canBubble:cancelable:relatedNode:prevValue:newValue:attrName:attrChange:"

-- | @Selector@ for @initMutationEvent::::::::@
initMutationEventSelector :: Selector
initMutationEventSelector = mkSelector "initMutationEvent::::::::"

-- | @Selector@ for @relatedNode@
relatedNodeSelector :: Selector
relatedNodeSelector = mkSelector "relatedNode"

-- | @Selector@ for @prevValue@
prevValueSelector :: Selector
prevValueSelector = mkSelector "prevValue"

-- | @Selector@ for @attrName@
attrNameSelector :: Selector
attrNameSelector = mkSelector "attrName"

-- | @Selector@ for @attrChange@
attrChangeSelector :: Selector
attrChangeSelector = mkSelector "attrChange"

