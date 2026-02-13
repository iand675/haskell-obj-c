{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSInvocation@.
module ObjC.Foundation.NSInvocation
  ( NSInvocation
  , IsNSInvocation(..)
  , invocationWithMethodSignature
  , retainArguments
  , getReturnValue
  , setReturnValue
  , getArgument_atIndex
  , setArgument_atIndex
  , invoke
  , invokeWithTarget
  , invokeUsingIMP
  , methodSignature
  , argumentsRetained
  , target
  , setTarget
  , selector
  , setSelector
  , argumentsRetainedSelector
  , getArgument_atIndexSelector
  , getReturnValueSelector
  , invocationWithMethodSignatureSelector
  , invokeSelector
  , invokeUsingIMPSelector
  , invokeWithTargetSelector
  , methodSignatureSelector
  , retainArgumentsSelector
  , selectorSelector
  , setArgument_atIndexSelector
  , setReturnValueSelector
  , setSelectorSelector
  , setTargetSelector
  , targetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ invocationWithMethodSignature:@
invocationWithMethodSignature :: IsNSMethodSignature sig => sig -> IO (Id NSInvocation)
invocationWithMethodSignature sig =
  do
    cls' <- getRequiredClass "NSInvocation"
    sendClassMessage cls' invocationWithMethodSignatureSelector (toNSMethodSignature sig)

-- | @- retainArguments@
retainArguments :: IsNSInvocation nsInvocation => nsInvocation -> IO ()
retainArguments nsInvocation =
  sendMessage nsInvocation retainArgumentsSelector

-- | @- getReturnValue:@
getReturnValue :: IsNSInvocation nsInvocation => nsInvocation -> Ptr () -> IO ()
getReturnValue nsInvocation retLoc =
  sendMessage nsInvocation getReturnValueSelector retLoc

-- | @- setReturnValue:@
setReturnValue :: IsNSInvocation nsInvocation => nsInvocation -> Ptr () -> IO ()
setReturnValue nsInvocation retLoc =
  sendMessage nsInvocation setReturnValueSelector retLoc

-- | @- getArgument:atIndex:@
getArgument_atIndex :: IsNSInvocation nsInvocation => nsInvocation -> Ptr () -> CLong -> IO ()
getArgument_atIndex nsInvocation argumentLocation idx =
  sendMessage nsInvocation getArgument_atIndexSelector argumentLocation idx

-- | @- setArgument:atIndex:@
setArgument_atIndex :: IsNSInvocation nsInvocation => nsInvocation -> Ptr () -> CLong -> IO ()
setArgument_atIndex nsInvocation argumentLocation idx =
  sendMessage nsInvocation setArgument_atIndexSelector argumentLocation idx

-- | @- invoke@
invoke :: IsNSInvocation nsInvocation => nsInvocation -> IO ()
invoke nsInvocation =
  sendMessage nsInvocation invokeSelector

-- | @- invokeWithTarget:@
invokeWithTarget :: IsNSInvocation nsInvocation => nsInvocation -> RawId -> IO ()
invokeWithTarget nsInvocation target =
  sendMessage nsInvocation invokeWithTargetSelector target

-- | @- invokeUsingIMP:@
invokeUsingIMP :: IsNSInvocation nsInvocation => nsInvocation -> Ptr () -> IO ()
invokeUsingIMP nsInvocation imp =
  sendMessage nsInvocation invokeUsingIMPSelector imp

-- | @- methodSignature@
methodSignature :: IsNSInvocation nsInvocation => nsInvocation -> IO (Id NSMethodSignature)
methodSignature nsInvocation =
  sendMessage nsInvocation methodSignatureSelector

-- | @- argumentsRetained@
argumentsRetained :: IsNSInvocation nsInvocation => nsInvocation -> IO Bool
argumentsRetained nsInvocation =
  sendMessage nsInvocation argumentsRetainedSelector

-- | @- target@
target :: IsNSInvocation nsInvocation => nsInvocation -> IO RawId
target nsInvocation =
  sendMessage nsInvocation targetSelector

-- | @- setTarget:@
setTarget :: IsNSInvocation nsInvocation => nsInvocation -> RawId -> IO ()
setTarget nsInvocation value =
  sendMessage nsInvocation setTargetSelector value

-- | @- selector@
selector :: IsNSInvocation nsInvocation => nsInvocation -> IO Sel
selector nsInvocation =
  sendMessage nsInvocation selectorSelector

-- | @- setSelector:@
setSelector :: IsNSInvocation nsInvocation => nsInvocation -> Sel -> IO ()
setSelector nsInvocation value =
  sendMessage nsInvocation setSelectorSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @invocationWithMethodSignature:@
invocationWithMethodSignatureSelector :: Selector '[Id NSMethodSignature] (Id NSInvocation)
invocationWithMethodSignatureSelector = mkSelector "invocationWithMethodSignature:"

-- | @Selector@ for @retainArguments@
retainArgumentsSelector :: Selector '[] ()
retainArgumentsSelector = mkSelector "retainArguments"

-- | @Selector@ for @getReturnValue:@
getReturnValueSelector :: Selector '[Ptr ()] ()
getReturnValueSelector = mkSelector "getReturnValue:"

-- | @Selector@ for @setReturnValue:@
setReturnValueSelector :: Selector '[Ptr ()] ()
setReturnValueSelector = mkSelector "setReturnValue:"

-- | @Selector@ for @getArgument:atIndex:@
getArgument_atIndexSelector :: Selector '[Ptr (), CLong] ()
getArgument_atIndexSelector = mkSelector "getArgument:atIndex:"

-- | @Selector@ for @setArgument:atIndex:@
setArgument_atIndexSelector :: Selector '[Ptr (), CLong] ()
setArgument_atIndexSelector = mkSelector "setArgument:atIndex:"

-- | @Selector@ for @invoke@
invokeSelector :: Selector '[] ()
invokeSelector = mkSelector "invoke"

-- | @Selector@ for @invokeWithTarget:@
invokeWithTargetSelector :: Selector '[RawId] ()
invokeWithTargetSelector = mkSelector "invokeWithTarget:"

-- | @Selector@ for @invokeUsingIMP:@
invokeUsingIMPSelector :: Selector '[Ptr ()] ()
invokeUsingIMPSelector = mkSelector "invokeUsingIMP:"

-- | @Selector@ for @methodSignature@
methodSignatureSelector :: Selector '[] (Id NSMethodSignature)
methodSignatureSelector = mkSelector "methodSignature"

-- | @Selector@ for @argumentsRetained@
argumentsRetainedSelector :: Selector '[] Bool
argumentsRetainedSelector = mkSelector "argumentsRetained"

-- | @Selector@ for @target@
targetSelector :: Selector '[] RawId
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector '[RawId] ()
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @selector@
selectorSelector :: Selector '[] Sel
selectorSelector = mkSelector "selector"

-- | @Selector@ for @setSelector:@
setSelectorSelector :: Selector '[Sel] ()
setSelectorSelector = mkSelector "setSelector:"

