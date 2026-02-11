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
  , invocationWithMethodSignatureSelector
  , retainArgumentsSelector
  , getReturnValueSelector
  , setReturnValueSelector
  , getArgument_atIndexSelector
  , setArgument_atIndexSelector
  , invokeSelector
  , invokeWithTargetSelector
  , invokeUsingIMPSelector
  , methodSignatureSelector
  , argumentsRetainedSelector
  , targetSelector
  , setTargetSelector
  , selectorSelector
  , setSelectorSelector


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

-- | @+ invocationWithMethodSignature:@
invocationWithMethodSignature :: IsNSMethodSignature sig => sig -> IO (Id NSInvocation)
invocationWithMethodSignature sig =
  do
    cls' <- getRequiredClass "NSInvocation"
    withObjCPtr sig $ \raw_sig ->
      sendClassMsg cls' (mkSelector "invocationWithMethodSignature:") (retPtr retVoid) [argPtr (castPtr raw_sig :: Ptr ())] >>= retainedObject . castPtr

-- | @- retainArguments@
retainArguments :: IsNSInvocation nsInvocation => nsInvocation -> IO ()
retainArguments nsInvocation  =
  sendMsg nsInvocation (mkSelector "retainArguments") retVoid []

-- | @- getReturnValue:@
getReturnValue :: IsNSInvocation nsInvocation => nsInvocation -> Ptr () -> IO ()
getReturnValue nsInvocation  retLoc =
  sendMsg nsInvocation (mkSelector "getReturnValue:") retVoid [argPtr retLoc]

-- | @- setReturnValue:@
setReturnValue :: IsNSInvocation nsInvocation => nsInvocation -> Ptr () -> IO ()
setReturnValue nsInvocation  retLoc =
  sendMsg nsInvocation (mkSelector "setReturnValue:") retVoid [argPtr retLoc]

-- | @- getArgument:atIndex:@
getArgument_atIndex :: IsNSInvocation nsInvocation => nsInvocation -> Ptr () -> CLong -> IO ()
getArgument_atIndex nsInvocation  argumentLocation idx =
  sendMsg nsInvocation (mkSelector "getArgument:atIndex:") retVoid [argPtr argumentLocation, argCLong (fromIntegral idx)]

-- | @- setArgument:atIndex:@
setArgument_atIndex :: IsNSInvocation nsInvocation => nsInvocation -> Ptr () -> CLong -> IO ()
setArgument_atIndex nsInvocation  argumentLocation idx =
  sendMsg nsInvocation (mkSelector "setArgument:atIndex:") retVoid [argPtr argumentLocation, argCLong (fromIntegral idx)]

-- | @- invoke@
invoke :: IsNSInvocation nsInvocation => nsInvocation -> IO ()
invoke nsInvocation  =
  sendMsg nsInvocation (mkSelector "invoke") retVoid []

-- | @- invokeWithTarget:@
invokeWithTarget :: IsNSInvocation nsInvocation => nsInvocation -> RawId -> IO ()
invokeWithTarget nsInvocation  target =
  sendMsg nsInvocation (mkSelector "invokeWithTarget:") retVoid [argPtr (castPtr (unRawId target) :: Ptr ())]

-- | @- invokeUsingIMP:@
invokeUsingIMP :: IsNSInvocation nsInvocation => nsInvocation -> Ptr () -> IO ()
invokeUsingIMP nsInvocation  imp =
  sendMsg nsInvocation (mkSelector "invokeUsingIMP:") retVoid [argPtr imp]

-- | @- methodSignature@
methodSignature :: IsNSInvocation nsInvocation => nsInvocation -> IO (Id NSMethodSignature)
methodSignature nsInvocation  =
  sendMsg nsInvocation (mkSelector "methodSignature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- argumentsRetained@
argumentsRetained :: IsNSInvocation nsInvocation => nsInvocation -> IO Bool
argumentsRetained nsInvocation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsInvocation (mkSelector "argumentsRetained") retCULong []

-- | @- target@
target :: IsNSInvocation nsInvocation => nsInvocation -> IO RawId
target nsInvocation  =
  fmap (RawId . castPtr) $ sendMsg nsInvocation (mkSelector "target") (retPtr retVoid) []

-- | @- setTarget:@
setTarget :: IsNSInvocation nsInvocation => nsInvocation -> RawId -> IO ()
setTarget nsInvocation  value =
  sendMsg nsInvocation (mkSelector "setTarget:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- selector@
selector :: IsNSInvocation nsInvocation => nsInvocation -> IO Selector
selector nsInvocation  =
  fmap (Selector . castPtr) $ sendMsg nsInvocation (mkSelector "selector") (retPtr retVoid) []

-- | @- setSelector:@
setSelector :: IsNSInvocation nsInvocation => nsInvocation -> Selector -> IO ()
setSelector nsInvocation  value =
  sendMsg nsInvocation (mkSelector "setSelector:") retVoid [argPtr (unSelector value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @invocationWithMethodSignature:@
invocationWithMethodSignatureSelector :: Selector
invocationWithMethodSignatureSelector = mkSelector "invocationWithMethodSignature:"

-- | @Selector@ for @retainArguments@
retainArgumentsSelector :: Selector
retainArgumentsSelector = mkSelector "retainArguments"

-- | @Selector@ for @getReturnValue:@
getReturnValueSelector :: Selector
getReturnValueSelector = mkSelector "getReturnValue:"

-- | @Selector@ for @setReturnValue:@
setReturnValueSelector :: Selector
setReturnValueSelector = mkSelector "setReturnValue:"

-- | @Selector@ for @getArgument:atIndex:@
getArgument_atIndexSelector :: Selector
getArgument_atIndexSelector = mkSelector "getArgument:atIndex:"

-- | @Selector@ for @setArgument:atIndex:@
setArgument_atIndexSelector :: Selector
setArgument_atIndexSelector = mkSelector "setArgument:atIndex:"

-- | @Selector@ for @invoke@
invokeSelector :: Selector
invokeSelector = mkSelector "invoke"

-- | @Selector@ for @invokeWithTarget:@
invokeWithTargetSelector :: Selector
invokeWithTargetSelector = mkSelector "invokeWithTarget:"

-- | @Selector@ for @invokeUsingIMP:@
invokeUsingIMPSelector :: Selector
invokeUsingIMPSelector = mkSelector "invokeUsingIMP:"

-- | @Selector@ for @methodSignature@
methodSignatureSelector :: Selector
methodSignatureSelector = mkSelector "methodSignature"

-- | @Selector@ for @argumentsRetained@
argumentsRetainedSelector :: Selector
argumentsRetainedSelector = mkSelector "argumentsRetained"

-- | @Selector@ for @target@
targetSelector :: Selector
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @selector@
selectorSelector :: Selector
selectorSelector = mkSelector "selector"

-- | @Selector@ for @setSelector:@
setSelectorSelector :: Selector
setSelectorSelector = mkSelector "setSelector:"

