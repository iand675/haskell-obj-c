{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAnimationContext@.
module ObjC.AppKit.NSAnimationContext
  ( NSAnimationContext
  , IsNSAnimationContext(..)
  , runAnimationGroup_completionHandler
  , runAnimationGroup
  , beginGrouping
  , endGrouping
  , currentContext
  , duration
  , setDuration
  , timingFunction
  , setTimingFunction
  , completionHandler
  , setCompletionHandler
  , allowsImplicitAnimation
  , setAllowsImplicitAnimation
  , runAnimationGroup_completionHandlerSelector
  , runAnimationGroupSelector
  , beginGroupingSelector
  , endGroupingSelector
  , currentContextSelector
  , durationSelector
  , setDurationSelector
  , timingFunctionSelector
  , setTimingFunctionSelector
  , completionHandlerSelector
  , setCompletionHandlerSelector
  , allowsImplicitAnimationSelector
  , setAllowsImplicitAnimationSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ runAnimationGroup:completionHandler:@
runAnimationGroup_completionHandler :: Ptr () -> Ptr () -> IO ()
runAnimationGroup_completionHandler changes completionHandler =
  do
    cls' <- getRequiredClass "NSAnimationContext"
    sendClassMsg cls' (mkSelector "runAnimationGroup:completionHandler:") retVoid [argPtr (castPtr changes :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @+ runAnimationGroup:@
runAnimationGroup :: Ptr () -> IO ()
runAnimationGroup changes =
  do
    cls' <- getRequiredClass "NSAnimationContext"
    sendClassMsg cls' (mkSelector "runAnimationGroup:") retVoid [argPtr (castPtr changes :: Ptr ())]

-- | @+ beginGrouping@
beginGrouping :: IO ()
beginGrouping  =
  do
    cls' <- getRequiredClass "NSAnimationContext"
    sendClassMsg cls' (mkSelector "beginGrouping") retVoid []

-- | @+ endGrouping@
endGrouping :: IO ()
endGrouping  =
  do
    cls' <- getRequiredClass "NSAnimationContext"
    sendClassMsg cls' (mkSelector "endGrouping") retVoid []

-- | @+ currentContext@
currentContext :: IO (Id NSAnimationContext)
currentContext  =
  do
    cls' <- getRequiredClass "NSAnimationContext"
    sendClassMsg cls' (mkSelector "currentContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- duration@
duration :: IsNSAnimationContext nsAnimationContext => nsAnimationContext -> IO CDouble
duration nsAnimationContext  =
    sendMsg nsAnimationContext (mkSelector "duration") retCDouble []

-- | @- setDuration:@
setDuration :: IsNSAnimationContext nsAnimationContext => nsAnimationContext -> CDouble -> IO ()
setDuration nsAnimationContext  value =
    sendMsg nsAnimationContext (mkSelector "setDuration:") retVoid [argCDouble value]

-- | @- timingFunction@
timingFunction :: IsNSAnimationContext nsAnimationContext => nsAnimationContext -> IO (Id CAMediaTimingFunction)
timingFunction nsAnimationContext  =
    sendMsg nsAnimationContext (mkSelector "timingFunction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTimingFunction:@
setTimingFunction :: (IsNSAnimationContext nsAnimationContext, IsCAMediaTimingFunction value) => nsAnimationContext -> value -> IO ()
setTimingFunction nsAnimationContext  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsAnimationContext (mkSelector "setTimingFunction:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- completionHandler@
completionHandler :: IsNSAnimationContext nsAnimationContext => nsAnimationContext -> IO (Ptr ())
completionHandler nsAnimationContext  =
    fmap castPtr $ sendMsg nsAnimationContext (mkSelector "completionHandler") (retPtr retVoid) []

-- | @- setCompletionHandler:@
setCompletionHandler :: IsNSAnimationContext nsAnimationContext => nsAnimationContext -> Ptr () -> IO ()
setCompletionHandler nsAnimationContext  value =
    sendMsg nsAnimationContext (mkSelector "setCompletionHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- allowsImplicitAnimation@
allowsImplicitAnimation :: IsNSAnimationContext nsAnimationContext => nsAnimationContext -> IO Bool
allowsImplicitAnimation nsAnimationContext  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsAnimationContext (mkSelector "allowsImplicitAnimation") retCULong []

-- | @- setAllowsImplicitAnimation:@
setAllowsImplicitAnimation :: IsNSAnimationContext nsAnimationContext => nsAnimationContext -> Bool -> IO ()
setAllowsImplicitAnimation nsAnimationContext  value =
    sendMsg nsAnimationContext (mkSelector "setAllowsImplicitAnimation:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @runAnimationGroup:completionHandler:@
runAnimationGroup_completionHandlerSelector :: Selector
runAnimationGroup_completionHandlerSelector = mkSelector "runAnimationGroup:completionHandler:"

-- | @Selector@ for @runAnimationGroup:@
runAnimationGroupSelector :: Selector
runAnimationGroupSelector = mkSelector "runAnimationGroup:"

-- | @Selector@ for @beginGrouping@
beginGroupingSelector :: Selector
beginGroupingSelector = mkSelector "beginGrouping"

-- | @Selector@ for @endGrouping@
endGroupingSelector :: Selector
endGroupingSelector = mkSelector "endGrouping"

-- | @Selector@ for @currentContext@
currentContextSelector :: Selector
currentContextSelector = mkSelector "currentContext"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @timingFunction@
timingFunctionSelector :: Selector
timingFunctionSelector = mkSelector "timingFunction"

-- | @Selector@ for @setTimingFunction:@
setTimingFunctionSelector :: Selector
setTimingFunctionSelector = mkSelector "setTimingFunction:"

-- | @Selector@ for @completionHandler@
completionHandlerSelector :: Selector
completionHandlerSelector = mkSelector "completionHandler"

-- | @Selector@ for @setCompletionHandler:@
setCompletionHandlerSelector :: Selector
setCompletionHandlerSelector = mkSelector "setCompletionHandler:"

-- | @Selector@ for @allowsImplicitAnimation@
allowsImplicitAnimationSelector :: Selector
allowsImplicitAnimationSelector = mkSelector "allowsImplicitAnimation"

-- | @Selector@ for @setAllowsImplicitAnimation:@
setAllowsImplicitAnimationSelector :: Selector
setAllowsImplicitAnimationSelector = mkSelector "setAllowsImplicitAnimation:"

