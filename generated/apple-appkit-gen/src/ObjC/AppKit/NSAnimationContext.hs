{-# LANGUAGE DataKinds #-}
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
  , allowsImplicitAnimationSelector
  , beginGroupingSelector
  , completionHandlerSelector
  , currentContextSelector
  , durationSelector
  , endGroupingSelector
  , runAnimationGroupSelector
  , runAnimationGroup_completionHandlerSelector
  , setAllowsImplicitAnimationSelector
  , setCompletionHandlerSelector
  , setDurationSelector
  , setTimingFunctionSelector
  , timingFunctionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ runAnimationGroup:completionHandler:@
runAnimationGroup_completionHandler :: Ptr () -> Ptr () -> IO ()
runAnimationGroup_completionHandler changes completionHandler =
  do
    cls' <- getRequiredClass "NSAnimationContext"
    sendClassMessage cls' runAnimationGroup_completionHandlerSelector changes completionHandler

-- | @+ runAnimationGroup:@
runAnimationGroup :: Ptr () -> IO ()
runAnimationGroup changes =
  do
    cls' <- getRequiredClass "NSAnimationContext"
    sendClassMessage cls' runAnimationGroupSelector changes

-- | @+ beginGrouping@
beginGrouping :: IO ()
beginGrouping  =
  do
    cls' <- getRequiredClass "NSAnimationContext"
    sendClassMessage cls' beginGroupingSelector

-- | @+ endGrouping@
endGrouping :: IO ()
endGrouping  =
  do
    cls' <- getRequiredClass "NSAnimationContext"
    sendClassMessage cls' endGroupingSelector

-- | @+ currentContext@
currentContext :: IO (Id NSAnimationContext)
currentContext  =
  do
    cls' <- getRequiredClass "NSAnimationContext"
    sendClassMessage cls' currentContextSelector

-- | @- duration@
duration :: IsNSAnimationContext nsAnimationContext => nsAnimationContext -> IO CDouble
duration nsAnimationContext =
  sendMessage nsAnimationContext durationSelector

-- | @- setDuration:@
setDuration :: IsNSAnimationContext nsAnimationContext => nsAnimationContext -> CDouble -> IO ()
setDuration nsAnimationContext value =
  sendMessage nsAnimationContext setDurationSelector value

-- | @- timingFunction@
timingFunction :: IsNSAnimationContext nsAnimationContext => nsAnimationContext -> IO (Id CAMediaTimingFunction)
timingFunction nsAnimationContext =
  sendMessage nsAnimationContext timingFunctionSelector

-- | @- setTimingFunction:@
setTimingFunction :: (IsNSAnimationContext nsAnimationContext, IsCAMediaTimingFunction value) => nsAnimationContext -> value -> IO ()
setTimingFunction nsAnimationContext value =
  sendMessage nsAnimationContext setTimingFunctionSelector (toCAMediaTimingFunction value)

-- | @- completionHandler@
completionHandler :: IsNSAnimationContext nsAnimationContext => nsAnimationContext -> IO (Ptr ())
completionHandler nsAnimationContext =
  sendMessage nsAnimationContext completionHandlerSelector

-- | @- setCompletionHandler:@
setCompletionHandler :: IsNSAnimationContext nsAnimationContext => nsAnimationContext -> Ptr () -> IO ()
setCompletionHandler nsAnimationContext value =
  sendMessage nsAnimationContext setCompletionHandlerSelector value

-- | @- allowsImplicitAnimation@
allowsImplicitAnimation :: IsNSAnimationContext nsAnimationContext => nsAnimationContext -> IO Bool
allowsImplicitAnimation nsAnimationContext =
  sendMessage nsAnimationContext allowsImplicitAnimationSelector

-- | @- setAllowsImplicitAnimation:@
setAllowsImplicitAnimation :: IsNSAnimationContext nsAnimationContext => nsAnimationContext -> Bool -> IO ()
setAllowsImplicitAnimation nsAnimationContext value =
  sendMessage nsAnimationContext setAllowsImplicitAnimationSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @runAnimationGroup:completionHandler:@
runAnimationGroup_completionHandlerSelector :: Selector '[Ptr (), Ptr ()] ()
runAnimationGroup_completionHandlerSelector = mkSelector "runAnimationGroup:completionHandler:"

-- | @Selector@ for @runAnimationGroup:@
runAnimationGroupSelector :: Selector '[Ptr ()] ()
runAnimationGroupSelector = mkSelector "runAnimationGroup:"

-- | @Selector@ for @beginGrouping@
beginGroupingSelector :: Selector '[] ()
beginGroupingSelector = mkSelector "beginGrouping"

-- | @Selector@ for @endGrouping@
endGroupingSelector :: Selector '[] ()
endGroupingSelector = mkSelector "endGrouping"

-- | @Selector@ for @currentContext@
currentContextSelector :: Selector '[] (Id NSAnimationContext)
currentContextSelector = mkSelector "currentContext"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector '[CDouble] ()
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @timingFunction@
timingFunctionSelector :: Selector '[] (Id CAMediaTimingFunction)
timingFunctionSelector = mkSelector "timingFunction"

-- | @Selector@ for @setTimingFunction:@
setTimingFunctionSelector :: Selector '[Id CAMediaTimingFunction] ()
setTimingFunctionSelector = mkSelector "setTimingFunction:"

-- | @Selector@ for @completionHandler@
completionHandlerSelector :: Selector '[] (Ptr ())
completionHandlerSelector = mkSelector "completionHandler"

-- | @Selector@ for @setCompletionHandler:@
setCompletionHandlerSelector :: Selector '[Ptr ()] ()
setCompletionHandlerSelector = mkSelector "setCompletionHandler:"

-- | @Selector@ for @allowsImplicitAnimation@
allowsImplicitAnimationSelector :: Selector '[] Bool
allowsImplicitAnimationSelector = mkSelector "allowsImplicitAnimation"

-- | @Selector@ for @setAllowsImplicitAnimation:@
setAllowsImplicitAnimationSelector :: Selector '[Bool] ()
setAllowsImplicitAnimationSelector = mkSelector "setAllowsImplicitAnimation:"

