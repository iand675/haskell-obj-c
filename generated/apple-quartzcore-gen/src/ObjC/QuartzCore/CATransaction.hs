{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CATransaction@.
module ObjC.QuartzCore.CATransaction
  ( CATransaction
  , IsCATransaction(..)
  , begin
  , commit
  , flush
  , lock
  , unlock
  , animationDuration
  , setAnimationDuration
  , animationTimingFunction
  , setAnimationTimingFunction
  , disableActions
  , setDisableActions
  , completionBlock
  , setCompletionBlock
  , valueForKey
  , setValue_forKey
  , animationDurationSelector
  , animationTimingFunctionSelector
  , beginSelector
  , commitSelector
  , completionBlockSelector
  , disableActionsSelector
  , flushSelector
  , lockSelector
  , setAnimationDurationSelector
  , setAnimationTimingFunctionSelector
  , setCompletionBlockSelector
  , setDisableActionsSelector
  , setValue_forKeySelector
  , unlockSelector
  , valueForKeySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ begin@
begin :: IO ()
begin  =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMessage cls' beginSelector

-- | @+ commit@
commit :: IO ()
commit  =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMessage cls' commitSelector

-- | @+ flush@
flush :: IO ()
flush  =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMessage cls' flushSelector

-- | @+ lock@
lock :: IO ()
lock  =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMessage cls' lockSelector

-- | @+ unlock@
unlock :: IO ()
unlock  =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMessage cls' unlockSelector

-- | @+ animationDuration@
animationDuration :: IO CDouble
animationDuration  =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMessage cls' animationDurationSelector

-- | @+ setAnimationDuration:@
setAnimationDuration :: CDouble -> IO ()
setAnimationDuration dur =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMessage cls' setAnimationDurationSelector dur

-- | @+ animationTimingFunction@
animationTimingFunction :: IO (Id CAMediaTimingFunction)
animationTimingFunction  =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMessage cls' animationTimingFunctionSelector

-- | @+ setAnimationTimingFunction:@
setAnimationTimingFunction :: IsCAMediaTimingFunction function => function -> IO ()
setAnimationTimingFunction function =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMessage cls' setAnimationTimingFunctionSelector (toCAMediaTimingFunction function)

-- | @+ disableActions@
disableActions :: IO Bool
disableActions  =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMessage cls' disableActionsSelector

-- | @+ setDisableActions:@
setDisableActions :: Bool -> IO ()
setDisableActions flag =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMessage cls' setDisableActionsSelector flag

-- | @+ completionBlock@
completionBlock :: IO (Ptr ())
completionBlock  =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMessage cls' completionBlockSelector

-- | @+ setCompletionBlock:@
setCompletionBlock :: Ptr () -> IO ()
setCompletionBlock block =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMessage cls' setCompletionBlockSelector block

-- | @+ valueForKey:@
valueForKey :: IsNSString key => key -> IO RawId
valueForKey key =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMessage cls' valueForKeySelector (toNSString key)

-- | @+ setValue:forKey:@
setValue_forKey :: IsNSString key => RawId -> key -> IO ()
setValue_forKey anObject key =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMessage cls' setValue_forKeySelector anObject (toNSString key)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @begin@
beginSelector :: Selector '[] ()
beginSelector = mkSelector "begin"

-- | @Selector@ for @commit@
commitSelector :: Selector '[] ()
commitSelector = mkSelector "commit"

-- | @Selector@ for @flush@
flushSelector :: Selector '[] ()
flushSelector = mkSelector "flush"

-- | @Selector@ for @lock@
lockSelector :: Selector '[] ()
lockSelector = mkSelector "lock"

-- | @Selector@ for @unlock@
unlockSelector :: Selector '[] ()
unlockSelector = mkSelector "unlock"

-- | @Selector@ for @animationDuration@
animationDurationSelector :: Selector '[] CDouble
animationDurationSelector = mkSelector "animationDuration"

-- | @Selector@ for @setAnimationDuration:@
setAnimationDurationSelector :: Selector '[CDouble] ()
setAnimationDurationSelector = mkSelector "setAnimationDuration:"

-- | @Selector@ for @animationTimingFunction@
animationTimingFunctionSelector :: Selector '[] (Id CAMediaTimingFunction)
animationTimingFunctionSelector = mkSelector "animationTimingFunction"

-- | @Selector@ for @setAnimationTimingFunction:@
setAnimationTimingFunctionSelector :: Selector '[Id CAMediaTimingFunction] ()
setAnimationTimingFunctionSelector = mkSelector "setAnimationTimingFunction:"

-- | @Selector@ for @disableActions@
disableActionsSelector :: Selector '[] Bool
disableActionsSelector = mkSelector "disableActions"

-- | @Selector@ for @setDisableActions:@
setDisableActionsSelector :: Selector '[Bool] ()
setDisableActionsSelector = mkSelector "setDisableActions:"

-- | @Selector@ for @completionBlock@
completionBlockSelector :: Selector '[] (Ptr ())
completionBlockSelector = mkSelector "completionBlock"

-- | @Selector@ for @setCompletionBlock:@
setCompletionBlockSelector :: Selector '[Ptr ()] ()
setCompletionBlockSelector = mkSelector "setCompletionBlock:"

-- | @Selector@ for @valueForKey:@
valueForKeySelector :: Selector '[Id NSString] RawId
valueForKeySelector = mkSelector "valueForKey:"

-- | @Selector@ for @setValue:forKey:@
setValue_forKeySelector :: Selector '[RawId, Id NSString] ()
setValue_forKeySelector = mkSelector "setValue:forKey:"

