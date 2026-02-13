{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SCNTransaction@.
module ObjC.SceneKit.SCNTransaction
  ( SCNTransaction
  , IsSCNTransaction(..)
  , begin
  , commit
  , flush
  , lock
  , unlock
  , valueForKey
  , setValue_forKey
  , animationDuration
  , setAnimationDuration
  , animationTimingFunction
  , setAnimationTimingFunction
  , disableActions
  , setDisableActions
  , completionBlock
  , setCompletionBlock
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

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ begin@
begin :: IO ()
begin  =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMessage cls' beginSelector

-- | @+ commit@
commit :: IO ()
commit  =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMessage cls' commitSelector

-- | @+ flush@
flush :: IO ()
flush  =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMessage cls' flushSelector

-- | @+ lock@
lock :: IO ()
lock  =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMessage cls' lockSelector

-- | @+ unlock@
unlock :: IO ()
unlock  =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMessage cls' unlockSelector

-- | @+ valueForKey:@
valueForKey :: IsNSString key => key -> IO RawId
valueForKey key =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMessage cls' valueForKeySelector (toNSString key)

-- | @+ setValue:forKey:@
setValue_forKey :: IsNSString key => RawId -> key -> IO ()
setValue_forKey value key =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMessage cls' setValue_forKeySelector value (toNSString key)

-- | @+ animationDuration@
animationDuration :: IO CDouble
animationDuration  =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMessage cls' animationDurationSelector

-- | @+ setAnimationDuration:@
setAnimationDuration :: CDouble -> IO ()
setAnimationDuration value =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMessage cls' setAnimationDurationSelector value

-- | @+ animationTimingFunction@
animationTimingFunction :: IO (Id CAMediaTimingFunction)
animationTimingFunction  =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMessage cls' animationTimingFunctionSelector

-- | @+ setAnimationTimingFunction:@
setAnimationTimingFunction :: IsCAMediaTimingFunction value => value -> IO ()
setAnimationTimingFunction value =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMessage cls' setAnimationTimingFunctionSelector (toCAMediaTimingFunction value)

-- | @+ disableActions@
disableActions :: IO Bool
disableActions  =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMessage cls' disableActionsSelector

-- | @+ setDisableActions:@
setDisableActions :: Bool -> IO ()
setDisableActions value =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMessage cls' setDisableActionsSelector value

-- | @+ completionBlock@
completionBlock :: IO (Ptr ())
completionBlock  =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMessage cls' completionBlockSelector

-- | @+ setCompletionBlock:@
setCompletionBlock :: Ptr () -> IO ()
setCompletionBlock value =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMessage cls' setCompletionBlockSelector value

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

-- | @Selector@ for @valueForKey:@
valueForKeySelector :: Selector '[Id NSString] RawId
valueForKeySelector = mkSelector "valueForKey:"

-- | @Selector@ for @setValue:forKey:@
setValue_forKeySelector :: Selector '[RawId, Id NSString] ()
setValue_forKeySelector = mkSelector "setValue:forKey:"

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

