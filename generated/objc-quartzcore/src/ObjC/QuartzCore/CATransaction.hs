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
  , beginSelector
  , commitSelector
  , flushSelector
  , lockSelector
  , unlockSelector
  , animationDurationSelector
  , setAnimationDurationSelector
  , animationTimingFunctionSelector
  , setAnimationTimingFunctionSelector
  , disableActionsSelector
  , setDisableActionsSelector
  , completionBlockSelector
  , setCompletionBlockSelector
  , valueForKeySelector
  , setValue_forKeySelector


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

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ begin@
begin :: IO ()
begin  =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMsg cls' (mkSelector "begin") retVoid []

-- | @+ commit@
commit :: IO ()
commit  =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMsg cls' (mkSelector "commit") retVoid []

-- | @+ flush@
flush :: IO ()
flush  =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMsg cls' (mkSelector "flush") retVoid []

-- | @+ lock@
lock :: IO ()
lock  =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMsg cls' (mkSelector "lock") retVoid []

-- | @+ unlock@
unlock :: IO ()
unlock  =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMsg cls' (mkSelector "unlock") retVoid []

-- | @+ animationDuration@
animationDuration :: IO CDouble
animationDuration  =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMsg cls' (mkSelector "animationDuration") retCDouble []

-- | @+ setAnimationDuration:@
setAnimationDuration :: CDouble -> IO ()
setAnimationDuration dur =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMsg cls' (mkSelector "setAnimationDuration:") retVoid [argCDouble (fromIntegral dur)]

-- | @+ animationTimingFunction@
animationTimingFunction :: IO (Id CAMediaTimingFunction)
animationTimingFunction  =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMsg cls' (mkSelector "animationTimingFunction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ setAnimationTimingFunction:@
setAnimationTimingFunction :: IsCAMediaTimingFunction function => function -> IO ()
setAnimationTimingFunction function =
  do
    cls' <- getRequiredClass "CATransaction"
    withObjCPtr function $ \raw_function ->
      sendClassMsg cls' (mkSelector "setAnimationTimingFunction:") retVoid [argPtr (castPtr raw_function :: Ptr ())]

-- | @+ disableActions@
disableActions :: IO Bool
disableActions  =
  do
    cls' <- getRequiredClass "CATransaction"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "disableActions") retCULong []

-- | @+ setDisableActions:@
setDisableActions :: Bool -> IO ()
setDisableActions flag =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMsg cls' (mkSelector "setDisableActions:") retVoid [argCULong (if flag then 1 else 0)]

-- | @+ completionBlock@
completionBlock :: IO (Ptr ())
completionBlock  =
  do
    cls' <- getRequiredClass "CATransaction"
    fmap castPtr $ sendClassMsg cls' (mkSelector "completionBlock") (retPtr retVoid) []

-- | @+ setCompletionBlock:@
setCompletionBlock :: Ptr () -> IO ()
setCompletionBlock block =
  do
    cls' <- getRequiredClass "CATransaction"
    sendClassMsg cls' (mkSelector "setCompletionBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @+ valueForKey:@
valueForKey :: IsNSString key => key -> IO RawId
valueForKey key =
  do
    cls' <- getRequiredClass "CATransaction"
    withObjCPtr key $ \raw_key ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "valueForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @+ setValue:forKey:@
setValue_forKey :: IsNSString key => RawId -> key -> IO ()
setValue_forKey anObject key =
  do
    cls' <- getRequiredClass "CATransaction"
    withObjCPtr key $ \raw_key ->
      sendClassMsg cls' (mkSelector "setValue:forKey:") retVoid [argPtr (castPtr (unRawId anObject) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @begin@
beginSelector :: Selector
beginSelector = mkSelector "begin"

-- | @Selector@ for @commit@
commitSelector :: Selector
commitSelector = mkSelector "commit"

-- | @Selector@ for @flush@
flushSelector :: Selector
flushSelector = mkSelector "flush"

-- | @Selector@ for @lock@
lockSelector :: Selector
lockSelector = mkSelector "lock"

-- | @Selector@ for @unlock@
unlockSelector :: Selector
unlockSelector = mkSelector "unlock"

-- | @Selector@ for @animationDuration@
animationDurationSelector :: Selector
animationDurationSelector = mkSelector "animationDuration"

-- | @Selector@ for @setAnimationDuration:@
setAnimationDurationSelector :: Selector
setAnimationDurationSelector = mkSelector "setAnimationDuration:"

-- | @Selector@ for @animationTimingFunction@
animationTimingFunctionSelector :: Selector
animationTimingFunctionSelector = mkSelector "animationTimingFunction"

-- | @Selector@ for @setAnimationTimingFunction:@
setAnimationTimingFunctionSelector :: Selector
setAnimationTimingFunctionSelector = mkSelector "setAnimationTimingFunction:"

-- | @Selector@ for @disableActions@
disableActionsSelector :: Selector
disableActionsSelector = mkSelector "disableActions"

-- | @Selector@ for @setDisableActions:@
setDisableActionsSelector :: Selector
setDisableActionsSelector = mkSelector "setDisableActions:"

-- | @Selector@ for @completionBlock@
completionBlockSelector :: Selector
completionBlockSelector = mkSelector "completionBlock"

-- | @Selector@ for @setCompletionBlock:@
setCompletionBlockSelector :: Selector
setCompletionBlockSelector = mkSelector "setCompletionBlock:"

-- | @Selector@ for @valueForKey:@
valueForKeySelector :: Selector
valueForKeySelector = mkSelector "valueForKey:"

-- | @Selector@ for @setValue:forKey:@
setValue_forKeySelector :: Selector
setValue_forKeySelector = mkSelector "setValue:forKey:"

