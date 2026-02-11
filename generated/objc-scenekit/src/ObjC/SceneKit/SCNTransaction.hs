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
  , disableActions
  , setDisableActions
  , completionBlock
  , setCompletionBlock
  , beginSelector
  , commitSelector
  , flushSelector
  , lockSelector
  , unlockSelector
  , valueForKeySelector
  , setValue_forKeySelector
  , animationDurationSelector
  , setAnimationDurationSelector
  , disableActionsSelector
  , setDisableActionsSelector
  , completionBlockSelector
  , setCompletionBlockSelector


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

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ begin@
begin :: IO ()
begin  =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMsg cls' (mkSelector "begin") retVoid []

-- | @+ commit@
commit :: IO ()
commit  =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMsg cls' (mkSelector "commit") retVoid []

-- | @+ flush@
flush :: IO ()
flush  =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMsg cls' (mkSelector "flush") retVoid []

-- | @+ lock@
lock :: IO ()
lock  =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMsg cls' (mkSelector "lock") retVoid []

-- | @+ unlock@
unlock :: IO ()
unlock  =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMsg cls' (mkSelector "unlock") retVoid []

-- | @+ valueForKey:@
valueForKey :: IsNSString key => key -> IO RawId
valueForKey key =
  do
    cls' <- getRequiredClass "SCNTransaction"
    withObjCPtr key $ \raw_key ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "valueForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @+ setValue:forKey:@
setValue_forKey :: IsNSString key => RawId -> key -> IO ()
setValue_forKey value key =
  do
    cls' <- getRequiredClass "SCNTransaction"
    withObjCPtr key $ \raw_key ->
      sendClassMsg cls' (mkSelector "setValue:forKey:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @+ animationDuration@
animationDuration :: IO CDouble
animationDuration  =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMsg cls' (mkSelector "animationDuration") retCDouble []

-- | @+ setAnimationDuration:@
setAnimationDuration :: CDouble -> IO ()
setAnimationDuration value =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMsg cls' (mkSelector "setAnimationDuration:") retVoid [argCDouble (fromIntegral value)]

-- | @+ disableActions@
disableActions :: IO Bool
disableActions  =
  do
    cls' <- getRequiredClass "SCNTransaction"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "disableActions") retCULong []

-- | @+ setDisableActions:@
setDisableActions :: Bool -> IO ()
setDisableActions value =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMsg cls' (mkSelector "setDisableActions:") retVoid [argCULong (if value then 1 else 0)]

-- | @+ completionBlock@
completionBlock :: IO (Ptr ())
completionBlock  =
  do
    cls' <- getRequiredClass "SCNTransaction"
    fmap castPtr $ sendClassMsg cls' (mkSelector "completionBlock") (retPtr retVoid) []

-- | @+ setCompletionBlock:@
setCompletionBlock :: Ptr () -> IO ()
setCompletionBlock value =
  do
    cls' <- getRequiredClass "SCNTransaction"
    sendClassMsg cls' (mkSelector "setCompletionBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

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

-- | @Selector@ for @valueForKey:@
valueForKeySelector :: Selector
valueForKeySelector = mkSelector "valueForKey:"

-- | @Selector@ for @setValue:forKey:@
setValue_forKeySelector :: Selector
setValue_forKeySelector = mkSelector "setValue:forKey:"

-- | @Selector@ for @animationDuration@
animationDurationSelector :: Selector
animationDurationSelector = mkSelector "animationDuration"

-- | @Selector@ for @setAnimationDuration:@
setAnimationDurationSelector :: Selector
setAnimationDurationSelector = mkSelector "setAnimationDuration:"

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

