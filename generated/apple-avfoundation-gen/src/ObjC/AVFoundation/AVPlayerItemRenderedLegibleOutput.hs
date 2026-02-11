{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVPlayerItemRenderedLegibleOutput
--
-- A subclass of AVPlayerItemOutput that can vend media with a legible characteristic as rendered CVPixelBufferRefs.
--
-- An instance of AVPlayerItemRenderedLegibleOutput is initialized using the -init method.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVPlayerItemRenderedLegibleOutput@.
module ObjC.AVFoundation.AVPlayerItemRenderedLegibleOutput
  ( AVPlayerItemRenderedLegibleOutput
  , IsAVPlayerItemRenderedLegibleOutput(..)
  , init_
  , new
  , setDelegate_queue
  , delegate
  , delegateQueue
  , advanceIntervalForDelegateInvocation
  , setAdvanceIntervalForDelegateInvocation
  , initSelector
  , newSelector
  , setDelegate_queueSelector
  , delegateSelector
  , delegateQueueSelector
  , advanceIntervalForDelegateInvocationSelector
  , setAdvanceIntervalForDelegateInvocationSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVPlayerItemRenderedLegibleOutput avPlayerItemRenderedLegibleOutput => avPlayerItemRenderedLegibleOutput -> IO (Id AVPlayerItemRenderedLegibleOutput)
init_ avPlayerItemRenderedLegibleOutput  =
    sendMsg avPlayerItemRenderedLegibleOutput (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVPlayerItemRenderedLegibleOutput)
new  =
  do
    cls' <- getRequiredClass "AVPlayerItemRenderedLegibleOutput"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | setDelegate:queue:
--
-- Sets the receiver's delegate and a dispatch queue on which the delegate will be called.
--
-- @delegate@ — An object conforming to AVPlayerItemRenderedLegibleOutputPushDelegate protocol.
--
-- @delegateQueue@ — A dispatch queue on which all delegate methods will be called.
--
-- The delegate is held using a zeroing-weak reference, so it is safe to deallocate the delegate while the receiver still has a reference to it.
--
-- ObjC selector: @- setDelegate:queue:@
setDelegate_queue :: (IsAVPlayerItemRenderedLegibleOutput avPlayerItemRenderedLegibleOutput, IsNSObject delegateQueue) => avPlayerItemRenderedLegibleOutput -> RawId -> delegateQueue -> IO ()
setDelegate_queue avPlayerItemRenderedLegibleOutput  delegate delegateQueue =
  withObjCPtr delegateQueue $ \raw_delegateQueue ->
      sendMsg avPlayerItemRenderedLegibleOutput (mkSelector "setDelegate:queue:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_delegateQueue :: Ptr ())]

-- | delegate
--
-- The receiver's delegate.
--
-- The delegate is held using a zeroing-weak reference, so this property will have a value of nil after a delegate that was previously set has been deallocated.  This property is not key-value observable.
--
-- ObjC selector: @- delegate@
delegate :: IsAVPlayerItemRenderedLegibleOutput avPlayerItemRenderedLegibleOutput => avPlayerItemRenderedLegibleOutput -> IO RawId
delegate avPlayerItemRenderedLegibleOutput  =
    fmap (RawId . castPtr) $ sendMsg avPlayerItemRenderedLegibleOutput (mkSelector "delegate") (retPtr retVoid) []

-- | delegateQueue
--
-- The dispatch queue where the delegate is messaged.
--
-- This property is not key-value observable.
--
-- ObjC selector: @- delegateQueue@
delegateQueue :: IsAVPlayerItemRenderedLegibleOutput avPlayerItemRenderedLegibleOutput => avPlayerItemRenderedLegibleOutput -> IO (Id NSObject)
delegateQueue avPlayerItemRenderedLegibleOutput  =
    sendMsg avPlayerItemRenderedLegibleOutput (mkSelector "delegateQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | advanceIntervalForDelegateInvocation
--
-- Permits advance invocation of the associated delegate, if any.
--
-- If it is possible, an AVPlayerItemRenderedLegibleOutput will message its delegate advanceIntervalForDelegateInvocation seconds earlier than otherwise. If the value you provide is large, effectively requesting provision of samples earlier than the AVPlayerItemRenderedLegibleOutput is prepared to act on them, the delegate will be invoked as soon as possible.
--
-- ObjC selector: @- advanceIntervalForDelegateInvocation@
advanceIntervalForDelegateInvocation :: IsAVPlayerItemRenderedLegibleOutput avPlayerItemRenderedLegibleOutput => avPlayerItemRenderedLegibleOutput -> IO CDouble
advanceIntervalForDelegateInvocation avPlayerItemRenderedLegibleOutput  =
    sendMsg avPlayerItemRenderedLegibleOutput (mkSelector "advanceIntervalForDelegateInvocation") retCDouble []

-- | advanceIntervalForDelegateInvocation
--
-- Permits advance invocation of the associated delegate, if any.
--
-- If it is possible, an AVPlayerItemRenderedLegibleOutput will message its delegate advanceIntervalForDelegateInvocation seconds earlier than otherwise. If the value you provide is large, effectively requesting provision of samples earlier than the AVPlayerItemRenderedLegibleOutput is prepared to act on them, the delegate will be invoked as soon as possible.
--
-- ObjC selector: @- setAdvanceIntervalForDelegateInvocation:@
setAdvanceIntervalForDelegateInvocation :: IsAVPlayerItemRenderedLegibleOutput avPlayerItemRenderedLegibleOutput => avPlayerItemRenderedLegibleOutput -> CDouble -> IO ()
setAdvanceIntervalForDelegateInvocation avPlayerItemRenderedLegibleOutput  value =
    sendMsg avPlayerItemRenderedLegibleOutput (mkSelector "setAdvanceIntervalForDelegateInvocation:") retVoid [argCDouble value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @setDelegate:queue:@
setDelegate_queueSelector :: Selector
setDelegate_queueSelector = mkSelector "setDelegate:queue:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @delegateQueue@
delegateQueueSelector :: Selector
delegateQueueSelector = mkSelector "delegateQueue"

-- | @Selector@ for @advanceIntervalForDelegateInvocation@
advanceIntervalForDelegateInvocationSelector :: Selector
advanceIntervalForDelegateInvocationSelector = mkSelector "advanceIntervalForDelegateInvocation"

-- | @Selector@ for @setAdvanceIntervalForDelegateInvocation:@
setAdvanceIntervalForDelegateInvocationSelector :: Selector
setAdvanceIntervalForDelegateInvocationSelector = mkSelector "setAdvanceIntervalForDelegateInvocation:"

