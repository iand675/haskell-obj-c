{-# LANGUAGE DataKinds #-}
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
  , advanceIntervalForDelegateInvocationSelector
  , delegateQueueSelector
  , delegateSelector
  , initSelector
  , newSelector
  , setAdvanceIntervalForDelegateInvocationSelector
  , setDelegate_queueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVPlayerItemRenderedLegibleOutput avPlayerItemRenderedLegibleOutput => avPlayerItemRenderedLegibleOutput -> IO (Id AVPlayerItemRenderedLegibleOutput)
init_ avPlayerItemRenderedLegibleOutput =
  sendOwnedMessage avPlayerItemRenderedLegibleOutput initSelector

-- | @+ new@
new :: IO (Id AVPlayerItemRenderedLegibleOutput)
new  =
  do
    cls' <- getRequiredClass "AVPlayerItemRenderedLegibleOutput"
    sendOwnedClassMessage cls' newSelector

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
setDelegate_queue avPlayerItemRenderedLegibleOutput delegate delegateQueue =
  sendMessage avPlayerItemRenderedLegibleOutput setDelegate_queueSelector delegate (toNSObject delegateQueue)

-- | delegate
--
-- The receiver's delegate.
--
-- The delegate is held using a zeroing-weak reference, so this property will have a value of nil after a delegate that was previously set has been deallocated.  This property is not key-value observable.
--
-- ObjC selector: @- delegate@
delegate :: IsAVPlayerItemRenderedLegibleOutput avPlayerItemRenderedLegibleOutput => avPlayerItemRenderedLegibleOutput -> IO RawId
delegate avPlayerItemRenderedLegibleOutput =
  sendMessage avPlayerItemRenderedLegibleOutput delegateSelector

-- | delegateQueue
--
-- The dispatch queue where the delegate is messaged.
--
-- This property is not key-value observable.
--
-- ObjC selector: @- delegateQueue@
delegateQueue :: IsAVPlayerItemRenderedLegibleOutput avPlayerItemRenderedLegibleOutput => avPlayerItemRenderedLegibleOutput -> IO (Id NSObject)
delegateQueue avPlayerItemRenderedLegibleOutput =
  sendMessage avPlayerItemRenderedLegibleOutput delegateQueueSelector

-- | advanceIntervalForDelegateInvocation
--
-- Permits advance invocation of the associated delegate, if any.
--
-- If it is possible, an AVPlayerItemRenderedLegibleOutput will message its delegate advanceIntervalForDelegateInvocation seconds earlier than otherwise. If the value you provide is large, effectively requesting provision of samples earlier than the AVPlayerItemRenderedLegibleOutput is prepared to act on them, the delegate will be invoked as soon as possible.
--
-- ObjC selector: @- advanceIntervalForDelegateInvocation@
advanceIntervalForDelegateInvocation :: IsAVPlayerItemRenderedLegibleOutput avPlayerItemRenderedLegibleOutput => avPlayerItemRenderedLegibleOutput -> IO CDouble
advanceIntervalForDelegateInvocation avPlayerItemRenderedLegibleOutput =
  sendMessage avPlayerItemRenderedLegibleOutput advanceIntervalForDelegateInvocationSelector

-- | advanceIntervalForDelegateInvocation
--
-- Permits advance invocation of the associated delegate, if any.
--
-- If it is possible, an AVPlayerItemRenderedLegibleOutput will message its delegate advanceIntervalForDelegateInvocation seconds earlier than otherwise. If the value you provide is large, effectively requesting provision of samples earlier than the AVPlayerItemRenderedLegibleOutput is prepared to act on them, the delegate will be invoked as soon as possible.
--
-- ObjC selector: @- setAdvanceIntervalForDelegateInvocation:@
setAdvanceIntervalForDelegateInvocation :: IsAVPlayerItemRenderedLegibleOutput avPlayerItemRenderedLegibleOutput => avPlayerItemRenderedLegibleOutput -> CDouble -> IO ()
setAdvanceIntervalForDelegateInvocation avPlayerItemRenderedLegibleOutput value =
  sendMessage avPlayerItemRenderedLegibleOutput setAdvanceIntervalForDelegateInvocationSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVPlayerItemRenderedLegibleOutput)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVPlayerItemRenderedLegibleOutput)
newSelector = mkSelector "new"

-- | @Selector@ for @setDelegate:queue:@
setDelegate_queueSelector :: Selector '[RawId, Id NSObject] ()
setDelegate_queueSelector = mkSelector "setDelegate:queue:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @delegateQueue@
delegateQueueSelector :: Selector '[] (Id NSObject)
delegateQueueSelector = mkSelector "delegateQueue"

-- | @Selector@ for @advanceIntervalForDelegateInvocation@
advanceIntervalForDelegateInvocationSelector :: Selector '[] CDouble
advanceIntervalForDelegateInvocationSelector = mkSelector "advanceIntervalForDelegateInvocation"

-- | @Selector@ for @setAdvanceIntervalForDelegateInvocation:@
setAdvanceIntervalForDelegateInvocationSelector :: Selector '[CDouble] ()
setAdvanceIntervalForDelegateInvocationSelector = mkSelector "setAdvanceIntervalForDelegateInvocation:"

