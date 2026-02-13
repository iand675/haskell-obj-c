{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVPlayerItemLegibleOutput
--
-- A subclass of AVPlayerItemOutput that can vend media with a legible characteristic as NSAttributedStrings.
--
-- An instance of AVPlayerItemLegibleOutput is typically initialized using the -init method.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVPlayerItemLegibleOutput@.
module ObjC.AVFoundation.AVPlayerItemLegibleOutput
  ( AVPlayerItemLegibleOutput
  , IsAVPlayerItemLegibleOutput(..)
  , setDelegate_queue
  , initWithMediaSubtypesForNativeRepresentation
  , delegate
  , delegateQueue
  , advanceIntervalForDelegateInvocation
  , setAdvanceIntervalForDelegateInvocation
  , textStylingResolution
  , setTextStylingResolution
  , advanceIntervalForDelegateInvocationSelector
  , delegateQueueSelector
  , delegateSelector
  , initWithMediaSubtypesForNativeRepresentationSelector
  , setAdvanceIntervalForDelegateInvocationSelector
  , setDelegate_queueSelector
  , setTextStylingResolutionSelector
  , textStylingResolutionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | setDelegate:queue:
--
-- Sets the receiver's delegate and a dispatch queue on which the delegate will be called.
--
-- @delegate@ — An object conforming to AVPlayerItemLegibleOutputPushDelegate protocol.
--
-- @delegateQueue@ — A dispatch queue on which all delegate methods will be called.
--
-- The delegate is held using a zeroing-weak reference, so it is safe to deallocate the delegate while the receiver still has a reference to it.
--
-- ObjC selector: @- setDelegate:queue:@
setDelegate_queue :: (IsAVPlayerItemLegibleOutput avPlayerItemLegibleOutput, IsNSObject delegateQueue) => avPlayerItemLegibleOutput -> RawId -> delegateQueue -> IO ()
setDelegate_queue avPlayerItemLegibleOutput delegate delegateQueue =
  sendMessage avPlayerItemLegibleOutput setDelegate_queueSelector delegate (toNSObject delegateQueue)

-- | initWithMediaSubtypesForNativeRepresentation:
--
-- Returns an instance of AVPlayerItemLegibleOutput with filtering enabled for AVPlayerItemLegibleOutputPushDelegate's legibleOutput:didOutputAttributedStrings:nativeSampleBuffers:forItemTime:.
--
-- @subtypes@ — NSArray of NSNumber FourCC codes, e.g. \@[ [NSNumber numberWithUnsignedInt:'tx3g'] ]
--
-- Returns: An instance of AVPlayerItemLegibleOutput.
--
-- Add media subtype FourCC number objects to the subtypes array to elect to receive that type as a CMSampleBuffer instead of an NSAttributedString.  Initializing an AVPlayerItemLegibleOutput using the -init method is equivalent to calling -initWithMediaSubtypesForNativeRepresentation: with an empty array, which means that all legible data, regardless of media subtype, will be delivered using NSAttributedString in a common format.
--
-- If a media subtype for which there is no legible data in the current player item is included in the media subtypes array, no error will occur.  AVPlayerItemLegibleOutput will not vend closed caption data as CMSampleBuffers, so it is an error to include 'c608' in the media subtypes array.
--
-- This method throws an exception if any media subtype is kCMClosedCaptionFormatType_CEA608 (native representation is not available for media subtype).
--
-- ObjC selector: @- initWithMediaSubtypesForNativeRepresentation:@
initWithMediaSubtypesForNativeRepresentation :: (IsAVPlayerItemLegibleOutput avPlayerItemLegibleOutput, IsNSArray subtypes) => avPlayerItemLegibleOutput -> subtypes -> IO (Id AVPlayerItemLegibleOutput)
initWithMediaSubtypesForNativeRepresentation avPlayerItemLegibleOutput subtypes =
  sendOwnedMessage avPlayerItemLegibleOutput initWithMediaSubtypesForNativeRepresentationSelector (toNSArray subtypes)

-- | delegate
--
-- The receiver's delegate.
--
-- The delegate is held using a zeroing-weak reference, so this property will have a value of nil after a delegate that was previously set has been deallocated.  This property is not key-value observable.
--
-- ObjC selector: @- delegate@
delegate :: IsAVPlayerItemLegibleOutput avPlayerItemLegibleOutput => avPlayerItemLegibleOutput -> IO RawId
delegate avPlayerItemLegibleOutput =
  sendMessage avPlayerItemLegibleOutput delegateSelector

-- | delegateQueue
--
-- The dispatch queue where the delegate is messaged.
--
-- This property is not key-value observable.
--
-- ObjC selector: @- delegateQueue@
delegateQueue :: IsAVPlayerItemLegibleOutput avPlayerItemLegibleOutput => avPlayerItemLegibleOutput -> IO (Id NSObject)
delegateQueue avPlayerItemLegibleOutput =
  sendMessage avPlayerItemLegibleOutput delegateQueueSelector

-- | advanceIntervalForDelegateInvocation
--
-- Permits advance invocation of the associated delegate, if any.
--
-- If it is possible, an AVPlayerItemLegibleOutput will message its delegate advanceIntervalForDelegateInvocation seconds earlier than otherwise. If the value you provide is large, effectively requesting provision of samples earlier than the AVPlayerItemLegibleOutput is prepared to act on them, the delegate will be invoked as soon as possible.
--
-- ObjC selector: @- advanceIntervalForDelegateInvocation@
advanceIntervalForDelegateInvocation :: IsAVPlayerItemLegibleOutput avPlayerItemLegibleOutput => avPlayerItemLegibleOutput -> IO CDouble
advanceIntervalForDelegateInvocation avPlayerItemLegibleOutput =
  sendMessage avPlayerItemLegibleOutput advanceIntervalForDelegateInvocationSelector

-- | advanceIntervalForDelegateInvocation
--
-- Permits advance invocation of the associated delegate, if any.
--
-- If it is possible, an AVPlayerItemLegibleOutput will message its delegate advanceIntervalForDelegateInvocation seconds earlier than otherwise. If the value you provide is large, effectively requesting provision of samples earlier than the AVPlayerItemLegibleOutput is prepared to act on them, the delegate will be invoked as soon as possible.
--
-- ObjC selector: @- setAdvanceIntervalForDelegateInvocation:@
setAdvanceIntervalForDelegateInvocation :: IsAVPlayerItemLegibleOutput avPlayerItemLegibleOutput => avPlayerItemLegibleOutput -> CDouble -> IO ()
setAdvanceIntervalForDelegateInvocation avPlayerItemLegibleOutput value =
  sendMessage avPlayerItemLegibleOutput setAdvanceIntervalForDelegateInvocationSelector value

-- | textStylingResolution
--
-- A string identifier indicating the degree of text styling to be applied to attributed strings vended by the receiver
--
-- Valid values are AVPlayerItemLegibleOutputTextStylingResolutionDefault and AVPlayerItemLegibleOutputTextStylingResolutionSourceAndRulesOnly.  An NSInvalidArgumentException is raised if this property is set to any other value.  The default value is AVPlayerItemLegibleOutputTextStylingResolutionDefault, which indicates that attributed strings vended by the receiver will include the same level of styling information that would be used if AVFoundation were rendering the text via AVPlayerLayer.
--
-- ObjC selector: @- textStylingResolution@
textStylingResolution :: IsAVPlayerItemLegibleOutput avPlayerItemLegibleOutput => avPlayerItemLegibleOutput -> IO (Id NSString)
textStylingResolution avPlayerItemLegibleOutput =
  sendMessage avPlayerItemLegibleOutput textStylingResolutionSelector

-- | textStylingResolution
--
-- A string identifier indicating the degree of text styling to be applied to attributed strings vended by the receiver
--
-- Valid values are AVPlayerItemLegibleOutputTextStylingResolutionDefault and AVPlayerItemLegibleOutputTextStylingResolutionSourceAndRulesOnly.  An NSInvalidArgumentException is raised if this property is set to any other value.  The default value is AVPlayerItemLegibleOutputTextStylingResolutionDefault, which indicates that attributed strings vended by the receiver will include the same level of styling information that would be used if AVFoundation were rendering the text via AVPlayerLayer.
--
-- ObjC selector: @- setTextStylingResolution:@
setTextStylingResolution :: (IsAVPlayerItemLegibleOutput avPlayerItemLegibleOutput, IsNSString value) => avPlayerItemLegibleOutput -> value -> IO ()
setTextStylingResolution avPlayerItemLegibleOutput value =
  sendMessage avPlayerItemLegibleOutput setTextStylingResolutionSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setDelegate:queue:@
setDelegate_queueSelector :: Selector '[RawId, Id NSObject] ()
setDelegate_queueSelector = mkSelector "setDelegate:queue:"

-- | @Selector@ for @initWithMediaSubtypesForNativeRepresentation:@
initWithMediaSubtypesForNativeRepresentationSelector :: Selector '[Id NSArray] (Id AVPlayerItemLegibleOutput)
initWithMediaSubtypesForNativeRepresentationSelector = mkSelector "initWithMediaSubtypesForNativeRepresentation:"

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

-- | @Selector@ for @textStylingResolution@
textStylingResolutionSelector :: Selector '[] (Id NSString)
textStylingResolutionSelector = mkSelector "textStylingResolution"

-- | @Selector@ for @setTextStylingResolution:@
setTextStylingResolutionSelector :: Selector '[Id NSString] ()
setTextStylingResolutionSelector = mkSelector "setTextStylingResolution:"

