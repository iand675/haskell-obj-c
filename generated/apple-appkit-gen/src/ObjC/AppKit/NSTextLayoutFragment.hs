{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextLayoutFragment@.
module ObjC.AppKit.NSTextLayoutFragment
  ( NSTextLayoutFragment
  , IsNSTextLayoutFragment(..)
  , initWithTextElement_range
  , initWithCoder
  , init_
  , textLineFragmentForVerticalOffset_requiresExactMatch
  , textLineFragmentForTextLocation_isUpstreamAffinity
  , invalidateLayout
  , textLayoutManager
  , textElement
  , rangeInElement
  , textLineFragments
  , layoutQueue
  , setLayoutQueue
  , state
  , leadingPadding
  , trailingPadding
  , topMargin
  , bottomMargin
  , textAttachmentViewProviders
  , bottomMarginSelector
  , initSelector
  , initWithCoderSelector
  , initWithTextElement_rangeSelector
  , invalidateLayoutSelector
  , layoutQueueSelector
  , leadingPaddingSelector
  , rangeInElementSelector
  , setLayoutQueueSelector
  , stateSelector
  , textAttachmentViewProvidersSelector
  , textElementSelector
  , textLayoutManagerSelector
  , textLineFragmentForTextLocation_isUpstreamAffinitySelector
  , textLineFragmentForVerticalOffset_requiresExactMatchSelector
  , textLineFragmentsSelector
  , topMarginSelector
  , trailingPaddingSelector

  -- * Enum types
  , NSTextLayoutFragmentState(NSTextLayoutFragmentState)
  , pattern NSTextLayoutFragmentStateNone
  , pattern NSTextLayoutFragmentStateEstimatedUsageBounds
  , pattern NSTextLayoutFragmentStateCalculatedUsageBounds
  , pattern NSTextLayoutFragmentStateLayoutAvailable

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithTextElement:range:@
initWithTextElement_range :: (IsNSTextLayoutFragment nsTextLayoutFragment, IsNSTextElement textElement, IsNSTextRange rangeInElement) => nsTextLayoutFragment -> textElement -> rangeInElement -> IO (Id NSTextLayoutFragment)
initWithTextElement_range nsTextLayoutFragment textElement rangeInElement =
  sendOwnedMessage nsTextLayoutFragment initWithTextElement_rangeSelector (toNSTextElement textElement) (toNSTextRange rangeInElement)

-- | @- initWithCoder:@
initWithCoder :: (IsNSTextLayoutFragment nsTextLayoutFragment, IsNSCoder coder) => nsTextLayoutFragment -> coder -> IO (Id NSTextLayoutFragment)
initWithCoder nsTextLayoutFragment coder =
  sendOwnedMessage nsTextLayoutFragment initWithCoderSelector (toNSCoder coder)

-- | @- init@
init_ :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO (Id NSTextLayoutFragment)
init_ nsTextLayoutFragment =
  sendOwnedMessage nsTextLayoutFragment initSelector

-- | @- textLineFragmentForVerticalOffset:requiresExactMatch:@
textLineFragmentForVerticalOffset_requiresExactMatch :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> CDouble -> Bool -> IO (Id NSTextLineFragment)
textLineFragmentForVerticalOffset_requiresExactMatch nsTextLayoutFragment verticalOffset requiresExactMatch =
  sendMessage nsTextLayoutFragment textLineFragmentForVerticalOffset_requiresExactMatchSelector verticalOffset requiresExactMatch

-- | @- textLineFragmentForTextLocation:isUpstreamAffinity:@
textLineFragmentForTextLocation_isUpstreamAffinity :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> RawId -> Bool -> IO (Id NSTextLineFragment)
textLineFragmentForTextLocation_isUpstreamAffinity nsTextLayoutFragment textLocation isUpstreamAffinity =
  sendMessage nsTextLayoutFragment textLineFragmentForTextLocation_isUpstreamAffinitySelector textLocation isUpstreamAffinity

-- | @- invalidateLayout@
invalidateLayout :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO ()
invalidateLayout nsTextLayoutFragment =
  sendMessage nsTextLayoutFragment invalidateLayoutSelector

-- | @- textLayoutManager@
textLayoutManager :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO (Id NSTextLayoutManager)
textLayoutManager nsTextLayoutFragment =
  sendMessage nsTextLayoutFragment textLayoutManagerSelector

-- | @- textElement@
textElement :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO (Id NSTextElement)
textElement nsTextLayoutFragment =
  sendMessage nsTextLayoutFragment textElementSelector

-- | @- rangeInElement@
rangeInElement :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO (Id NSTextRange)
rangeInElement nsTextLayoutFragment =
  sendMessage nsTextLayoutFragment rangeInElementSelector

-- | @- textLineFragments@
textLineFragments :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO (Id NSArray)
textLineFragments nsTextLayoutFragment =
  sendMessage nsTextLayoutFragment textLineFragmentsSelector

-- | @- layoutQueue@
layoutQueue :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO (Id NSOperationQueue)
layoutQueue nsTextLayoutFragment =
  sendMessage nsTextLayoutFragment layoutQueueSelector

-- | @- setLayoutQueue:@
setLayoutQueue :: (IsNSTextLayoutFragment nsTextLayoutFragment, IsNSOperationQueue value) => nsTextLayoutFragment -> value -> IO ()
setLayoutQueue nsTextLayoutFragment value =
  sendMessage nsTextLayoutFragment setLayoutQueueSelector (toNSOperationQueue value)

-- | @- state@
state :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO NSTextLayoutFragmentState
state nsTextLayoutFragment =
  sendMessage nsTextLayoutFragment stateSelector

-- | @- leadingPadding@
leadingPadding :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO CDouble
leadingPadding nsTextLayoutFragment =
  sendMessage nsTextLayoutFragment leadingPaddingSelector

-- | @- trailingPadding@
trailingPadding :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO CDouble
trailingPadding nsTextLayoutFragment =
  sendMessage nsTextLayoutFragment trailingPaddingSelector

-- | @- topMargin@
topMargin :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO CDouble
topMargin nsTextLayoutFragment =
  sendMessage nsTextLayoutFragment topMarginSelector

-- | @- bottomMargin@
bottomMargin :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO CDouble
bottomMargin nsTextLayoutFragment =
  sendMessage nsTextLayoutFragment bottomMarginSelector

-- | @- textAttachmentViewProviders@
textAttachmentViewProviders :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO (Id NSArray)
textAttachmentViewProviders nsTextLayoutFragment =
  sendMessage nsTextLayoutFragment textAttachmentViewProvidersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTextElement:range:@
initWithTextElement_rangeSelector :: Selector '[Id NSTextElement, Id NSTextRange] (Id NSTextLayoutFragment)
initWithTextElement_rangeSelector = mkSelector "initWithTextElement:range:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSTextLayoutFragment)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSTextLayoutFragment)
initSelector = mkSelector "init"

-- | @Selector@ for @textLineFragmentForVerticalOffset:requiresExactMatch:@
textLineFragmentForVerticalOffset_requiresExactMatchSelector :: Selector '[CDouble, Bool] (Id NSTextLineFragment)
textLineFragmentForVerticalOffset_requiresExactMatchSelector = mkSelector "textLineFragmentForVerticalOffset:requiresExactMatch:"

-- | @Selector@ for @textLineFragmentForTextLocation:isUpstreamAffinity:@
textLineFragmentForTextLocation_isUpstreamAffinitySelector :: Selector '[RawId, Bool] (Id NSTextLineFragment)
textLineFragmentForTextLocation_isUpstreamAffinitySelector = mkSelector "textLineFragmentForTextLocation:isUpstreamAffinity:"

-- | @Selector@ for @invalidateLayout@
invalidateLayoutSelector :: Selector '[] ()
invalidateLayoutSelector = mkSelector "invalidateLayout"

-- | @Selector@ for @textLayoutManager@
textLayoutManagerSelector :: Selector '[] (Id NSTextLayoutManager)
textLayoutManagerSelector = mkSelector "textLayoutManager"

-- | @Selector@ for @textElement@
textElementSelector :: Selector '[] (Id NSTextElement)
textElementSelector = mkSelector "textElement"

-- | @Selector@ for @rangeInElement@
rangeInElementSelector :: Selector '[] (Id NSTextRange)
rangeInElementSelector = mkSelector "rangeInElement"

-- | @Selector@ for @textLineFragments@
textLineFragmentsSelector :: Selector '[] (Id NSArray)
textLineFragmentsSelector = mkSelector "textLineFragments"

-- | @Selector@ for @layoutQueue@
layoutQueueSelector :: Selector '[] (Id NSOperationQueue)
layoutQueueSelector = mkSelector "layoutQueue"

-- | @Selector@ for @setLayoutQueue:@
setLayoutQueueSelector :: Selector '[Id NSOperationQueue] ()
setLayoutQueueSelector = mkSelector "setLayoutQueue:"

-- | @Selector@ for @state@
stateSelector :: Selector '[] NSTextLayoutFragmentState
stateSelector = mkSelector "state"

-- | @Selector@ for @leadingPadding@
leadingPaddingSelector :: Selector '[] CDouble
leadingPaddingSelector = mkSelector "leadingPadding"

-- | @Selector@ for @trailingPadding@
trailingPaddingSelector :: Selector '[] CDouble
trailingPaddingSelector = mkSelector "trailingPadding"

-- | @Selector@ for @topMargin@
topMarginSelector :: Selector '[] CDouble
topMarginSelector = mkSelector "topMargin"

-- | @Selector@ for @bottomMargin@
bottomMarginSelector :: Selector '[] CDouble
bottomMarginSelector = mkSelector "bottomMargin"

-- | @Selector@ for @textAttachmentViewProviders@
textAttachmentViewProvidersSelector :: Selector '[] (Id NSArray)
textAttachmentViewProvidersSelector = mkSelector "textAttachmentViewProviders"

