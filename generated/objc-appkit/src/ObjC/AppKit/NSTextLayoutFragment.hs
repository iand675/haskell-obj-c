{-# LANGUAGE PatternSynonyms #-}
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
  , initWithTextElement_rangeSelector
  , initWithCoderSelector
  , initSelector
  , textLineFragmentForVerticalOffset_requiresExactMatchSelector
  , textLineFragmentForTextLocation_isUpstreamAffinitySelector
  , invalidateLayoutSelector
  , textLayoutManagerSelector
  , textElementSelector
  , rangeInElementSelector
  , textLineFragmentsSelector
  , layoutQueueSelector
  , setLayoutQueueSelector
  , stateSelector
  , leadingPaddingSelector
  , trailingPaddingSelector
  , topMarginSelector
  , bottomMarginSelector
  , textAttachmentViewProvidersSelector

  -- * Enum types
  , NSTextLayoutFragmentState(NSTextLayoutFragmentState)
  , pattern NSTextLayoutFragmentStateNone
  , pattern NSTextLayoutFragmentStateEstimatedUsageBounds
  , pattern NSTextLayoutFragmentStateCalculatedUsageBounds
  , pattern NSTextLayoutFragmentStateLayoutAvailable

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

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithTextElement:range:@
initWithTextElement_range :: (IsNSTextLayoutFragment nsTextLayoutFragment, IsNSTextElement textElement, IsNSTextRange rangeInElement) => nsTextLayoutFragment -> textElement -> rangeInElement -> IO (Id NSTextLayoutFragment)
initWithTextElement_range nsTextLayoutFragment  textElement rangeInElement =
withObjCPtr textElement $ \raw_textElement ->
  withObjCPtr rangeInElement $ \raw_rangeInElement ->
      sendMsg nsTextLayoutFragment (mkSelector "initWithTextElement:range:") (retPtr retVoid) [argPtr (castPtr raw_textElement :: Ptr ()), argPtr (castPtr raw_rangeInElement :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSTextLayoutFragment nsTextLayoutFragment, IsNSCoder coder) => nsTextLayoutFragment -> coder -> IO (Id NSTextLayoutFragment)
initWithCoder nsTextLayoutFragment  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsTextLayoutFragment (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO (Id NSTextLayoutFragment)
init_ nsTextLayoutFragment  =
  sendMsg nsTextLayoutFragment (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- textLineFragmentForVerticalOffset:requiresExactMatch:@
textLineFragmentForVerticalOffset_requiresExactMatch :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> CDouble -> Bool -> IO (Id NSTextLineFragment)
textLineFragmentForVerticalOffset_requiresExactMatch nsTextLayoutFragment  verticalOffset requiresExactMatch =
  sendMsg nsTextLayoutFragment (mkSelector "textLineFragmentForVerticalOffset:requiresExactMatch:") (retPtr retVoid) [argCDouble (fromIntegral verticalOffset), argCULong (if requiresExactMatch then 1 else 0)] >>= retainedObject . castPtr

-- | @- textLineFragmentForTextLocation:isUpstreamAffinity:@
textLineFragmentForTextLocation_isUpstreamAffinity :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> RawId -> Bool -> IO (Id NSTextLineFragment)
textLineFragmentForTextLocation_isUpstreamAffinity nsTextLayoutFragment  textLocation isUpstreamAffinity =
  sendMsg nsTextLayoutFragment (mkSelector "textLineFragmentForTextLocation:isUpstreamAffinity:") (retPtr retVoid) [argPtr (castPtr (unRawId textLocation) :: Ptr ()), argCULong (if isUpstreamAffinity then 1 else 0)] >>= retainedObject . castPtr

-- | @- invalidateLayout@
invalidateLayout :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO ()
invalidateLayout nsTextLayoutFragment  =
  sendMsg nsTextLayoutFragment (mkSelector "invalidateLayout") retVoid []

-- | @- textLayoutManager@
textLayoutManager :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO (Id NSTextLayoutManager)
textLayoutManager nsTextLayoutFragment  =
  sendMsg nsTextLayoutFragment (mkSelector "textLayoutManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- textElement@
textElement :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO (Id NSTextElement)
textElement nsTextLayoutFragment  =
  sendMsg nsTextLayoutFragment (mkSelector "textElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rangeInElement@
rangeInElement :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO (Id NSTextRange)
rangeInElement nsTextLayoutFragment  =
  sendMsg nsTextLayoutFragment (mkSelector "rangeInElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- textLineFragments@
textLineFragments :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO (Id NSArray)
textLineFragments nsTextLayoutFragment  =
  sendMsg nsTextLayoutFragment (mkSelector "textLineFragments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- layoutQueue@
layoutQueue :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO (Id NSOperationQueue)
layoutQueue nsTextLayoutFragment  =
  sendMsg nsTextLayoutFragment (mkSelector "layoutQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLayoutQueue:@
setLayoutQueue :: (IsNSTextLayoutFragment nsTextLayoutFragment, IsNSOperationQueue value) => nsTextLayoutFragment -> value -> IO ()
setLayoutQueue nsTextLayoutFragment  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextLayoutFragment (mkSelector "setLayoutQueue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- state@
state :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO NSTextLayoutFragmentState
state nsTextLayoutFragment  =
  fmap (coerce :: CULong -> NSTextLayoutFragmentState) $ sendMsg nsTextLayoutFragment (mkSelector "state") retCULong []

-- | @- leadingPadding@
leadingPadding :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO CDouble
leadingPadding nsTextLayoutFragment  =
  sendMsg nsTextLayoutFragment (mkSelector "leadingPadding") retCDouble []

-- | @- trailingPadding@
trailingPadding :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO CDouble
trailingPadding nsTextLayoutFragment  =
  sendMsg nsTextLayoutFragment (mkSelector "trailingPadding") retCDouble []

-- | @- topMargin@
topMargin :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO CDouble
topMargin nsTextLayoutFragment  =
  sendMsg nsTextLayoutFragment (mkSelector "topMargin") retCDouble []

-- | @- bottomMargin@
bottomMargin :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO CDouble
bottomMargin nsTextLayoutFragment  =
  sendMsg nsTextLayoutFragment (mkSelector "bottomMargin") retCDouble []

-- | @- textAttachmentViewProviders@
textAttachmentViewProviders :: IsNSTextLayoutFragment nsTextLayoutFragment => nsTextLayoutFragment -> IO (Id NSArray)
textAttachmentViewProviders nsTextLayoutFragment  =
  sendMsg nsTextLayoutFragment (mkSelector "textAttachmentViewProviders") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTextElement:range:@
initWithTextElement_rangeSelector :: Selector
initWithTextElement_rangeSelector = mkSelector "initWithTextElement:range:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @textLineFragmentForVerticalOffset:requiresExactMatch:@
textLineFragmentForVerticalOffset_requiresExactMatchSelector :: Selector
textLineFragmentForVerticalOffset_requiresExactMatchSelector = mkSelector "textLineFragmentForVerticalOffset:requiresExactMatch:"

-- | @Selector@ for @textLineFragmentForTextLocation:isUpstreamAffinity:@
textLineFragmentForTextLocation_isUpstreamAffinitySelector :: Selector
textLineFragmentForTextLocation_isUpstreamAffinitySelector = mkSelector "textLineFragmentForTextLocation:isUpstreamAffinity:"

-- | @Selector@ for @invalidateLayout@
invalidateLayoutSelector :: Selector
invalidateLayoutSelector = mkSelector "invalidateLayout"

-- | @Selector@ for @textLayoutManager@
textLayoutManagerSelector :: Selector
textLayoutManagerSelector = mkSelector "textLayoutManager"

-- | @Selector@ for @textElement@
textElementSelector :: Selector
textElementSelector = mkSelector "textElement"

-- | @Selector@ for @rangeInElement@
rangeInElementSelector :: Selector
rangeInElementSelector = mkSelector "rangeInElement"

-- | @Selector@ for @textLineFragments@
textLineFragmentsSelector :: Selector
textLineFragmentsSelector = mkSelector "textLineFragments"

-- | @Selector@ for @layoutQueue@
layoutQueueSelector :: Selector
layoutQueueSelector = mkSelector "layoutQueue"

-- | @Selector@ for @setLayoutQueue:@
setLayoutQueueSelector :: Selector
setLayoutQueueSelector = mkSelector "setLayoutQueue:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @leadingPadding@
leadingPaddingSelector :: Selector
leadingPaddingSelector = mkSelector "leadingPadding"

-- | @Selector@ for @trailingPadding@
trailingPaddingSelector :: Selector
trailingPaddingSelector = mkSelector "trailingPadding"

-- | @Selector@ for @topMargin@
topMarginSelector :: Selector
topMarginSelector = mkSelector "topMargin"

-- | @Selector@ for @bottomMargin@
bottomMarginSelector :: Selector
bottomMarginSelector = mkSelector "bottomMargin"

-- | @Selector@ for @textAttachmentViewProviders@
textAttachmentViewProvidersSelector :: Selector
textAttachmentViewProvidersSelector = mkSelector "textAttachmentViewProviders"

