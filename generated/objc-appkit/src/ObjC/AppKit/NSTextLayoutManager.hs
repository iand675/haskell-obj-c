{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextLayoutManager@.
module ObjC.AppKit.NSTextLayoutManager
  ( NSTextLayoutManager
  , IsNSTextLayoutManager(..)
  , init_
  , initWithCoder
  , replaceTextContentManager
  , ensureLayoutForRange
  , invalidateLayoutForRange
  , textLayoutFragmentForLocation
  , enumerateTextLayoutFragmentsFromLocation_options_usingBlock
  , setRenderingAttributes_forTextRange
  , addRenderingAttribute_value_forTextRange
  , removeRenderingAttribute_forTextRange
  , invalidateRenderingAttributesForTextRange
  , renderingAttributesForLink_atLocation
  , enumerateTextSegmentsInRange_type_options_usingBlock
  , replaceContentsInRange_withTextElements
  , replaceContentsInRange_withAttributedString
  , usesFontLeading
  , setUsesFontLeading
  , limitsLayoutForSuspiciousContents
  , setLimitsLayoutForSuspiciousContents
  , usesHyphenation
  , setUsesHyphenation
  , resolvesNaturalAlignmentWithBaseWritingDirection
  , setResolvesNaturalAlignmentWithBaseWritingDirection
  , textContentManager
  , textContainer
  , setTextContainer
  , textViewportLayoutController
  , layoutQueue
  , setLayoutQueue
  , textSelections
  , setTextSelections
  , textSelectionNavigation
  , setTextSelectionNavigation
  , renderingAttributesValidator
  , setRenderingAttributesValidator
  , linkRenderingAttributes
  , initSelector
  , initWithCoderSelector
  , replaceTextContentManagerSelector
  , ensureLayoutForRangeSelector
  , invalidateLayoutForRangeSelector
  , textLayoutFragmentForLocationSelector
  , enumerateTextLayoutFragmentsFromLocation_options_usingBlockSelector
  , setRenderingAttributes_forTextRangeSelector
  , addRenderingAttribute_value_forTextRangeSelector
  , removeRenderingAttribute_forTextRangeSelector
  , invalidateRenderingAttributesForTextRangeSelector
  , renderingAttributesForLink_atLocationSelector
  , enumerateTextSegmentsInRange_type_options_usingBlockSelector
  , replaceContentsInRange_withTextElementsSelector
  , replaceContentsInRange_withAttributedStringSelector
  , usesFontLeadingSelector
  , setUsesFontLeadingSelector
  , limitsLayoutForSuspiciousContentsSelector
  , setLimitsLayoutForSuspiciousContentsSelector
  , usesHyphenationSelector
  , setUsesHyphenationSelector
  , resolvesNaturalAlignmentWithBaseWritingDirectionSelector
  , setResolvesNaturalAlignmentWithBaseWritingDirectionSelector
  , textContentManagerSelector
  , textContainerSelector
  , setTextContainerSelector
  , textViewportLayoutControllerSelector
  , layoutQueueSelector
  , setLayoutQueueSelector
  , textSelectionsSelector
  , setTextSelectionsSelector
  , textSelectionNavigationSelector
  , setTextSelectionNavigationSelector
  , renderingAttributesValidatorSelector
  , setRenderingAttributesValidatorSelector
  , linkRenderingAttributesSelector

  -- * Enum types
  , NSTextLayoutFragmentEnumerationOptions(NSTextLayoutFragmentEnumerationOptions)
  , pattern NSTextLayoutFragmentEnumerationOptionsNone
  , pattern NSTextLayoutFragmentEnumerationOptionsReverse
  , pattern NSTextLayoutFragmentEnumerationOptionsEstimatesSize
  , pattern NSTextLayoutFragmentEnumerationOptionsEnsuresLayout
  , pattern NSTextLayoutFragmentEnumerationOptionsEnsuresExtraLineFragment
  , NSTextLayoutManagerSegmentOptions(NSTextLayoutManagerSegmentOptions)
  , pattern NSTextLayoutManagerSegmentOptionsNone
  , pattern NSTextLayoutManagerSegmentOptionsRangeNotRequired
  , pattern NSTextLayoutManagerSegmentOptionsMiddleFragmentsExcluded
  , pattern NSTextLayoutManagerSegmentOptionsHeadSegmentExtended
  , pattern NSTextLayoutManagerSegmentOptionsTailSegmentExtended
  , pattern NSTextLayoutManagerSegmentOptionsUpstreamAffinity
  , NSTextLayoutManagerSegmentType(NSTextLayoutManagerSegmentType)
  , pattern NSTextLayoutManagerSegmentTypeStandard
  , pattern NSTextLayoutManagerSegmentTypeSelection
  , pattern NSTextLayoutManagerSegmentTypeHighlight

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

-- | @- init@
init_ :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO (Id NSTextLayoutManager)
init_ nsTextLayoutManager  =
  sendMsg nsTextLayoutManager (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSCoder coder) => nsTextLayoutManager -> coder -> IO (Id NSTextLayoutManager)
initWithCoder nsTextLayoutManager  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsTextLayoutManager (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- replaceTextContentManager:@
replaceTextContentManager :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSTextContentManager textContentManager) => nsTextLayoutManager -> textContentManager -> IO ()
replaceTextContentManager nsTextLayoutManager  textContentManager =
withObjCPtr textContentManager $ \raw_textContentManager ->
    sendMsg nsTextLayoutManager (mkSelector "replaceTextContentManager:") retVoid [argPtr (castPtr raw_textContentManager :: Ptr ())]

-- | @- ensureLayoutForRange:@
ensureLayoutForRange :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSTextRange range) => nsTextLayoutManager -> range -> IO ()
ensureLayoutForRange nsTextLayoutManager  range =
withObjCPtr range $ \raw_range ->
    sendMsg nsTextLayoutManager (mkSelector "ensureLayoutForRange:") retVoid [argPtr (castPtr raw_range :: Ptr ())]

-- | @- invalidateLayoutForRange:@
invalidateLayoutForRange :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSTextRange range) => nsTextLayoutManager -> range -> IO ()
invalidateLayoutForRange nsTextLayoutManager  range =
withObjCPtr range $ \raw_range ->
    sendMsg nsTextLayoutManager (mkSelector "invalidateLayoutForRange:") retVoid [argPtr (castPtr raw_range :: Ptr ())]

-- | @- textLayoutFragmentForLocation:@
textLayoutFragmentForLocation :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> RawId -> IO (Id NSTextLayoutFragment)
textLayoutFragmentForLocation nsTextLayoutManager  location =
  sendMsg nsTextLayoutManager (mkSelector "textLayoutFragmentForLocation:") (retPtr retVoid) [argPtr (castPtr (unRawId location) :: Ptr ())] >>= retainedObject . castPtr

-- | @- enumerateTextLayoutFragmentsFromLocation:options:usingBlock:@
enumerateTextLayoutFragmentsFromLocation_options_usingBlock :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> RawId -> NSTextLayoutFragmentEnumerationOptions -> Ptr () -> IO RawId
enumerateTextLayoutFragmentsFromLocation_options_usingBlock nsTextLayoutManager  location options block =
  fmap (RawId . castPtr) $ sendMsg nsTextLayoutManager (mkSelector "enumerateTextLayoutFragmentsFromLocation:options:usingBlock:") (retPtr retVoid) [argPtr (castPtr (unRawId location) :: Ptr ()), argCULong (coerce options), argPtr (castPtr block :: Ptr ())]

-- | @- setRenderingAttributes:forTextRange:@
setRenderingAttributes_forTextRange :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSDictionary renderingAttributes, IsNSTextRange textRange) => nsTextLayoutManager -> renderingAttributes -> textRange -> IO ()
setRenderingAttributes_forTextRange nsTextLayoutManager  renderingAttributes textRange =
withObjCPtr renderingAttributes $ \raw_renderingAttributes ->
  withObjCPtr textRange $ \raw_textRange ->
      sendMsg nsTextLayoutManager (mkSelector "setRenderingAttributes:forTextRange:") retVoid [argPtr (castPtr raw_renderingAttributes :: Ptr ()), argPtr (castPtr raw_textRange :: Ptr ())]

-- | @- addRenderingAttribute:value:forTextRange:@
addRenderingAttribute_value_forTextRange :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSString renderingAttribute, IsNSTextRange textRange) => nsTextLayoutManager -> renderingAttribute -> RawId -> textRange -> IO ()
addRenderingAttribute_value_forTextRange nsTextLayoutManager  renderingAttribute value textRange =
withObjCPtr renderingAttribute $ \raw_renderingAttribute ->
  withObjCPtr textRange $ \raw_textRange ->
      sendMsg nsTextLayoutManager (mkSelector "addRenderingAttribute:value:forTextRange:") retVoid [argPtr (castPtr raw_renderingAttribute :: Ptr ()), argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_textRange :: Ptr ())]

-- | @- removeRenderingAttribute:forTextRange:@
removeRenderingAttribute_forTextRange :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSString renderingAttribute, IsNSTextRange textRange) => nsTextLayoutManager -> renderingAttribute -> textRange -> IO ()
removeRenderingAttribute_forTextRange nsTextLayoutManager  renderingAttribute textRange =
withObjCPtr renderingAttribute $ \raw_renderingAttribute ->
  withObjCPtr textRange $ \raw_textRange ->
      sendMsg nsTextLayoutManager (mkSelector "removeRenderingAttribute:forTextRange:") retVoid [argPtr (castPtr raw_renderingAttribute :: Ptr ()), argPtr (castPtr raw_textRange :: Ptr ())]

-- | @- invalidateRenderingAttributesForTextRange:@
invalidateRenderingAttributesForTextRange :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSTextRange textRange) => nsTextLayoutManager -> textRange -> IO ()
invalidateRenderingAttributesForTextRange nsTextLayoutManager  textRange =
withObjCPtr textRange $ \raw_textRange ->
    sendMsg nsTextLayoutManager (mkSelector "invalidateRenderingAttributesForTextRange:") retVoid [argPtr (castPtr raw_textRange :: Ptr ())]

-- | @- renderingAttributesForLink:atLocation:@
renderingAttributesForLink_atLocation :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> RawId -> RawId -> IO (Id NSDictionary)
renderingAttributesForLink_atLocation nsTextLayoutManager  link location =
  sendMsg nsTextLayoutManager (mkSelector "renderingAttributesForLink:atLocation:") (retPtr retVoid) [argPtr (castPtr (unRawId link) :: Ptr ()), argPtr (castPtr (unRawId location) :: Ptr ())] >>= retainedObject . castPtr

-- | @- enumerateTextSegmentsInRange:type:options:usingBlock:@
enumerateTextSegmentsInRange_type_options_usingBlock :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSTextRange textRange) => nsTextLayoutManager -> textRange -> NSTextLayoutManagerSegmentType -> NSTextLayoutManagerSegmentOptions -> Ptr () -> IO ()
enumerateTextSegmentsInRange_type_options_usingBlock nsTextLayoutManager  textRange type_ options block =
withObjCPtr textRange $ \raw_textRange ->
    sendMsg nsTextLayoutManager (mkSelector "enumerateTextSegmentsInRange:type:options:usingBlock:") retVoid [argPtr (castPtr raw_textRange :: Ptr ()), argCLong (coerce type_), argCULong (coerce options), argPtr (castPtr block :: Ptr ())]

-- | @- replaceContentsInRange:withTextElements:@
replaceContentsInRange_withTextElements :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSTextRange range, IsNSArray textElements) => nsTextLayoutManager -> range -> textElements -> IO ()
replaceContentsInRange_withTextElements nsTextLayoutManager  range textElements =
withObjCPtr range $ \raw_range ->
  withObjCPtr textElements $ \raw_textElements ->
      sendMsg nsTextLayoutManager (mkSelector "replaceContentsInRange:withTextElements:") retVoid [argPtr (castPtr raw_range :: Ptr ()), argPtr (castPtr raw_textElements :: Ptr ())]

-- | @- replaceContentsInRange:withAttributedString:@
replaceContentsInRange_withAttributedString :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSTextRange range, IsNSAttributedString attributedString) => nsTextLayoutManager -> range -> attributedString -> IO ()
replaceContentsInRange_withAttributedString nsTextLayoutManager  range attributedString =
withObjCPtr range $ \raw_range ->
  withObjCPtr attributedString $ \raw_attributedString ->
      sendMsg nsTextLayoutManager (mkSelector "replaceContentsInRange:withAttributedString:") retVoid [argPtr (castPtr raw_range :: Ptr ()), argPtr (castPtr raw_attributedString :: Ptr ())]

-- | @- usesFontLeading@
usesFontLeading :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO Bool
usesFontLeading nsTextLayoutManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextLayoutManager (mkSelector "usesFontLeading") retCULong []

-- | @- setUsesFontLeading:@
setUsesFontLeading :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> Bool -> IO ()
setUsesFontLeading nsTextLayoutManager  value =
  sendMsg nsTextLayoutManager (mkSelector "setUsesFontLeading:") retVoid [argCULong (if value then 1 else 0)]

-- | @- limitsLayoutForSuspiciousContents@
limitsLayoutForSuspiciousContents :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO Bool
limitsLayoutForSuspiciousContents nsTextLayoutManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextLayoutManager (mkSelector "limitsLayoutForSuspiciousContents") retCULong []

-- | @- setLimitsLayoutForSuspiciousContents:@
setLimitsLayoutForSuspiciousContents :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> Bool -> IO ()
setLimitsLayoutForSuspiciousContents nsTextLayoutManager  value =
  sendMsg nsTextLayoutManager (mkSelector "setLimitsLayoutForSuspiciousContents:") retVoid [argCULong (if value then 1 else 0)]

-- | @- usesHyphenation@
usesHyphenation :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO Bool
usesHyphenation nsTextLayoutManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextLayoutManager (mkSelector "usesHyphenation") retCULong []

-- | @- setUsesHyphenation:@
setUsesHyphenation :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> Bool -> IO ()
setUsesHyphenation nsTextLayoutManager  value =
  sendMsg nsTextLayoutManager (mkSelector "setUsesHyphenation:") retVoid [argCULong (if value then 1 else 0)]

-- | Specifies the behavior for resolving ``NSTextAlignment.natural`` to the visual alignment.
--
-- When set to ``true``, the resolved visual alignment is determined by the resolved base writing direction; otherwise, it is using the user’s preferred language. The default value is ``true``.
--
-- ObjC selector: @- resolvesNaturalAlignmentWithBaseWritingDirection@
resolvesNaturalAlignmentWithBaseWritingDirection :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO Bool
resolvesNaturalAlignmentWithBaseWritingDirection nsTextLayoutManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextLayoutManager (mkSelector "resolvesNaturalAlignmentWithBaseWritingDirection") retCULong []

-- | Specifies the behavior for resolving ``NSTextAlignment.natural`` to the visual alignment.
--
-- When set to ``true``, the resolved visual alignment is determined by the resolved base writing direction; otherwise, it is using the user’s preferred language. The default value is ``true``.
--
-- ObjC selector: @- setResolvesNaturalAlignmentWithBaseWritingDirection:@
setResolvesNaturalAlignmentWithBaseWritingDirection :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> Bool -> IO ()
setResolvesNaturalAlignmentWithBaseWritingDirection nsTextLayoutManager  value =
  sendMsg nsTextLayoutManager (mkSelector "setResolvesNaturalAlignmentWithBaseWritingDirection:") retVoid [argCULong (if value then 1 else 0)]

-- | @- textContentManager@
textContentManager :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO (Id NSTextContentManager)
textContentManager nsTextLayoutManager  =
  sendMsg nsTextLayoutManager (mkSelector "textContentManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- textContainer@
textContainer :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO (Id NSTextContainer)
textContainer nsTextLayoutManager  =
  sendMsg nsTextLayoutManager (mkSelector "textContainer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextContainer:@
setTextContainer :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSTextContainer value) => nsTextLayoutManager -> value -> IO ()
setTextContainer nsTextLayoutManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextLayoutManager (mkSelector "setTextContainer:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- textViewportLayoutController@
textViewportLayoutController :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO (Id NSTextViewportLayoutController)
textViewportLayoutController nsTextLayoutManager  =
  sendMsg nsTextLayoutManager (mkSelector "textViewportLayoutController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- layoutQueue@
layoutQueue :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO (Id NSOperationQueue)
layoutQueue nsTextLayoutManager  =
  sendMsg nsTextLayoutManager (mkSelector "layoutQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLayoutQueue:@
setLayoutQueue :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSOperationQueue value) => nsTextLayoutManager -> value -> IO ()
setLayoutQueue nsTextLayoutManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextLayoutManager (mkSelector "setLayoutQueue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- textSelections@
textSelections :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO (Id NSArray)
textSelections nsTextLayoutManager  =
  sendMsg nsTextLayoutManager (mkSelector "textSelections") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextSelections:@
setTextSelections :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSArray value) => nsTextLayoutManager -> value -> IO ()
setTextSelections nsTextLayoutManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextLayoutManager (mkSelector "setTextSelections:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- textSelectionNavigation@
textSelectionNavigation :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO (Id NSTextSelectionNavigation)
textSelectionNavigation nsTextLayoutManager  =
  sendMsg nsTextLayoutManager (mkSelector "textSelectionNavigation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextSelectionNavigation:@
setTextSelectionNavigation :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSTextSelectionNavigation value) => nsTextLayoutManager -> value -> IO ()
setTextSelectionNavigation nsTextLayoutManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextLayoutManager (mkSelector "setTextSelectionNavigation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- renderingAttributesValidator@
renderingAttributesValidator :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO (Ptr ())
renderingAttributesValidator nsTextLayoutManager  =
  fmap castPtr $ sendMsg nsTextLayoutManager (mkSelector "renderingAttributesValidator") (retPtr retVoid) []

-- | @- setRenderingAttributesValidator:@
setRenderingAttributesValidator :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> Ptr () -> IO ()
setRenderingAttributesValidator nsTextLayoutManager  value =
  sendMsg nsTextLayoutManager (mkSelector "setRenderingAttributesValidator:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @+ linkRenderingAttributes@
linkRenderingAttributes :: IO (Id NSDictionary)
linkRenderingAttributes  =
  do
    cls' <- getRequiredClass "NSTextLayoutManager"
    sendClassMsg cls' (mkSelector "linkRenderingAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @replaceTextContentManager:@
replaceTextContentManagerSelector :: Selector
replaceTextContentManagerSelector = mkSelector "replaceTextContentManager:"

-- | @Selector@ for @ensureLayoutForRange:@
ensureLayoutForRangeSelector :: Selector
ensureLayoutForRangeSelector = mkSelector "ensureLayoutForRange:"

-- | @Selector@ for @invalidateLayoutForRange:@
invalidateLayoutForRangeSelector :: Selector
invalidateLayoutForRangeSelector = mkSelector "invalidateLayoutForRange:"

-- | @Selector@ for @textLayoutFragmentForLocation:@
textLayoutFragmentForLocationSelector :: Selector
textLayoutFragmentForLocationSelector = mkSelector "textLayoutFragmentForLocation:"

-- | @Selector@ for @enumerateTextLayoutFragmentsFromLocation:options:usingBlock:@
enumerateTextLayoutFragmentsFromLocation_options_usingBlockSelector :: Selector
enumerateTextLayoutFragmentsFromLocation_options_usingBlockSelector = mkSelector "enumerateTextLayoutFragmentsFromLocation:options:usingBlock:"

-- | @Selector@ for @setRenderingAttributes:forTextRange:@
setRenderingAttributes_forTextRangeSelector :: Selector
setRenderingAttributes_forTextRangeSelector = mkSelector "setRenderingAttributes:forTextRange:"

-- | @Selector@ for @addRenderingAttribute:value:forTextRange:@
addRenderingAttribute_value_forTextRangeSelector :: Selector
addRenderingAttribute_value_forTextRangeSelector = mkSelector "addRenderingAttribute:value:forTextRange:"

-- | @Selector@ for @removeRenderingAttribute:forTextRange:@
removeRenderingAttribute_forTextRangeSelector :: Selector
removeRenderingAttribute_forTextRangeSelector = mkSelector "removeRenderingAttribute:forTextRange:"

-- | @Selector@ for @invalidateRenderingAttributesForTextRange:@
invalidateRenderingAttributesForTextRangeSelector :: Selector
invalidateRenderingAttributesForTextRangeSelector = mkSelector "invalidateRenderingAttributesForTextRange:"

-- | @Selector@ for @renderingAttributesForLink:atLocation:@
renderingAttributesForLink_atLocationSelector :: Selector
renderingAttributesForLink_atLocationSelector = mkSelector "renderingAttributesForLink:atLocation:"

-- | @Selector@ for @enumerateTextSegmentsInRange:type:options:usingBlock:@
enumerateTextSegmentsInRange_type_options_usingBlockSelector :: Selector
enumerateTextSegmentsInRange_type_options_usingBlockSelector = mkSelector "enumerateTextSegmentsInRange:type:options:usingBlock:"

-- | @Selector@ for @replaceContentsInRange:withTextElements:@
replaceContentsInRange_withTextElementsSelector :: Selector
replaceContentsInRange_withTextElementsSelector = mkSelector "replaceContentsInRange:withTextElements:"

-- | @Selector@ for @replaceContentsInRange:withAttributedString:@
replaceContentsInRange_withAttributedStringSelector :: Selector
replaceContentsInRange_withAttributedStringSelector = mkSelector "replaceContentsInRange:withAttributedString:"

-- | @Selector@ for @usesFontLeading@
usesFontLeadingSelector :: Selector
usesFontLeadingSelector = mkSelector "usesFontLeading"

-- | @Selector@ for @setUsesFontLeading:@
setUsesFontLeadingSelector :: Selector
setUsesFontLeadingSelector = mkSelector "setUsesFontLeading:"

-- | @Selector@ for @limitsLayoutForSuspiciousContents@
limitsLayoutForSuspiciousContentsSelector :: Selector
limitsLayoutForSuspiciousContentsSelector = mkSelector "limitsLayoutForSuspiciousContents"

-- | @Selector@ for @setLimitsLayoutForSuspiciousContents:@
setLimitsLayoutForSuspiciousContentsSelector :: Selector
setLimitsLayoutForSuspiciousContentsSelector = mkSelector "setLimitsLayoutForSuspiciousContents:"

-- | @Selector@ for @usesHyphenation@
usesHyphenationSelector :: Selector
usesHyphenationSelector = mkSelector "usesHyphenation"

-- | @Selector@ for @setUsesHyphenation:@
setUsesHyphenationSelector :: Selector
setUsesHyphenationSelector = mkSelector "setUsesHyphenation:"

-- | @Selector@ for @resolvesNaturalAlignmentWithBaseWritingDirection@
resolvesNaturalAlignmentWithBaseWritingDirectionSelector :: Selector
resolvesNaturalAlignmentWithBaseWritingDirectionSelector = mkSelector "resolvesNaturalAlignmentWithBaseWritingDirection"

-- | @Selector@ for @setResolvesNaturalAlignmentWithBaseWritingDirection:@
setResolvesNaturalAlignmentWithBaseWritingDirectionSelector :: Selector
setResolvesNaturalAlignmentWithBaseWritingDirectionSelector = mkSelector "setResolvesNaturalAlignmentWithBaseWritingDirection:"

-- | @Selector@ for @textContentManager@
textContentManagerSelector :: Selector
textContentManagerSelector = mkSelector "textContentManager"

-- | @Selector@ for @textContainer@
textContainerSelector :: Selector
textContainerSelector = mkSelector "textContainer"

-- | @Selector@ for @setTextContainer:@
setTextContainerSelector :: Selector
setTextContainerSelector = mkSelector "setTextContainer:"

-- | @Selector@ for @textViewportLayoutController@
textViewportLayoutControllerSelector :: Selector
textViewportLayoutControllerSelector = mkSelector "textViewportLayoutController"

-- | @Selector@ for @layoutQueue@
layoutQueueSelector :: Selector
layoutQueueSelector = mkSelector "layoutQueue"

-- | @Selector@ for @setLayoutQueue:@
setLayoutQueueSelector :: Selector
setLayoutQueueSelector = mkSelector "setLayoutQueue:"

-- | @Selector@ for @textSelections@
textSelectionsSelector :: Selector
textSelectionsSelector = mkSelector "textSelections"

-- | @Selector@ for @setTextSelections:@
setTextSelectionsSelector :: Selector
setTextSelectionsSelector = mkSelector "setTextSelections:"

-- | @Selector@ for @textSelectionNavigation@
textSelectionNavigationSelector :: Selector
textSelectionNavigationSelector = mkSelector "textSelectionNavigation"

-- | @Selector@ for @setTextSelectionNavigation:@
setTextSelectionNavigationSelector :: Selector
setTextSelectionNavigationSelector = mkSelector "setTextSelectionNavigation:"

-- | @Selector@ for @renderingAttributesValidator@
renderingAttributesValidatorSelector :: Selector
renderingAttributesValidatorSelector = mkSelector "renderingAttributesValidator"

-- | @Selector@ for @setRenderingAttributesValidator:@
setRenderingAttributesValidatorSelector :: Selector
setRenderingAttributesValidatorSelector = mkSelector "setRenderingAttributesValidator:"

-- | @Selector@ for @linkRenderingAttributes@
linkRenderingAttributesSelector :: Selector
linkRenderingAttributesSelector = mkSelector "linkRenderingAttributes"

