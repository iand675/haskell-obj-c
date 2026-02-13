{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
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
  , addRenderingAttribute_value_forTextRangeSelector
  , delegateSelector
  , ensureLayoutForRangeSelector
  , enumerateTextLayoutFragmentsFromLocation_options_usingBlockSelector
  , enumerateTextSegmentsInRange_type_options_usingBlockSelector
  , initSelector
  , initWithCoderSelector
  , invalidateLayoutForRangeSelector
  , invalidateRenderingAttributesForTextRangeSelector
  , layoutQueueSelector
  , limitsLayoutForSuspiciousContentsSelector
  , linkRenderingAttributesSelector
  , removeRenderingAttribute_forTextRangeSelector
  , renderingAttributesForLink_atLocationSelector
  , renderingAttributesValidatorSelector
  , replaceContentsInRange_withAttributedStringSelector
  , replaceContentsInRange_withTextElementsSelector
  , replaceTextContentManagerSelector
  , resolvesNaturalAlignmentWithBaseWritingDirectionSelector
  , setDelegateSelector
  , setLayoutQueueSelector
  , setLimitsLayoutForSuspiciousContentsSelector
  , setRenderingAttributesValidatorSelector
  , setRenderingAttributes_forTextRangeSelector
  , setResolvesNaturalAlignmentWithBaseWritingDirectionSelector
  , setTextContainerSelector
  , setTextSelectionNavigationSelector
  , setTextSelectionsSelector
  , setUsesFontLeadingSelector
  , setUsesHyphenationSelector
  , textContainerSelector
  , textContentManagerSelector
  , textLayoutFragmentForLocationSelector
  , textSelectionNavigationSelector
  , textSelectionsSelector
  , textViewportLayoutControllerSelector
  , usesFontLeadingSelector
  , usesHyphenationSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO (Id NSTextLayoutManager)
init_ nsTextLayoutManager =
  sendOwnedMessage nsTextLayoutManager initSelector

-- | @- initWithCoder:@
initWithCoder :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSCoder coder) => nsTextLayoutManager -> coder -> IO (Id NSTextLayoutManager)
initWithCoder nsTextLayoutManager coder =
  sendOwnedMessage nsTextLayoutManager initWithCoderSelector (toNSCoder coder)

-- | @- replaceTextContentManager:@
replaceTextContentManager :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSTextContentManager textContentManager) => nsTextLayoutManager -> textContentManager -> IO ()
replaceTextContentManager nsTextLayoutManager textContentManager =
  sendMessage nsTextLayoutManager replaceTextContentManagerSelector (toNSTextContentManager textContentManager)

-- | @- ensureLayoutForRange:@
ensureLayoutForRange :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSTextRange range) => nsTextLayoutManager -> range -> IO ()
ensureLayoutForRange nsTextLayoutManager range =
  sendMessage nsTextLayoutManager ensureLayoutForRangeSelector (toNSTextRange range)

-- | @- invalidateLayoutForRange:@
invalidateLayoutForRange :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSTextRange range) => nsTextLayoutManager -> range -> IO ()
invalidateLayoutForRange nsTextLayoutManager range =
  sendMessage nsTextLayoutManager invalidateLayoutForRangeSelector (toNSTextRange range)

-- | @- textLayoutFragmentForLocation:@
textLayoutFragmentForLocation :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> RawId -> IO (Id NSTextLayoutFragment)
textLayoutFragmentForLocation nsTextLayoutManager location =
  sendMessage nsTextLayoutManager textLayoutFragmentForLocationSelector location

-- | @- enumerateTextLayoutFragmentsFromLocation:options:usingBlock:@
enumerateTextLayoutFragmentsFromLocation_options_usingBlock :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> RawId -> NSTextLayoutFragmentEnumerationOptions -> Ptr () -> IO RawId
enumerateTextLayoutFragmentsFromLocation_options_usingBlock nsTextLayoutManager location options block =
  sendMessage nsTextLayoutManager enumerateTextLayoutFragmentsFromLocation_options_usingBlockSelector location options block

-- | @- setRenderingAttributes:forTextRange:@
setRenderingAttributes_forTextRange :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSDictionary renderingAttributes, IsNSTextRange textRange) => nsTextLayoutManager -> renderingAttributes -> textRange -> IO ()
setRenderingAttributes_forTextRange nsTextLayoutManager renderingAttributes textRange =
  sendMessage nsTextLayoutManager setRenderingAttributes_forTextRangeSelector (toNSDictionary renderingAttributes) (toNSTextRange textRange)

-- | @- addRenderingAttribute:value:forTextRange:@
addRenderingAttribute_value_forTextRange :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSString renderingAttribute, IsNSTextRange textRange) => nsTextLayoutManager -> renderingAttribute -> RawId -> textRange -> IO ()
addRenderingAttribute_value_forTextRange nsTextLayoutManager renderingAttribute value textRange =
  sendMessage nsTextLayoutManager addRenderingAttribute_value_forTextRangeSelector (toNSString renderingAttribute) value (toNSTextRange textRange)

-- | @- removeRenderingAttribute:forTextRange:@
removeRenderingAttribute_forTextRange :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSString renderingAttribute, IsNSTextRange textRange) => nsTextLayoutManager -> renderingAttribute -> textRange -> IO ()
removeRenderingAttribute_forTextRange nsTextLayoutManager renderingAttribute textRange =
  sendMessage nsTextLayoutManager removeRenderingAttribute_forTextRangeSelector (toNSString renderingAttribute) (toNSTextRange textRange)

-- | @- invalidateRenderingAttributesForTextRange:@
invalidateRenderingAttributesForTextRange :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSTextRange textRange) => nsTextLayoutManager -> textRange -> IO ()
invalidateRenderingAttributesForTextRange nsTextLayoutManager textRange =
  sendMessage nsTextLayoutManager invalidateRenderingAttributesForTextRangeSelector (toNSTextRange textRange)

-- | @- renderingAttributesForLink:atLocation:@
renderingAttributesForLink_atLocation :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> RawId -> RawId -> IO (Id NSDictionary)
renderingAttributesForLink_atLocation nsTextLayoutManager link location =
  sendMessage nsTextLayoutManager renderingAttributesForLink_atLocationSelector link location

-- | @- enumerateTextSegmentsInRange:type:options:usingBlock:@
enumerateTextSegmentsInRange_type_options_usingBlock :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSTextRange textRange) => nsTextLayoutManager -> textRange -> NSTextLayoutManagerSegmentType -> NSTextLayoutManagerSegmentOptions -> Ptr () -> IO ()
enumerateTextSegmentsInRange_type_options_usingBlock nsTextLayoutManager textRange type_ options block =
  sendMessage nsTextLayoutManager enumerateTextSegmentsInRange_type_options_usingBlockSelector (toNSTextRange textRange) type_ options block

-- | @- replaceContentsInRange:withTextElements:@
replaceContentsInRange_withTextElements :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSTextRange range, IsNSArray textElements) => nsTextLayoutManager -> range -> textElements -> IO ()
replaceContentsInRange_withTextElements nsTextLayoutManager range textElements =
  sendMessage nsTextLayoutManager replaceContentsInRange_withTextElementsSelector (toNSTextRange range) (toNSArray textElements)

-- | @- replaceContentsInRange:withAttributedString:@
replaceContentsInRange_withAttributedString :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSTextRange range, IsNSAttributedString attributedString) => nsTextLayoutManager -> range -> attributedString -> IO ()
replaceContentsInRange_withAttributedString nsTextLayoutManager range attributedString =
  sendMessage nsTextLayoutManager replaceContentsInRange_withAttributedStringSelector (toNSTextRange range) (toNSAttributedString attributedString)

-- | @- delegate@
delegate :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO RawId
delegate nsTextLayoutManager =
  sendMessage nsTextLayoutManager delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> RawId -> IO ()
setDelegate nsTextLayoutManager value =
  sendMessage nsTextLayoutManager setDelegateSelector value

-- | @- usesFontLeading@
usesFontLeading :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO Bool
usesFontLeading nsTextLayoutManager =
  sendMessage nsTextLayoutManager usesFontLeadingSelector

-- | @- setUsesFontLeading:@
setUsesFontLeading :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> Bool -> IO ()
setUsesFontLeading nsTextLayoutManager value =
  sendMessage nsTextLayoutManager setUsesFontLeadingSelector value

-- | @- limitsLayoutForSuspiciousContents@
limitsLayoutForSuspiciousContents :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO Bool
limitsLayoutForSuspiciousContents nsTextLayoutManager =
  sendMessage nsTextLayoutManager limitsLayoutForSuspiciousContentsSelector

-- | @- setLimitsLayoutForSuspiciousContents:@
setLimitsLayoutForSuspiciousContents :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> Bool -> IO ()
setLimitsLayoutForSuspiciousContents nsTextLayoutManager value =
  sendMessage nsTextLayoutManager setLimitsLayoutForSuspiciousContentsSelector value

-- | @- usesHyphenation@
usesHyphenation :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO Bool
usesHyphenation nsTextLayoutManager =
  sendMessage nsTextLayoutManager usesHyphenationSelector

-- | @- setUsesHyphenation:@
setUsesHyphenation :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> Bool -> IO ()
setUsesHyphenation nsTextLayoutManager value =
  sendMessage nsTextLayoutManager setUsesHyphenationSelector value

-- | Specifies the behavior for resolving ``NSTextAlignment.natural`` to the visual alignment.
--
-- When set to ``true``, the resolved visual alignment is determined by the resolved base writing direction; otherwise, it is using the user’s preferred language. The default value is ``true``.
--
-- ObjC selector: @- resolvesNaturalAlignmentWithBaseWritingDirection@
resolvesNaturalAlignmentWithBaseWritingDirection :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO Bool
resolvesNaturalAlignmentWithBaseWritingDirection nsTextLayoutManager =
  sendMessage nsTextLayoutManager resolvesNaturalAlignmentWithBaseWritingDirectionSelector

-- | Specifies the behavior for resolving ``NSTextAlignment.natural`` to the visual alignment.
--
-- When set to ``true``, the resolved visual alignment is determined by the resolved base writing direction; otherwise, it is using the user’s preferred language. The default value is ``true``.
--
-- ObjC selector: @- setResolvesNaturalAlignmentWithBaseWritingDirection:@
setResolvesNaturalAlignmentWithBaseWritingDirection :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> Bool -> IO ()
setResolvesNaturalAlignmentWithBaseWritingDirection nsTextLayoutManager value =
  sendMessage nsTextLayoutManager setResolvesNaturalAlignmentWithBaseWritingDirectionSelector value

-- | @- textContentManager@
textContentManager :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO (Id NSTextContentManager)
textContentManager nsTextLayoutManager =
  sendMessage nsTextLayoutManager textContentManagerSelector

-- | @- textContainer@
textContainer :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO (Id NSTextContainer)
textContainer nsTextLayoutManager =
  sendMessage nsTextLayoutManager textContainerSelector

-- | @- setTextContainer:@
setTextContainer :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSTextContainer value) => nsTextLayoutManager -> value -> IO ()
setTextContainer nsTextLayoutManager value =
  sendMessage nsTextLayoutManager setTextContainerSelector (toNSTextContainer value)

-- | @- textViewportLayoutController@
textViewportLayoutController :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO (Id NSTextViewportLayoutController)
textViewportLayoutController nsTextLayoutManager =
  sendMessage nsTextLayoutManager textViewportLayoutControllerSelector

-- | @- layoutQueue@
layoutQueue :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO (Id NSOperationQueue)
layoutQueue nsTextLayoutManager =
  sendMessage nsTextLayoutManager layoutQueueSelector

-- | @- setLayoutQueue:@
setLayoutQueue :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSOperationQueue value) => nsTextLayoutManager -> value -> IO ()
setLayoutQueue nsTextLayoutManager value =
  sendMessage nsTextLayoutManager setLayoutQueueSelector (toNSOperationQueue value)

-- | @- textSelections@
textSelections :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO (Id NSArray)
textSelections nsTextLayoutManager =
  sendMessage nsTextLayoutManager textSelectionsSelector

-- | @- setTextSelections:@
setTextSelections :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSArray value) => nsTextLayoutManager -> value -> IO ()
setTextSelections nsTextLayoutManager value =
  sendMessage nsTextLayoutManager setTextSelectionsSelector (toNSArray value)

-- | @- textSelectionNavigation@
textSelectionNavigation :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO (Id NSTextSelectionNavigation)
textSelectionNavigation nsTextLayoutManager =
  sendMessage nsTextLayoutManager textSelectionNavigationSelector

-- | @- setTextSelectionNavigation:@
setTextSelectionNavigation :: (IsNSTextLayoutManager nsTextLayoutManager, IsNSTextSelectionNavigation value) => nsTextLayoutManager -> value -> IO ()
setTextSelectionNavigation nsTextLayoutManager value =
  sendMessage nsTextLayoutManager setTextSelectionNavigationSelector (toNSTextSelectionNavigation value)

-- | @- renderingAttributesValidator@
renderingAttributesValidator :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> IO (Ptr ())
renderingAttributesValidator nsTextLayoutManager =
  sendMessage nsTextLayoutManager renderingAttributesValidatorSelector

-- | @- setRenderingAttributesValidator:@
setRenderingAttributesValidator :: IsNSTextLayoutManager nsTextLayoutManager => nsTextLayoutManager -> Ptr () -> IO ()
setRenderingAttributesValidator nsTextLayoutManager value =
  sendMessage nsTextLayoutManager setRenderingAttributesValidatorSelector value

-- | @+ linkRenderingAttributes@
linkRenderingAttributes :: IO (Id NSDictionary)
linkRenderingAttributes  =
  do
    cls' <- getRequiredClass "NSTextLayoutManager"
    sendClassMessage cls' linkRenderingAttributesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSTextLayoutManager)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSTextLayoutManager)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @replaceTextContentManager:@
replaceTextContentManagerSelector :: Selector '[Id NSTextContentManager] ()
replaceTextContentManagerSelector = mkSelector "replaceTextContentManager:"

-- | @Selector@ for @ensureLayoutForRange:@
ensureLayoutForRangeSelector :: Selector '[Id NSTextRange] ()
ensureLayoutForRangeSelector = mkSelector "ensureLayoutForRange:"

-- | @Selector@ for @invalidateLayoutForRange:@
invalidateLayoutForRangeSelector :: Selector '[Id NSTextRange] ()
invalidateLayoutForRangeSelector = mkSelector "invalidateLayoutForRange:"

-- | @Selector@ for @textLayoutFragmentForLocation:@
textLayoutFragmentForLocationSelector :: Selector '[RawId] (Id NSTextLayoutFragment)
textLayoutFragmentForLocationSelector = mkSelector "textLayoutFragmentForLocation:"

-- | @Selector@ for @enumerateTextLayoutFragmentsFromLocation:options:usingBlock:@
enumerateTextLayoutFragmentsFromLocation_options_usingBlockSelector :: Selector '[RawId, NSTextLayoutFragmentEnumerationOptions, Ptr ()] RawId
enumerateTextLayoutFragmentsFromLocation_options_usingBlockSelector = mkSelector "enumerateTextLayoutFragmentsFromLocation:options:usingBlock:"

-- | @Selector@ for @setRenderingAttributes:forTextRange:@
setRenderingAttributes_forTextRangeSelector :: Selector '[Id NSDictionary, Id NSTextRange] ()
setRenderingAttributes_forTextRangeSelector = mkSelector "setRenderingAttributes:forTextRange:"

-- | @Selector@ for @addRenderingAttribute:value:forTextRange:@
addRenderingAttribute_value_forTextRangeSelector :: Selector '[Id NSString, RawId, Id NSTextRange] ()
addRenderingAttribute_value_forTextRangeSelector = mkSelector "addRenderingAttribute:value:forTextRange:"

-- | @Selector@ for @removeRenderingAttribute:forTextRange:@
removeRenderingAttribute_forTextRangeSelector :: Selector '[Id NSString, Id NSTextRange] ()
removeRenderingAttribute_forTextRangeSelector = mkSelector "removeRenderingAttribute:forTextRange:"

-- | @Selector@ for @invalidateRenderingAttributesForTextRange:@
invalidateRenderingAttributesForTextRangeSelector :: Selector '[Id NSTextRange] ()
invalidateRenderingAttributesForTextRangeSelector = mkSelector "invalidateRenderingAttributesForTextRange:"

-- | @Selector@ for @renderingAttributesForLink:atLocation:@
renderingAttributesForLink_atLocationSelector :: Selector '[RawId, RawId] (Id NSDictionary)
renderingAttributesForLink_atLocationSelector = mkSelector "renderingAttributesForLink:atLocation:"

-- | @Selector@ for @enumerateTextSegmentsInRange:type:options:usingBlock:@
enumerateTextSegmentsInRange_type_options_usingBlockSelector :: Selector '[Id NSTextRange, NSTextLayoutManagerSegmentType, NSTextLayoutManagerSegmentOptions, Ptr ()] ()
enumerateTextSegmentsInRange_type_options_usingBlockSelector = mkSelector "enumerateTextSegmentsInRange:type:options:usingBlock:"

-- | @Selector@ for @replaceContentsInRange:withTextElements:@
replaceContentsInRange_withTextElementsSelector :: Selector '[Id NSTextRange, Id NSArray] ()
replaceContentsInRange_withTextElementsSelector = mkSelector "replaceContentsInRange:withTextElements:"

-- | @Selector@ for @replaceContentsInRange:withAttributedString:@
replaceContentsInRange_withAttributedStringSelector :: Selector '[Id NSTextRange, Id NSAttributedString] ()
replaceContentsInRange_withAttributedStringSelector = mkSelector "replaceContentsInRange:withAttributedString:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @usesFontLeading@
usesFontLeadingSelector :: Selector '[] Bool
usesFontLeadingSelector = mkSelector "usesFontLeading"

-- | @Selector@ for @setUsesFontLeading:@
setUsesFontLeadingSelector :: Selector '[Bool] ()
setUsesFontLeadingSelector = mkSelector "setUsesFontLeading:"

-- | @Selector@ for @limitsLayoutForSuspiciousContents@
limitsLayoutForSuspiciousContentsSelector :: Selector '[] Bool
limitsLayoutForSuspiciousContentsSelector = mkSelector "limitsLayoutForSuspiciousContents"

-- | @Selector@ for @setLimitsLayoutForSuspiciousContents:@
setLimitsLayoutForSuspiciousContentsSelector :: Selector '[Bool] ()
setLimitsLayoutForSuspiciousContentsSelector = mkSelector "setLimitsLayoutForSuspiciousContents:"

-- | @Selector@ for @usesHyphenation@
usesHyphenationSelector :: Selector '[] Bool
usesHyphenationSelector = mkSelector "usesHyphenation"

-- | @Selector@ for @setUsesHyphenation:@
setUsesHyphenationSelector :: Selector '[Bool] ()
setUsesHyphenationSelector = mkSelector "setUsesHyphenation:"

-- | @Selector@ for @resolvesNaturalAlignmentWithBaseWritingDirection@
resolvesNaturalAlignmentWithBaseWritingDirectionSelector :: Selector '[] Bool
resolvesNaturalAlignmentWithBaseWritingDirectionSelector = mkSelector "resolvesNaturalAlignmentWithBaseWritingDirection"

-- | @Selector@ for @setResolvesNaturalAlignmentWithBaseWritingDirection:@
setResolvesNaturalAlignmentWithBaseWritingDirectionSelector :: Selector '[Bool] ()
setResolvesNaturalAlignmentWithBaseWritingDirectionSelector = mkSelector "setResolvesNaturalAlignmentWithBaseWritingDirection:"

-- | @Selector@ for @textContentManager@
textContentManagerSelector :: Selector '[] (Id NSTextContentManager)
textContentManagerSelector = mkSelector "textContentManager"

-- | @Selector@ for @textContainer@
textContainerSelector :: Selector '[] (Id NSTextContainer)
textContainerSelector = mkSelector "textContainer"

-- | @Selector@ for @setTextContainer:@
setTextContainerSelector :: Selector '[Id NSTextContainer] ()
setTextContainerSelector = mkSelector "setTextContainer:"

-- | @Selector@ for @textViewportLayoutController@
textViewportLayoutControllerSelector :: Selector '[] (Id NSTextViewportLayoutController)
textViewportLayoutControllerSelector = mkSelector "textViewportLayoutController"

-- | @Selector@ for @layoutQueue@
layoutQueueSelector :: Selector '[] (Id NSOperationQueue)
layoutQueueSelector = mkSelector "layoutQueue"

-- | @Selector@ for @setLayoutQueue:@
setLayoutQueueSelector :: Selector '[Id NSOperationQueue] ()
setLayoutQueueSelector = mkSelector "setLayoutQueue:"

-- | @Selector@ for @textSelections@
textSelectionsSelector :: Selector '[] (Id NSArray)
textSelectionsSelector = mkSelector "textSelections"

-- | @Selector@ for @setTextSelections:@
setTextSelectionsSelector :: Selector '[Id NSArray] ()
setTextSelectionsSelector = mkSelector "setTextSelections:"

-- | @Selector@ for @textSelectionNavigation@
textSelectionNavigationSelector :: Selector '[] (Id NSTextSelectionNavigation)
textSelectionNavigationSelector = mkSelector "textSelectionNavigation"

-- | @Selector@ for @setTextSelectionNavigation:@
setTextSelectionNavigationSelector :: Selector '[Id NSTextSelectionNavigation] ()
setTextSelectionNavigationSelector = mkSelector "setTextSelectionNavigation:"

-- | @Selector@ for @renderingAttributesValidator@
renderingAttributesValidatorSelector :: Selector '[] (Ptr ())
renderingAttributesValidatorSelector = mkSelector "renderingAttributesValidator"

-- | @Selector@ for @setRenderingAttributesValidator:@
setRenderingAttributesValidatorSelector :: Selector '[Ptr ()] ()
setRenderingAttributesValidatorSelector = mkSelector "setRenderingAttributesValidator:"

-- | @Selector@ for @linkRenderingAttributes@
linkRenderingAttributesSelector :: Selector '[] (Id NSDictionary)
linkRenderingAttributesSelector = mkSelector "linkRenderingAttributes"

