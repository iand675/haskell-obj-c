{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextView@.
module ObjC.AppKit.NSTextView
  ( NSTextView
  , IsNSTextView(..)
  , initWithFrame_textContainer
  , initWithCoder
  , initWithFrame
  , initUsingTextLayoutManager
  , textViewUsingTextLayoutManager
  , replaceTextContainer
  , invalidateTextContainerOrigin
  , insertText
  , setConstrainedFrameSize
  , setAlignment_range
  , setBaseWritingDirection_range
  , turnOffKerning
  , tightenKerning
  , loosenKerning
  , useStandardKerning
  , turnOffLigatures
  , useStandardLigatures
  , useAllLigatures
  , raiseBaseline
  , lowerBaseline
  , toggleTraditionalCharacterShape
  , outline
  , performFindPanelAction
  , alignJustified
  , changeColor
  , changeAttributes
  , changeDocumentBackgroundColor
  , orderFrontSpacingPanel
  , orderFrontLinkPanel
  , orderFrontListPanel
  , orderFrontTablePanel
  , rulerView_didMoveMarker
  , rulerView_didRemoveMarker
  , rulerView_didAddMarker
  , rulerView_shouldMoveMarker
  , rulerView_shouldAddMarker
  , rulerView_willMoveMarker_toLocation
  , rulerView_shouldRemoveMarker
  , rulerView_willAddMarker_atLocation
  , rulerView_handleMouseDown
  , setNeedsDisplayInRect_avoidAdditionalLayout
  , drawInsertionPointInRect_color_turnedOn
  , drawViewBackgroundInRect
  , updateRuler
  , updateFontPanel
  , updateDragTypeRegistration
  , selectionRangeForProposedRange_granularity
  , clickedOnLink_atIndex
  , startSpeaking
  , stopSpeaking
  , setLayoutOrientation
  , changeLayoutOrientation
  , characterIndexForInsertionAtPoint
  , performValidatedReplacementInRange_withAttributedString
  , toggleBaseWritingDirection
  , drawTextHighlightBackgroundForTextRange_origin
  , highlight
  , scrollableTextView
  , nsTextViewFieldEditor
  , scrollableDocumentContentTextView
  , scrollablePlainDocumentContentTextView
  , toggleAutomaticTextCompletion
  , updateTouchBarItemIdentifiers
  , updateTextTouchBarItems
  , updateCandidates
  , orderFrontSharingServicePicker
  , toggleQuickLookPreviewPanel
  , quickLookPreviewableItemsInRanges
  , updateQuickLookPreviewPanel
  , smartDeleteRangeForProposedRange
  , toggleSmartInsertDelete
  , smartInsertForString_replacingRange_beforeString_afterString
  , smartInsertBeforeStringForString_replacingRange
  , smartInsertAfterStringForString_replacingRange
  , toggleAutomaticQuoteSubstitution
  , toggleAutomaticLinkDetection
  , toggleAutomaticDataDetection
  , toggleAutomaticDashSubstitution
  , toggleAutomaticTextReplacement
  , toggleAutomaticSpellingCorrection
  , checkTextInRange_types_options
  , handleTextCheckingResults_forRange_types_options_orthography_wordCount
  , orderFrontSubstitutionsPanel
  , checkTextInSelection
  , checkTextInDocument
  , setSelectedRanges_affinity_stillSelecting
  , setSelectedRange_affinity_stillSelecting
  , updateInsertionPointStateAndRestartTimer
  , toggleContinuousSpellChecking
  , toggleGrammarChecking
  , setSpellingState_range
  , shouldChangeTextInRanges_replacementStrings
  , shouldChangeTextInRange_replacementString
  , didChangeText
  , breakUndoCoalescing
  , showFindIndicatorForRange
  , setSelectedRange
  , dragSelectionWithEvent_offset_slideBack
  , dragImageForSelectionWithEvent_origin
  , dragOperationForDraggingInfo_type
  , cleanUpAfterDragOperation
  , writeSelectionToPasteboard_type
  , writeSelectionToPasteboard_types
  , preferredPasteboardTypeFromArray_restrictedToTypesFromArray
  , readSelectionFromPasteboard_type
  , readSelectionFromPasteboard
  , registerForServices
  , validRequestorForSendType_returnType
  , pasteAsPlainText
  , pasteAsRichText
  , complete
  , completionsForPartialWordRange_indexOfSelectedItem
  , insertCompletion_forPartialWordRange_movement_isFinal
  , textContainer
  , setTextContainer
  , textContainerInset
  , setTextContainerInset
  , textContainerOrigin
  , layoutManager
  , textStorage
  , shouldDrawInsertionPoint
  , stronglyReferencesTextStorage
  , usesAdaptiveColorMappingForDarkAppearance
  , setUsesAdaptiveColorMappingForDarkAppearance
  , automaticTextCompletionEnabled
  , setAutomaticTextCompletionEnabled
  , allowsCharacterPickerTouchBarItem
  , setAllowsCharacterPickerTouchBarItem
  , smartInsertDeleteEnabled
  , setSmartInsertDeleteEnabled
  , automaticQuoteSubstitutionEnabled
  , setAutomaticQuoteSubstitutionEnabled
  , automaticLinkDetectionEnabled
  , setAutomaticLinkDetectionEnabled
  , automaticDataDetectionEnabled
  , setAutomaticDataDetectionEnabled
  , automaticDashSubstitutionEnabled
  , setAutomaticDashSubstitutionEnabled
  , automaticTextReplacementEnabled
  , setAutomaticTextReplacementEnabled
  , automaticSpellingCorrectionEnabled
  , setAutomaticSpellingCorrectionEnabled
  , enabledTextCheckingTypes
  , setEnabledTextCheckingTypes
  , usesFindPanel
  , setUsesFindPanel
  , usesFindBar
  , setUsesFindBar
  , incrementalSearchingEnabled
  , setIncrementalSearchingEnabled
  , inlinePredictionType
  , setInlinePredictionType
  , mathExpressionCompletionType
  , setMathExpressionCompletionType
  , selectedRanges
  , setSelectedRanges
  , selectionAffinity
  , selectionGranularity
  , setSelectionGranularity
  , selectedTextAttributes
  , setSelectedTextAttributes
  , insertionPointColor
  , setInsertionPointColor
  , markedTextAttributes
  , setMarkedTextAttributes
  , linkTextAttributes
  , setLinkTextAttributes
  , displaysLinkToolTips
  , setDisplaysLinkToolTips
  , acceptsGlyphInfo
  , setAcceptsGlyphInfo
  , usesRuler
  , setUsesRuler
  , usesInspectorBar
  , setUsesInspectorBar
  , continuousSpellCheckingEnabled
  , setContinuousSpellCheckingEnabled
  , spellCheckerDocumentTag
  , grammarCheckingEnabled
  , setGrammarCheckingEnabled
  , typingAttributes
  , setTypingAttributes
  , rangesForUserTextChange
  , rangesForUserCharacterAttributeChange
  , rangesForUserParagraphAttributeChange
  , rangeForUserTextChange
  , rangeForUserCharacterAttributeChange
  , rangeForUserParagraphAttributeChange
  , allowsDocumentBackgroundColorChange
  , setAllowsDocumentBackgroundColorChange
  , defaultParagraphStyle
  , setDefaultParagraphStyle
  , allowsUndo
  , setAllowsUndo
  , coalescingUndo
  , allowsImageEditing
  , setAllowsImageEditing
  , usesRolloverButtonForSelection
  , setUsesRolloverButtonForSelection
  , editable
  , setEditable
  , selectable
  , setSelectable
  , richText
  , setRichText
  , importsGraphics
  , setImportsGraphics
  , drawsBackground
  , setDrawsBackground
  , backgroundColor
  , setBackgroundColor
  , fieldEditor
  , setFieldEditor
  , usesFontPanel
  , setUsesFontPanel
  , rulerVisible
  , setRulerVisible
  , writingToolsActive
  , writingToolsBehavior
  , setWritingToolsBehavior
  , allowedWritingToolsResultOptions
  , setAllowedWritingToolsResultOptions
  , acceptableDragTypes
  , writablePasteboardTypes
  , readablePasteboardTypes
  , rangeForUserCompletion
  , initWithFrame_textContainerSelector
  , initWithCoderSelector
  , initWithFrameSelector
  , initUsingTextLayoutManagerSelector
  , textViewUsingTextLayoutManagerSelector
  , replaceTextContainerSelector
  , invalidateTextContainerOriginSelector
  , insertTextSelector
  , setConstrainedFrameSizeSelector
  , setAlignment_rangeSelector
  , setBaseWritingDirection_rangeSelector
  , turnOffKerningSelector
  , tightenKerningSelector
  , loosenKerningSelector
  , useStandardKerningSelector
  , turnOffLigaturesSelector
  , useStandardLigaturesSelector
  , useAllLigaturesSelector
  , raiseBaselineSelector
  , lowerBaselineSelector
  , toggleTraditionalCharacterShapeSelector
  , outlineSelector
  , performFindPanelActionSelector
  , alignJustifiedSelector
  , changeColorSelector
  , changeAttributesSelector
  , changeDocumentBackgroundColorSelector
  , orderFrontSpacingPanelSelector
  , orderFrontLinkPanelSelector
  , orderFrontListPanelSelector
  , orderFrontTablePanelSelector
  , rulerView_didMoveMarkerSelector
  , rulerView_didRemoveMarkerSelector
  , rulerView_didAddMarkerSelector
  , rulerView_shouldMoveMarkerSelector
  , rulerView_shouldAddMarkerSelector
  , rulerView_willMoveMarker_toLocationSelector
  , rulerView_shouldRemoveMarkerSelector
  , rulerView_willAddMarker_atLocationSelector
  , rulerView_handleMouseDownSelector
  , setNeedsDisplayInRect_avoidAdditionalLayoutSelector
  , drawInsertionPointInRect_color_turnedOnSelector
  , drawViewBackgroundInRectSelector
  , updateRulerSelector
  , updateFontPanelSelector
  , updateDragTypeRegistrationSelector
  , selectionRangeForProposedRange_granularitySelector
  , clickedOnLink_atIndexSelector
  , startSpeakingSelector
  , stopSpeakingSelector
  , setLayoutOrientationSelector
  , changeLayoutOrientationSelector
  , characterIndexForInsertionAtPointSelector
  , performValidatedReplacementInRange_withAttributedStringSelector
  , toggleBaseWritingDirectionSelector
  , drawTextHighlightBackgroundForTextRange_originSelector
  , highlightSelector
  , scrollableTextViewSelector
  , fieldEditorSelector
  , scrollableDocumentContentTextViewSelector
  , scrollablePlainDocumentContentTextViewSelector
  , toggleAutomaticTextCompletionSelector
  , updateTouchBarItemIdentifiersSelector
  , updateTextTouchBarItemsSelector
  , updateCandidatesSelector
  , orderFrontSharingServicePickerSelector
  , toggleQuickLookPreviewPanelSelector
  , quickLookPreviewableItemsInRangesSelector
  , updateQuickLookPreviewPanelSelector
  , smartDeleteRangeForProposedRangeSelector
  , toggleSmartInsertDeleteSelector
  , smartInsertForString_replacingRange_beforeString_afterStringSelector
  , smartInsertBeforeStringForString_replacingRangeSelector
  , smartInsertAfterStringForString_replacingRangeSelector
  , toggleAutomaticQuoteSubstitutionSelector
  , toggleAutomaticLinkDetectionSelector
  , toggleAutomaticDataDetectionSelector
  , toggleAutomaticDashSubstitutionSelector
  , toggleAutomaticTextReplacementSelector
  , toggleAutomaticSpellingCorrectionSelector
  , checkTextInRange_types_optionsSelector
  , handleTextCheckingResults_forRange_types_options_orthography_wordCountSelector
  , orderFrontSubstitutionsPanelSelector
  , checkTextInSelectionSelector
  , checkTextInDocumentSelector
  , setSelectedRanges_affinity_stillSelectingSelector
  , setSelectedRange_affinity_stillSelectingSelector
  , updateInsertionPointStateAndRestartTimerSelector
  , toggleContinuousSpellCheckingSelector
  , toggleGrammarCheckingSelector
  , setSpellingState_rangeSelector
  , shouldChangeTextInRanges_replacementStringsSelector
  , shouldChangeTextInRange_replacementStringSelector
  , didChangeTextSelector
  , breakUndoCoalescingSelector
  , showFindIndicatorForRangeSelector
  , setSelectedRangeSelector
  , dragSelectionWithEvent_offset_slideBackSelector
  , dragImageForSelectionWithEvent_originSelector
  , dragOperationForDraggingInfo_typeSelector
  , cleanUpAfterDragOperationSelector
  , writeSelectionToPasteboard_typeSelector
  , writeSelectionToPasteboard_typesSelector
  , preferredPasteboardTypeFromArray_restrictedToTypesFromArraySelector
  , readSelectionFromPasteboard_typeSelector
  , readSelectionFromPasteboardSelector
  , registerForServicesSelector
  , validRequestorForSendType_returnTypeSelector
  , pasteAsPlainTextSelector
  , pasteAsRichTextSelector
  , completeSelector
  , completionsForPartialWordRange_indexOfSelectedItemSelector
  , insertCompletion_forPartialWordRange_movement_isFinalSelector
  , textContainerSelector
  , setTextContainerSelector
  , textContainerInsetSelector
  , setTextContainerInsetSelector
  , textContainerOriginSelector
  , layoutManagerSelector
  , textStorageSelector
  , shouldDrawInsertionPointSelector
  , stronglyReferencesTextStorageSelector
  , usesAdaptiveColorMappingForDarkAppearanceSelector
  , setUsesAdaptiveColorMappingForDarkAppearanceSelector
  , automaticTextCompletionEnabledSelector
  , setAutomaticTextCompletionEnabledSelector
  , allowsCharacterPickerTouchBarItemSelector
  , setAllowsCharacterPickerTouchBarItemSelector
  , smartInsertDeleteEnabledSelector
  , setSmartInsertDeleteEnabledSelector
  , automaticQuoteSubstitutionEnabledSelector
  , setAutomaticQuoteSubstitutionEnabledSelector
  , automaticLinkDetectionEnabledSelector
  , setAutomaticLinkDetectionEnabledSelector
  , automaticDataDetectionEnabledSelector
  , setAutomaticDataDetectionEnabledSelector
  , automaticDashSubstitutionEnabledSelector
  , setAutomaticDashSubstitutionEnabledSelector
  , automaticTextReplacementEnabledSelector
  , setAutomaticTextReplacementEnabledSelector
  , automaticSpellingCorrectionEnabledSelector
  , setAutomaticSpellingCorrectionEnabledSelector
  , enabledTextCheckingTypesSelector
  , setEnabledTextCheckingTypesSelector
  , usesFindPanelSelector
  , setUsesFindPanelSelector
  , usesFindBarSelector
  , setUsesFindBarSelector
  , incrementalSearchingEnabledSelector
  , setIncrementalSearchingEnabledSelector
  , inlinePredictionTypeSelector
  , setInlinePredictionTypeSelector
  , mathExpressionCompletionTypeSelector
  , setMathExpressionCompletionTypeSelector
  , selectedRangesSelector
  , setSelectedRangesSelector
  , selectionAffinitySelector
  , selectionGranularitySelector
  , setSelectionGranularitySelector
  , selectedTextAttributesSelector
  , setSelectedTextAttributesSelector
  , insertionPointColorSelector
  , setInsertionPointColorSelector
  , markedTextAttributesSelector
  , setMarkedTextAttributesSelector
  , linkTextAttributesSelector
  , setLinkTextAttributesSelector
  , displaysLinkToolTipsSelector
  , setDisplaysLinkToolTipsSelector
  , acceptsGlyphInfoSelector
  , setAcceptsGlyphInfoSelector
  , usesRulerSelector
  , setUsesRulerSelector
  , usesInspectorBarSelector
  , setUsesInspectorBarSelector
  , continuousSpellCheckingEnabledSelector
  , setContinuousSpellCheckingEnabledSelector
  , spellCheckerDocumentTagSelector
  , grammarCheckingEnabledSelector
  , setGrammarCheckingEnabledSelector
  , typingAttributesSelector
  , setTypingAttributesSelector
  , rangesForUserTextChangeSelector
  , rangesForUserCharacterAttributeChangeSelector
  , rangesForUserParagraphAttributeChangeSelector
  , rangeForUserTextChangeSelector
  , rangeForUserCharacterAttributeChangeSelector
  , rangeForUserParagraphAttributeChangeSelector
  , allowsDocumentBackgroundColorChangeSelector
  , setAllowsDocumentBackgroundColorChangeSelector
  , defaultParagraphStyleSelector
  , setDefaultParagraphStyleSelector
  , allowsUndoSelector
  , setAllowsUndoSelector
  , coalescingUndoSelector
  , allowsImageEditingSelector
  , setAllowsImageEditingSelector
  , usesRolloverButtonForSelectionSelector
  , setUsesRolloverButtonForSelectionSelector
  , editableSelector
  , setEditableSelector
  , selectableSelector
  , setSelectableSelector
  , richTextSelector
  , setRichTextSelector
  , importsGraphicsSelector
  , setImportsGraphicsSelector
  , drawsBackgroundSelector
  , setDrawsBackgroundSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , setFieldEditorSelector
  , usesFontPanelSelector
  , setUsesFontPanelSelector
  , rulerVisibleSelector
  , setRulerVisibleSelector
  , writingToolsActiveSelector
  , writingToolsBehaviorSelector
  , setWritingToolsBehaviorSelector
  , allowedWritingToolsResultOptionsSelector
  , setAllowedWritingToolsResultOptionsSelector
  , acceptableDragTypesSelector
  , writablePasteboardTypesSelector
  , readablePasteboardTypesSelector
  , rangeForUserCompletionSelector

  -- * Enum types
  , NSDragOperation(NSDragOperation)
  , pattern NSDragOperationNone
  , pattern NSDragOperationCopy
  , pattern NSDragOperationLink
  , pattern NSDragOperationGeneric
  , pattern NSDragOperationPrivate
  , pattern NSDragOperationMove
  , pattern NSDragOperationDelete
  , pattern NSDragOperationEvery
  , pattern NSDragOperationAll_Obsolete
  , pattern NSDragOperationAll
  , NSSelectionAffinity(NSSelectionAffinity)
  , pattern NSSelectionAffinityUpstream
  , pattern NSSelectionAffinityDownstream
  , NSSelectionGranularity(NSSelectionGranularity)
  , pattern NSSelectByCharacter
  , pattern NSSelectByWord
  , pattern NSSelectByParagraph
  , NSTextAlignment(NSTextAlignment)
  , pattern NSTextAlignmentLeft
  , pattern NSTextAlignmentCenter
  , pattern NSTextAlignmentRight
  , pattern NSTextAlignmentJustified
  , pattern NSTextAlignmentNatural
  , NSTextInputTraitType(NSTextInputTraitType)
  , pattern NSTextInputTraitTypeDefault
  , pattern NSTextInputTraitTypeNo
  , pattern NSTextInputTraitTypeYes
  , NSTextLayoutOrientation(NSTextLayoutOrientation)
  , pattern NSTextLayoutOrientationHorizontal
  , pattern NSTextLayoutOrientationVertical
  , NSWritingDirection(NSWritingDirection)
  , pattern NSWritingDirectionNatural
  , pattern NSWritingDirectionLeftToRight
  , pattern NSWritingDirectionRightToLeft
  , NSWritingToolsBehavior(NSWritingToolsBehavior)
  , pattern NSWritingToolsBehaviorNone
  , pattern NSWritingToolsBehaviorDefault
  , pattern NSWritingToolsBehaviorComplete
  , pattern NSWritingToolsBehaviorLimited
  , NSWritingToolsResultOptions(NSWritingToolsResultOptions)
  , pattern NSWritingToolsResultDefault
  , pattern NSWritingToolsResultPlainText
  , pattern NSWritingToolsResultRichText
  , pattern NSWritingToolsResultList
  , pattern NSWritingToolsResultTable
  , pattern NSWritingToolsResultPresentationIntent

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
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | ************************** Initializing ***************************
--
-- ObjC selector: @- initWithFrame:textContainer:@
initWithFrame_textContainer :: (IsNSTextView nsTextView, IsNSTextContainer container) => nsTextView -> NSRect -> container -> IO (Id NSTextView)
initWithFrame_textContainer nsTextView  frameRect container =
withObjCPtr container $ \raw_container ->
    sendMsg nsTextView (mkSelector "initWithFrame:textContainer:") (retPtr retVoid) [argNSRect frameRect, argPtr (castPtr raw_container :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSTextView nsTextView, IsNSCoder coder) => nsTextView -> coder -> IO (Id NSTextView)
initWithCoder nsTextView  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsTextView (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithFrame:@
initWithFrame :: IsNSTextView nsTextView => nsTextView -> NSRect -> IO (Id NSTextView)
initWithFrame nsTextView  frameRect =
  sendMsg nsTextView (mkSelector "initWithFrame:") (retPtr retVoid) [argNSRect frameRect] >>= ownedObject . castPtr

-- | @- initUsingTextLayoutManager:@
initUsingTextLayoutManager :: IsNSTextView nsTextView => nsTextView -> Bool -> IO (Id NSTextView)
initUsingTextLayoutManager nsTextView  usingTextLayoutManager =
  sendMsg nsTextView (mkSelector "initUsingTextLayoutManager:") (retPtr retVoid) [argCULong (if usingTextLayoutManager then 1 else 0)] >>= ownedObject . castPtr

-- | @+ textViewUsingTextLayoutManager:@
textViewUsingTextLayoutManager :: Bool -> IO (Id NSTextView)
textViewUsingTextLayoutManager usingTextLayoutManager =
  do
    cls' <- getRequiredClass "NSTextView"
    sendClassMsg cls' (mkSelector "textViewUsingTextLayoutManager:") (retPtr retVoid) [argCULong (if usingTextLayoutManager then 1 else 0)] >>= retainedObject . castPtr

-- | @- replaceTextContainer:@
replaceTextContainer :: (IsNSTextView nsTextView, IsNSTextContainer newContainer) => nsTextView -> newContainer -> IO ()
replaceTextContainer nsTextView  newContainer =
withObjCPtr newContainer $ \raw_newContainer ->
    sendMsg nsTextView (mkSelector "replaceTextContainer:") retVoid [argPtr (castPtr raw_newContainer :: Ptr ())]

-- | @- invalidateTextContainerOrigin@
invalidateTextContainerOrigin :: IsNSTextView nsTextView => nsTextView -> IO ()
invalidateTextContainerOrigin nsTextView  =
  sendMsg nsTextView (mkSelector "invalidateTextContainerOrigin") retVoid []

-- | *********************** Key binding entry-point ************************
--
-- ObjC selector: @- insertText:@
insertText :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
insertText nsTextView  insertString =
  sendMsg nsTextView (mkSelector "insertText:") retVoid [argPtr (castPtr (unRawId insertString) :: Ptr ())]

-- | ************************* Sizing methods **************************
--
-- ObjC selector: @- setConstrainedFrameSize:@
setConstrainedFrameSize :: IsNSTextView nsTextView => nsTextView -> NSSize -> IO ()
setConstrainedFrameSize nsTextView  desiredSize =
  sendMsg nsTextView (mkSelector "setConstrainedFrameSize:") retVoid [argNSSize desiredSize]

-- | @- setAlignment:range:@
setAlignment_range :: IsNSTextView nsTextView => nsTextView -> NSTextAlignment -> NSRange -> IO ()
setAlignment_range nsTextView  alignment range =
  sendMsg nsTextView (mkSelector "setAlignment:range:") retVoid [argCLong (coerce alignment), argNSRange range]

-- | @- setBaseWritingDirection:range:@
setBaseWritingDirection_range :: IsNSTextView nsTextView => nsTextView -> NSWritingDirection -> NSRange -> IO ()
setBaseWritingDirection_range nsTextView  writingDirection range =
  sendMsg nsTextView (mkSelector "setBaseWritingDirection:range:") retVoid [argCLong (coerce writingDirection), argNSRange range]

-- | ************************* New Font menu commands **************************
--
-- ObjC selector: @- turnOffKerning:@
turnOffKerning :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
turnOffKerning nsTextView  sender =
  sendMsg nsTextView (mkSelector "turnOffKerning:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- tightenKerning:@
tightenKerning :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
tightenKerning nsTextView  sender =
  sendMsg nsTextView (mkSelector "tightenKerning:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- loosenKerning:@
loosenKerning :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
loosenKerning nsTextView  sender =
  sendMsg nsTextView (mkSelector "loosenKerning:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- useStandardKerning:@
useStandardKerning :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
useStandardKerning nsTextView  sender =
  sendMsg nsTextView (mkSelector "useStandardKerning:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- turnOffLigatures:@
turnOffLigatures :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
turnOffLigatures nsTextView  sender =
  sendMsg nsTextView (mkSelector "turnOffLigatures:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- useStandardLigatures:@
useStandardLigatures :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
useStandardLigatures nsTextView  sender =
  sendMsg nsTextView (mkSelector "useStandardLigatures:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- useAllLigatures:@
useAllLigatures :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
useAllLigatures nsTextView  sender =
  sendMsg nsTextView (mkSelector "useAllLigatures:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- raiseBaseline:@
raiseBaseline :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
raiseBaseline nsTextView  sender =
  sendMsg nsTextView (mkSelector "raiseBaseline:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- lowerBaseline:@
lowerBaseline :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
lowerBaseline nsTextView  sender =
  sendMsg nsTextView (mkSelector "lowerBaseline:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- toggleTraditionalCharacterShape:@
toggleTraditionalCharacterShape :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleTraditionalCharacterShape nsTextView  sender =
  sendMsg nsTextView (mkSelector "toggleTraditionalCharacterShape:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- outline:@
outline :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
outline nsTextView  sender =
  sendMsg nsTextView (mkSelector "outline:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | ************************* Find menu commands **************************
--
-- ObjC selector: @- performFindPanelAction:@
performFindPanelAction :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
performFindPanelAction nsTextView  sender =
  sendMsg nsTextView (mkSelector "performFindPanelAction:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | ************************* New Text commands **************************
--
-- ObjC selector: @- alignJustified:@
alignJustified :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
alignJustified nsTextView  sender =
  sendMsg nsTextView (mkSelector "alignJustified:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- changeColor:@
changeColor :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
changeColor nsTextView  sender =
  sendMsg nsTextView (mkSelector "changeColor:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- changeAttributes:@
changeAttributes :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
changeAttributes nsTextView  sender =
  sendMsg nsTextView (mkSelector "changeAttributes:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- changeDocumentBackgroundColor:@
changeDocumentBackgroundColor :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
changeDocumentBackgroundColor nsTextView  sender =
  sendMsg nsTextView (mkSelector "changeDocumentBackgroundColor:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- orderFrontSpacingPanel:@
orderFrontSpacingPanel :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
orderFrontSpacingPanel nsTextView  sender =
  sendMsg nsTextView (mkSelector "orderFrontSpacingPanel:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- orderFrontLinkPanel:@
orderFrontLinkPanel :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
orderFrontLinkPanel nsTextView  sender =
  sendMsg nsTextView (mkSelector "orderFrontLinkPanel:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- orderFrontListPanel:@
orderFrontListPanel :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
orderFrontListPanel nsTextView  sender =
  sendMsg nsTextView (mkSelector "orderFrontListPanel:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- orderFrontTablePanel:@
orderFrontTablePanel :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
orderFrontTablePanel nsTextView  sender =
  sendMsg nsTextView (mkSelector "orderFrontTablePanel:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | ************************* Ruler support **************************
--
-- ObjC selector: @- rulerView:didMoveMarker:@
rulerView_didMoveMarker :: (IsNSTextView nsTextView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsTextView -> ruler -> marker -> IO ()
rulerView_didMoveMarker nsTextView  ruler marker =
withObjCPtr ruler $ \raw_ruler ->
  withObjCPtr marker $ \raw_marker ->
      sendMsg nsTextView (mkSelector "rulerView:didMoveMarker:") retVoid [argPtr (castPtr raw_ruler :: Ptr ()), argPtr (castPtr raw_marker :: Ptr ())]

-- | @- rulerView:didRemoveMarker:@
rulerView_didRemoveMarker :: (IsNSTextView nsTextView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsTextView -> ruler -> marker -> IO ()
rulerView_didRemoveMarker nsTextView  ruler marker =
withObjCPtr ruler $ \raw_ruler ->
  withObjCPtr marker $ \raw_marker ->
      sendMsg nsTextView (mkSelector "rulerView:didRemoveMarker:") retVoid [argPtr (castPtr raw_ruler :: Ptr ()), argPtr (castPtr raw_marker :: Ptr ())]

-- | @- rulerView:didAddMarker:@
rulerView_didAddMarker :: (IsNSTextView nsTextView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsTextView -> ruler -> marker -> IO ()
rulerView_didAddMarker nsTextView  ruler marker =
withObjCPtr ruler $ \raw_ruler ->
  withObjCPtr marker $ \raw_marker ->
      sendMsg nsTextView (mkSelector "rulerView:didAddMarker:") retVoid [argPtr (castPtr raw_ruler :: Ptr ()), argPtr (castPtr raw_marker :: Ptr ())]

-- | @- rulerView:shouldMoveMarker:@
rulerView_shouldMoveMarker :: (IsNSTextView nsTextView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsTextView -> ruler -> marker -> IO Bool
rulerView_shouldMoveMarker nsTextView  ruler marker =
withObjCPtr ruler $ \raw_ruler ->
  withObjCPtr marker $ \raw_marker ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "rulerView:shouldMoveMarker:") retCULong [argPtr (castPtr raw_ruler :: Ptr ()), argPtr (castPtr raw_marker :: Ptr ())]

-- | @- rulerView:shouldAddMarker:@
rulerView_shouldAddMarker :: (IsNSTextView nsTextView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsTextView -> ruler -> marker -> IO Bool
rulerView_shouldAddMarker nsTextView  ruler marker =
withObjCPtr ruler $ \raw_ruler ->
  withObjCPtr marker $ \raw_marker ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "rulerView:shouldAddMarker:") retCULong [argPtr (castPtr raw_ruler :: Ptr ()), argPtr (castPtr raw_marker :: Ptr ())]

-- | @- rulerView:willMoveMarker:toLocation:@
rulerView_willMoveMarker_toLocation :: (IsNSTextView nsTextView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsTextView -> ruler -> marker -> CDouble -> IO CDouble
rulerView_willMoveMarker_toLocation nsTextView  ruler marker location =
withObjCPtr ruler $ \raw_ruler ->
  withObjCPtr marker $ \raw_marker ->
      sendMsg nsTextView (mkSelector "rulerView:willMoveMarker:toLocation:") retCDouble [argPtr (castPtr raw_ruler :: Ptr ()), argPtr (castPtr raw_marker :: Ptr ()), argCDouble (fromIntegral location)]

-- | @- rulerView:shouldRemoveMarker:@
rulerView_shouldRemoveMarker :: (IsNSTextView nsTextView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsTextView -> ruler -> marker -> IO Bool
rulerView_shouldRemoveMarker nsTextView  ruler marker =
withObjCPtr ruler $ \raw_ruler ->
  withObjCPtr marker $ \raw_marker ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "rulerView:shouldRemoveMarker:") retCULong [argPtr (castPtr raw_ruler :: Ptr ()), argPtr (castPtr raw_marker :: Ptr ())]

-- | @- rulerView:willAddMarker:atLocation:@
rulerView_willAddMarker_atLocation :: (IsNSTextView nsTextView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsTextView -> ruler -> marker -> CDouble -> IO CDouble
rulerView_willAddMarker_atLocation nsTextView  ruler marker location =
withObjCPtr ruler $ \raw_ruler ->
  withObjCPtr marker $ \raw_marker ->
      sendMsg nsTextView (mkSelector "rulerView:willAddMarker:atLocation:") retCDouble [argPtr (castPtr raw_ruler :: Ptr ()), argPtr (castPtr raw_marker :: Ptr ()), argCDouble (fromIntegral location)]

-- | @- rulerView:handleMouseDown:@
rulerView_handleMouseDown :: (IsNSTextView nsTextView, IsNSRulerView ruler, IsNSEvent event) => nsTextView -> ruler -> event -> IO ()
rulerView_handleMouseDown nsTextView  ruler event =
withObjCPtr ruler $ \raw_ruler ->
  withObjCPtr event $ \raw_event ->
      sendMsg nsTextView (mkSelector "rulerView:handleMouseDown:") retVoid [argPtr (castPtr raw_ruler :: Ptr ()), argPtr (castPtr raw_event :: Ptr ())]

-- | ************************* Fine display control **************************
--
-- ObjC selector: @- setNeedsDisplayInRect:avoidAdditionalLayout:@
setNeedsDisplayInRect_avoidAdditionalLayout :: IsNSTextView nsTextView => nsTextView -> NSRect -> Bool -> IO ()
setNeedsDisplayInRect_avoidAdditionalLayout nsTextView  rect flag =
  sendMsg nsTextView (mkSelector "setNeedsDisplayInRect:avoidAdditionalLayout:") retVoid [argNSRect rect, argCULong (if flag then 1 else 0)]

-- | @- drawInsertionPointInRect:color:turnedOn:@
drawInsertionPointInRect_color_turnedOn :: (IsNSTextView nsTextView, IsNSColor color) => nsTextView -> NSRect -> color -> Bool -> IO ()
drawInsertionPointInRect_color_turnedOn nsTextView  rect color flag =
withObjCPtr color $ \raw_color ->
    sendMsg nsTextView (mkSelector "drawInsertionPointInRect:color:turnedOn:") retVoid [argNSRect rect, argPtr (castPtr raw_color :: Ptr ()), argCULong (if flag then 1 else 0)]

-- | @- drawViewBackgroundInRect:@
drawViewBackgroundInRect :: IsNSTextView nsTextView => nsTextView -> NSRect -> IO ()
drawViewBackgroundInRect nsTextView  rect =
  sendMsg nsTextView (mkSelector "drawViewBackgroundInRect:") retVoid [argNSRect rect]

-- | ************************* Especially for subclassers **************************
--
-- ObjC selector: @- updateRuler@
updateRuler :: IsNSTextView nsTextView => nsTextView -> IO ()
updateRuler nsTextView  =
  sendMsg nsTextView (mkSelector "updateRuler") retVoid []

-- | @- updateFontPanel@
updateFontPanel :: IsNSTextView nsTextView => nsTextView -> IO ()
updateFontPanel nsTextView  =
  sendMsg nsTextView (mkSelector "updateFontPanel") retVoid []

-- | @- updateDragTypeRegistration@
updateDragTypeRegistration :: IsNSTextView nsTextView => nsTextView -> IO ()
updateDragTypeRegistration nsTextView  =
  sendMsg nsTextView (mkSelector "updateDragTypeRegistration") retVoid []

-- | @- selectionRangeForProposedRange:granularity:@
selectionRangeForProposedRange_granularity :: IsNSTextView nsTextView => nsTextView -> NSRange -> NSSelectionGranularity -> IO NSRange
selectionRangeForProposedRange_granularity nsTextView  proposedCharRange granularity =
  sendMsgStret nsTextView (mkSelector "selectionRangeForProposedRange:granularity:") retNSRange [argNSRange proposedCharRange, argCULong (coerce granularity)]

-- | ************************* Especially for subclassers **************************
--
-- ObjC selector: @- clickedOnLink:atIndex:@
clickedOnLink_atIndex :: IsNSTextView nsTextView => nsTextView -> RawId -> CULong -> IO ()
clickedOnLink_atIndex nsTextView  link charIndex =
  sendMsg nsTextView (mkSelector "clickedOnLink:atIndex:") retVoid [argPtr (castPtr (unRawId link) :: Ptr ()), argCULong (fromIntegral charIndex)]

-- | *********************** Speech support ************************
--
-- ObjC selector: @- startSpeaking:@
startSpeaking :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
startSpeaking nsTextView  sender =
  sendMsg nsTextView (mkSelector "startSpeaking:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- stopSpeaking:@
stopSpeaking :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
stopSpeaking nsTextView  sender =
  sendMsg nsTextView (mkSelector "stopSpeaking:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- setLayoutOrientation:@
setLayoutOrientation :: IsNSTextView nsTextView => nsTextView -> NSTextLayoutOrientation -> IO ()
setLayoutOrientation nsTextView  orientation =
  sendMsg nsTextView (mkSelector "setLayoutOrientation:") retVoid [argCLong (coerce orientation)]

-- | @- changeLayoutOrientation:@
changeLayoutOrientation :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
changeLayoutOrientation nsTextView  sender =
  sendMsg nsTextView (mkSelector "changeLayoutOrientation:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | *********************** Helper for subclassers ************************
--
-- ObjC selector: @- characterIndexForInsertionAtPoint:@
characterIndexForInsertionAtPoint :: IsNSTextView nsTextView => nsTextView -> NSPoint -> IO CULong
characterIndexForInsertionAtPoint nsTextView  point =
  sendMsg nsTextView (mkSelector "characterIndexForInsertionAtPoint:") retCULong [argNSPoint point]

-- | @- performValidatedReplacementInRange:withAttributedString:@
performValidatedReplacementInRange_withAttributedString :: (IsNSTextView nsTextView, IsNSAttributedString attributedString) => nsTextView -> NSRange -> attributedString -> IO Bool
performValidatedReplacementInRange_withAttributedString nsTextView  range attributedString =
withObjCPtr attributedString $ \raw_attributedString ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "performValidatedReplacementInRange:withAttributedString:") retCULong [argNSRange range, argPtr (castPtr raw_attributedString :: Ptr ())]

-- | @- toggleBaseWritingDirection:@
toggleBaseWritingDirection :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleBaseWritingDirection nsTextView  sender =
  sendMsg nsTextView (mkSelector "toggleBaseWritingDirection:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- drawTextHighlightBackgroundForTextRange:origin:@
drawTextHighlightBackgroundForTextRange_origin :: (IsNSTextView nsTextView, IsNSTextRange textRange) => nsTextView -> textRange -> NSPoint -> IO ()
drawTextHighlightBackgroundForTextRange_origin nsTextView  textRange origin =
withObjCPtr textRange $ \raw_textRange ->
    sendMsg nsTextView (mkSelector "drawTextHighlightBackgroundForTextRange:origin:") retVoid [argPtr (castPtr raw_textRange :: Ptr ()), argNSPoint origin]

-- | An action for toggling @NSTextHighlightStyleAttributeName@ in the receiver’s selected range. The sender should be a menu item with a @representedObject@ of type (@NSTextHighlightColorScheme@).
--
-- ObjC selector: @- highlight:@
highlight :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
highlight nsTextView  sender =
  sendMsg nsTextView (mkSelector "highlight:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @+ scrollableTextView@
scrollableTextView :: IO (Id NSScrollView)
scrollableTextView  =
  do
    cls' <- getRequiredClass "NSTextView"
    sendClassMsg cls' (mkSelector "scrollableTextView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ fieldEditor@
nsTextViewFieldEditor :: IO (Id NSTextView)
nsTextViewFieldEditor  =
  do
    cls' <- getRequiredClass "NSTextView"
    sendClassMsg cls' (mkSelector "fieldEditor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ scrollableDocumentContentTextView@
scrollableDocumentContentTextView :: IO (Id NSScrollView)
scrollableDocumentContentTextView  =
  do
    cls' <- getRequiredClass "NSTextView"
    sendClassMsg cls' (mkSelector "scrollableDocumentContentTextView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ scrollablePlainDocumentContentTextView@
scrollablePlainDocumentContentTextView :: IO (Id NSScrollView)
scrollablePlainDocumentContentTextView  =
  do
    cls' <- getRequiredClass "NSTextView"
    sendClassMsg cls' (mkSelector "scrollablePlainDocumentContentTextView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- toggleAutomaticTextCompletion:@
toggleAutomaticTextCompletion :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleAutomaticTextCompletion nsTextView  sender =
  sendMsg nsTextView (mkSelector "toggleAutomaticTextCompletion:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- updateTouchBarItemIdentifiers@
updateTouchBarItemIdentifiers :: IsNSTextView nsTextView => nsTextView -> IO ()
updateTouchBarItemIdentifiers nsTextView  =
  sendMsg nsTextView (mkSelector "updateTouchBarItemIdentifiers") retVoid []

-- | @- updateTextTouchBarItems@
updateTextTouchBarItems :: IsNSTextView nsTextView => nsTextView -> IO ()
updateTextTouchBarItems nsTextView  =
  sendMsg nsTextView (mkSelector "updateTextTouchBarItems") retVoid []

-- | @- updateCandidates@
updateCandidates :: IsNSTextView nsTextView => nsTextView -> IO ()
updateCandidates nsTextView  =
  sendMsg nsTextView (mkSelector "updateCandidates") retVoid []

-- | ************************* NSSharingService support **************************
--
-- ObjC selector: @- orderFrontSharingServicePicker:@
orderFrontSharingServicePicker :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
orderFrontSharingServicePicker nsTextView  sender =
  sendMsg nsTextView (mkSelector "orderFrontSharingServicePicker:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | ************************* Quick Look support **************************
--
-- ObjC selector: @- toggleQuickLookPreviewPanel:@
toggleQuickLookPreviewPanel :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleQuickLookPreviewPanel nsTextView  sender =
  sendMsg nsTextView (mkSelector "toggleQuickLookPreviewPanel:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- quickLookPreviewableItemsInRanges:@
quickLookPreviewableItemsInRanges :: (IsNSTextView nsTextView, IsNSArray ranges) => nsTextView -> ranges -> IO (Id NSArray)
quickLookPreviewableItemsInRanges nsTextView  ranges =
withObjCPtr ranges $ \raw_ranges ->
    sendMsg nsTextView (mkSelector "quickLookPreviewableItemsInRanges:") (retPtr retVoid) [argPtr (castPtr raw_ranges :: Ptr ())] >>= retainedObject . castPtr

-- | @- updateQuickLookPreviewPanel@
updateQuickLookPreviewPanel :: IsNSTextView nsTextView => nsTextView -> IO ()
updateQuickLookPreviewPanel nsTextView  =
  sendMsg nsTextView (mkSelector "updateQuickLookPreviewPanel") retVoid []

-- | @- smartDeleteRangeForProposedRange:@
smartDeleteRangeForProposedRange :: IsNSTextView nsTextView => nsTextView -> NSRange -> IO NSRange
smartDeleteRangeForProposedRange nsTextView  proposedCharRange =
  sendMsgStret nsTextView (mkSelector "smartDeleteRangeForProposedRange:") retNSRange [argNSRange proposedCharRange]

-- | @- toggleSmartInsertDelete:@
toggleSmartInsertDelete :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleSmartInsertDelete nsTextView  sender =
  sendMsg nsTextView (mkSelector "toggleSmartInsertDelete:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- smartInsertForString:replacingRange:beforeString:afterString:@
smartInsertForString_replacingRange_beforeString_afterString :: (IsNSTextView nsTextView, IsNSString pasteString, IsNSString beforeString, IsNSString afterString) => nsTextView -> pasteString -> NSRange -> beforeString -> afterString -> IO ()
smartInsertForString_replacingRange_beforeString_afterString nsTextView  pasteString charRangeToReplace beforeString afterString =
withObjCPtr pasteString $ \raw_pasteString ->
  withObjCPtr beforeString $ \raw_beforeString ->
    withObjCPtr afterString $ \raw_afterString ->
        sendMsg nsTextView (mkSelector "smartInsertForString:replacingRange:beforeString:afterString:") retVoid [argPtr (castPtr raw_pasteString :: Ptr ()), argNSRange charRangeToReplace, argPtr (castPtr raw_beforeString :: Ptr ()), argPtr (castPtr raw_afterString :: Ptr ())]

-- | @- smartInsertBeforeStringForString:replacingRange:@
smartInsertBeforeStringForString_replacingRange :: (IsNSTextView nsTextView, IsNSString pasteString) => nsTextView -> pasteString -> NSRange -> IO (Id NSString)
smartInsertBeforeStringForString_replacingRange nsTextView  pasteString charRangeToReplace =
withObjCPtr pasteString $ \raw_pasteString ->
    sendMsg nsTextView (mkSelector "smartInsertBeforeStringForString:replacingRange:") (retPtr retVoid) [argPtr (castPtr raw_pasteString :: Ptr ()), argNSRange charRangeToReplace] >>= retainedObject . castPtr

-- | @- smartInsertAfterStringForString:replacingRange:@
smartInsertAfterStringForString_replacingRange :: (IsNSTextView nsTextView, IsNSString pasteString) => nsTextView -> pasteString -> NSRange -> IO (Id NSString)
smartInsertAfterStringForString_replacingRange nsTextView  pasteString charRangeToReplace =
withObjCPtr pasteString $ \raw_pasteString ->
    sendMsg nsTextView (mkSelector "smartInsertAfterStringForString:replacingRange:") (retPtr retVoid) [argPtr (castPtr raw_pasteString :: Ptr ()), argNSRange charRangeToReplace] >>= retainedObject . castPtr

-- | @- toggleAutomaticQuoteSubstitution:@
toggleAutomaticQuoteSubstitution :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleAutomaticQuoteSubstitution nsTextView  sender =
  sendMsg nsTextView (mkSelector "toggleAutomaticQuoteSubstitution:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- toggleAutomaticLinkDetection:@
toggleAutomaticLinkDetection :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleAutomaticLinkDetection nsTextView  sender =
  sendMsg nsTextView (mkSelector "toggleAutomaticLinkDetection:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- toggleAutomaticDataDetection:@
toggleAutomaticDataDetection :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleAutomaticDataDetection nsTextView  sender =
  sendMsg nsTextView (mkSelector "toggleAutomaticDataDetection:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- toggleAutomaticDashSubstitution:@
toggleAutomaticDashSubstitution :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleAutomaticDashSubstitution nsTextView  sender =
  sendMsg nsTextView (mkSelector "toggleAutomaticDashSubstitution:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- toggleAutomaticTextReplacement:@
toggleAutomaticTextReplacement :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleAutomaticTextReplacement nsTextView  sender =
  sendMsg nsTextView (mkSelector "toggleAutomaticTextReplacement:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- toggleAutomaticSpellingCorrection:@
toggleAutomaticSpellingCorrection :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleAutomaticSpellingCorrection nsTextView  sender =
  sendMsg nsTextView (mkSelector "toggleAutomaticSpellingCorrection:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- checkTextInRange:types:options:@
checkTextInRange_types_options :: (IsNSTextView nsTextView, IsNSDictionary options) => nsTextView -> NSRange -> CULong -> options -> IO ()
checkTextInRange_types_options nsTextView  range checkingTypes options =
withObjCPtr options $ \raw_options ->
    sendMsg nsTextView (mkSelector "checkTextInRange:types:options:") retVoid [argNSRange range, argCULong (fromIntegral checkingTypes), argPtr (castPtr raw_options :: Ptr ())]

-- | @- handleTextCheckingResults:forRange:types:options:orthography:wordCount:@
handleTextCheckingResults_forRange_types_options_orthography_wordCount :: (IsNSTextView nsTextView, IsNSArray results, IsNSDictionary options, IsNSOrthography orthography) => nsTextView -> results -> NSRange -> CULong -> options -> orthography -> CLong -> IO ()
handleTextCheckingResults_forRange_types_options_orthography_wordCount nsTextView  results range checkingTypes options orthography wordCount =
withObjCPtr results $ \raw_results ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr orthography $ \raw_orthography ->
        sendMsg nsTextView (mkSelector "handleTextCheckingResults:forRange:types:options:orthography:wordCount:") retVoid [argPtr (castPtr raw_results :: Ptr ()), argNSRange range, argCULong (fromIntegral checkingTypes), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_orthography :: Ptr ()), argCLong (fromIntegral wordCount)]

-- | @- orderFrontSubstitutionsPanel:@
orderFrontSubstitutionsPanel :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
orderFrontSubstitutionsPanel nsTextView  sender =
  sendMsg nsTextView (mkSelector "orderFrontSubstitutionsPanel:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- checkTextInSelection:@
checkTextInSelection :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
checkTextInSelection nsTextView  sender =
  sendMsg nsTextView (mkSelector "checkTextInSelection:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- checkTextInDocument:@
checkTextInDocument :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
checkTextInDocument nsTextView  sender =
  sendMsg nsTextView (mkSelector "checkTextInDocument:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- setSelectedRanges:affinity:stillSelecting:@
setSelectedRanges_affinity_stillSelecting :: (IsNSTextView nsTextView, IsNSArray ranges) => nsTextView -> ranges -> NSSelectionAffinity -> Bool -> IO ()
setSelectedRanges_affinity_stillSelecting nsTextView  ranges affinity stillSelectingFlag =
withObjCPtr ranges $ \raw_ranges ->
    sendMsg nsTextView (mkSelector "setSelectedRanges:affinity:stillSelecting:") retVoid [argPtr (castPtr raw_ranges :: Ptr ()), argCULong (coerce affinity), argCULong (if stillSelectingFlag then 1 else 0)]

-- | @- setSelectedRange:affinity:stillSelecting:@
setSelectedRange_affinity_stillSelecting :: IsNSTextView nsTextView => nsTextView -> NSRange -> NSSelectionAffinity -> Bool -> IO ()
setSelectedRange_affinity_stillSelecting nsTextView  charRange affinity stillSelectingFlag =
  sendMsg nsTextView (mkSelector "setSelectedRange:affinity:stillSelecting:") retVoid [argNSRange charRange, argCULong (coerce affinity), argCULong (if stillSelectingFlag then 1 else 0)]

-- | @- updateInsertionPointStateAndRestartTimer:@
updateInsertionPointStateAndRestartTimer :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
updateInsertionPointStateAndRestartTimer nsTextView  restartFlag =
  sendMsg nsTextView (mkSelector "updateInsertionPointStateAndRestartTimer:") retVoid [argCULong (if restartFlag then 1 else 0)]

-- | @- toggleContinuousSpellChecking:@
toggleContinuousSpellChecking :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleContinuousSpellChecking nsTextView  sender =
  sendMsg nsTextView (mkSelector "toggleContinuousSpellChecking:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- toggleGrammarChecking:@
toggleGrammarChecking :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleGrammarChecking nsTextView  sender =
  sendMsg nsTextView (mkSelector "toggleGrammarChecking:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- setSpellingState:range:@
setSpellingState_range :: IsNSTextView nsTextView => nsTextView -> CLong -> NSRange -> IO ()
setSpellingState_range nsTextView  value charRange =
  sendMsg nsTextView (mkSelector "setSpellingState:range:") retVoid [argCLong (fromIntegral value), argNSRange charRange]

-- | @- shouldChangeTextInRanges:replacementStrings:@
shouldChangeTextInRanges_replacementStrings :: (IsNSTextView nsTextView, IsNSArray affectedRanges, IsNSArray replacementStrings) => nsTextView -> affectedRanges -> replacementStrings -> IO Bool
shouldChangeTextInRanges_replacementStrings nsTextView  affectedRanges replacementStrings =
withObjCPtr affectedRanges $ \raw_affectedRanges ->
  withObjCPtr replacementStrings $ \raw_replacementStrings ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "shouldChangeTextInRanges:replacementStrings:") retCULong [argPtr (castPtr raw_affectedRanges :: Ptr ()), argPtr (castPtr raw_replacementStrings :: Ptr ())]

-- | @- shouldChangeTextInRange:replacementString:@
shouldChangeTextInRange_replacementString :: (IsNSTextView nsTextView, IsNSString replacementString) => nsTextView -> NSRange -> replacementString -> IO Bool
shouldChangeTextInRange_replacementString nsTextView  affectedCharRange replacementString =
withObjCPtr replacementString $ \raw_replacementString ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "shouldChangeTextInRange:replacementString:") retCULong [argNSRange affectedCharRange, argPtr (castPtr raw_replacementString :: Ptr ())]

-- | @- didChangeText@
didChangeText :: IsNSTextView nsTextView => nsTextView -> IO ()
didChangeText nsTextView  =
  sendMsg nsTextView (mkSelector "didChangeText") retVoid []

-- | @- breakUndoCoalescing@
breakUndoCoalescing :: IsNSTextView nsTextView => nsTextView -> IO ()
breakUndoCoalescing nsTextView  =
  sendMsg nsTextView (mkSelector "breakUndoCoalescing") retVoid []

-- | @- showFindIndicatorForRange:@
showFindIndicatorForRange :: IsNSTextView nsTextView => nsTextView -> NSRange -> IO ()
showFindIndicatorForRange nsTextView  charRange =
  sendMsg nsTextView (mkSelector "showFindIndicatorForRange:") retVoid [argNSRange charRange]

-- | @- setSelectedRange:@
setSelectedRange :: IsNSTextView nsTextView => nsTextView -> NSRange -> IO ()
setSelectedRange nsTextView  charRange =
  sendMsg nsTextView (mkSelector "setSelectedRange:") retVoid [argNSRange charRange]

-- | @- dragSelectionWithEvent:offset:slideBack:@
dragSelectionWithEvent_offset_slideBack :: (IsNSTextView nsTextView, IsNSEvent event) => nsTextView -> event -> NSSize -> Bool -> IO Bool
dragSelectionWithEvent_offset_slideBack nsTextView  event mouseOffset slideBack =
withObjCPtr event $ \raw_event ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "dragSelectionWithEvent:offset:slideBack:") retCULong [argPtr (castPtr raw_event :: Ptr ()), argNSSize mouseOffset, argCULong (if slideBack then 1 else 0)]

-- | @- dragImageForSelectionWithEvent:origin:@
dragImageForSelectionWithEvent_origin :: (IsNSTextView nsTextView, IsNSEvent event) => nsTextView -> event -> Ptr NSPoint -> IO (Id NSImage)
dragImageForSelectionWithEvent_origin nsTextView  event origin =
withObjCPtr event $ \raw_event ->
    sendMsg nsTextView (mkSelector "dragImageForSelectionWithEvent:origin:") (retPtr retVoid) [argPtr (castPtr raw_event :: Ptr ()), argPtr origin] >>= retainedObject . castPtr

-- | @- dragOperationForDraggingInfo:type:@
dragOperationForDraggingInfo_type :: (IsNSTextView nsTextView, IsNSString type_) => nsTextView -> RawId -> type_ -> IO NSDragOperation
dragOperationForDraggingInfo_type nsTextView  dragInfo type_ =
withObjCPtr type_ $ \raw_type_ ->
    fmap (coerce :: CULong -> NSDragOperation) $ sendMsg nsTextView (mkSelector "dragOperationForDraggingInfo:type:") retCULong [argPtr (castPtr (unRawId dragInfo) :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- cleanUpAfterDragOperation@
cleanUpAfterDragOperation :: IsNSTextView nsTextView => nsTextView -> IO ()
cleanUpAfterDragOperation nsTextView  =
  sendMsg nsTextView (mkSelector "cleanUpAfterDragOperation") retVoid []

-- | @- writeSelectionToPasteboard:type:@
writeSelectionToPasteboard_type :: (IsNSTextView nsTextView, IsNSPasteboard pboard, IsNSString type_) => nsTextView -> pboard -> type_ -> IO Bool
writeSelectionToPasteboard_type nsTextView  pboard type_ =
withObjCPtr pboard $ \raw_pboard ->
  withObjCPtr type_ $ \raw_type_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "writeSelectionToPasteboard:type:") retCULong [argPtr (castPtr raw_pboard :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- writeSelectionToPasteboard:types:@
writeSelectionToPasteboard_types :: (IsNSTextView nsTextView, IsNSPasteboard pboard, IsNSArray types) => nsTextView -> pboard -> types -> IO Bool
writeSelectionToPasteboard_types nsTextView  pboard types =
withObjCPtr pboard $ \raw_pboard ->
  withObjCPtr types $ \raw_types ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "writeSelectionToPasteboard:types:") retCULong [argPtr (castPtr raw_pboard :: Ptr ()), argPtr (castPtr raw_types :: Ptr ())]

-- | @- preferredPasteboardTypeFromArray:restrictedToTypesFromArray:@
preferredPasteboardTypeFromArray_restrictedToTypesFromArray :: (IsNSTextView nsTextView, IsNSArray availableTypes, IsNSArray allowedTypes) => nsTextView -> availableTypes -> allowedTypes -> IO (Id NSString)
preferredPasteboardTypeFromArray_restrictedToTypesFromArray nsTextView  availableTypes allowedTypes =
withObjCPtr availableTypes $ \raw_availableTypes ->
  withObjCPtr allowedTypes $ \raw_allowedTypes ->
      sendMsg nsTextView (mkSelector "preferredPasteboardTypeFromArray:restrictedToTypesFromArray:") (retPtr retVoid) [argPtr (castPtr raw_availableTypes :: Ptr ()), argPtr (castPtr raw_allowedTypes :: Ptr ())] >>= retainedObject . castPtr

-- | @- readSelectionFromPasteboard:type:@
readSelectionFromPasteboard_type :: (IsNSTextView nsTextView, IsNSPasteboard pboard, IsNSString type_) => nsTextView -> pboard -> type_ -> IO Bool
readSelectionFromPasteboard_type nsTextView  pboard type_ =
withObjCPtr pboard $ \raw_pboard ->
  withObjCPtr type_ $ \raw_type_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "readSelectionFromPasteboard:type:") retCULong [argPtr (castPtr raw_pboard :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- readSelectionFromPasteboard:@
readSelectionFromPasteboard :: (IsNSTextView nsTextView, IsNSPasteboard pboard) => nsTextView -> pboard -> IO Bool
readSelectionFromPasteboard nsTextView  pboard =
withObjCPtr pboard $ \raw_pboard ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "readSelectionFromPasteboard:") retCULong [argPtr (castPtr raw_pboard :: Ptr ())]

-- | @+ registerForServices@
registerForServices :: IO ()
registerForServices  =
  do
    cls' <- getRequiredClass "NSTextView"
    sendClassMsg cls' (mkSelector "registerForServices") retVoid []

-- | @- validRequestorForSendType:returnType:@
validRequestorForSendType_returnType :: (IsNSTextView nsTextView, IsNSString sendType, IsNSString returnType) => nsTextView -> sendType -> returnType -> IO RawId
validRequestorForSendType_returnType nsTextView  sendType returnType =
withObjCPtr sendType $ \raw_sendType ->
  withObjCPtr returnType $ \raw_returnType ->
      fmap (RawId . castPtr) $ sendMsg nsTextView (mkSelector "validRequestorForSendType:returnType:") (retPtr retVoid) [argPtr (castPtr raw_sendType :: Ptr ()), argPtr (castPtr raw_returnType :: Ptr ())]

-- | @- pasteAsPlainText:@
pasteAsPlainText :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
pasteAsPlainText nsTextView  sender =
  sendMsg nsTextView (mkSelector "pasteAsPlainText:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- pasteAsRichText:@
pasteAsRichText :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
pasteAsRichText nsTextView  sender =
  sendMsg nsTextView (mkSelector "pasteAsRichText:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | *********************** Completion support ********************
--
-- ObjC selector: @- complete:@
complete :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
complete nsTextView  sender =
  sendMsg nsTextView (mkSelector "complete:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- completionsForPartialWordRange:indexOfSelectedItem:@
completionsForPartialWordRange_indexOfSelectedItem :: IsNSTextView nsTextView => nsTextView -> NSRange -> Ptr CLong -> IO (Id NSArray)
completionsForPartialWordRange_indexOfSelectedItem nsTextView  charRange index =
  sendMsg nsTextView (mkSelector "completionsForPartialWordRange:indexOfSelectedItem:") (retPtr retVoid) [argNSRange charRange, argPtr index] >>= retainedObject . castPtr

-- | @- insertCompletion:forPartialWordRange:movement:isFinal:@
insertCompletion_forPartialWordRange_movement_isFinal :: (IsNSTextView nsTextView, IsNSString word) => nsTextView -> word -> NSRange -> CLong -> Bool -> IO ()
insertCompletion_forPartialWordRange_movement_isFinal nsTextView  word charRange movement flag =
withObjCPtr word $ \raw_word ->
    sendMsg nsTextView (mkSelector "insertCompletion:forPartialWordRange:movement:isFinal:") retVoid [argPtr (castPtr raw_word :: Ptr ()), argNSRange charRange, argCLong (fromIntegral movement), argCULong (if flag then 1 else 0)]

-- | *************** Get/Set the container and other stuff ****************
--
-- ObjC selector: @- textContainer@
textContainer :: IsNSTextView nsTextView => nsTextView -> IO (Id NSTextContainer)
textContainer nsTextView  =
  sendMsg nsTextView (mkSelector "textContainer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | *************** Get/Set the container and other stuff ****************
--
-- ObjC selector: @- setTextContainer:@
setTextContainer :: (IsNSTextView nsTextView, IsNSTextContainer value) => nsTextView -> value -> IO ()
setTextContainer nsTextView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextView (mkSelector "setTextContainer:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- textContainerInset@
textContainerInset :: IsNSTextView nsTextView => nsTextView -> IO NSSize
textContainerInset nsTextView  =
  sendMsgStret nsTextView (mkSelector "textContainerInset") retNSSize []

-- | @- setTextContainerInset:@
setTextContainerInset :: IsNSTextView nsTextView => nsTextView -> NSSize -> IO ()
setTextContainerInset nsTextView  value =
  sendMsg nsTextView (mkSelector "setTextContainerInset:") retVoid [argNSSize value]

-- | @- textContainerOrigin@
textContainerOrigin :: IsNSTextView nsTextView => nsTextView -> IO NSPoint
textContainerOrigin nsTextView  =
  sendMsgStret nsTextView (mkSelector "textContainerOrigin") retNSPoint []

-- | @- layoutManager@
layoutManager :: IsNSTextView nsTextView => nsTextView -> IO (Id NSLayoutManager)
layoutManager nsTextView  =
  sendMsg nsTextView (mkSelector "layoutManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- textStorage@
textStorage :: IsNSTextView nsTextView => nsTextView -> IO (Id NSTextStorage)
textStorage nsTextView  =
  sendMsg nsTextView (mkSelector "textStorage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- shouldDrawInsertionPoint@
shouldDrawInsertionPoint :: IsNSTextView nsTextView => nsTextView -> IO Bool
shouldDrawInsertionPoint nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "shouldDrawInsertionPoint") retCULong []

-- | ************************** Ownership policy ***************************
--
-- ObjC selector: @+ stronglyReferencesTextStorage@
stronglyReferencesTextStorage :: IO Bool
stronglyReferencesTextStorage  =
  do
    cls' <- getRequiredClass "NSTextView"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "stronglyReferencesTextStorage") retCULong []

-- | @- usesAdaptiveColorMappingForDarkAppearance@
usesAdaptiveColorMappingForDarkAppearance :: IsNSTextView nsTextView => nsTextView -> IO Bool
usesAdaptiveColorMappingForDarkAppearance nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "usesAdaptiveColorMappingForDarkAppearance") retCULong []

-- | @- setUsesAdaptiveColorMappingForDarkAppearance:@
setUsesAdaptiveColorMappingForDarkAppearance :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setUsesAdaptiveColorMappingForDarkAppearance nsTextView  value =
  sendMsg nsTextView (mkSelector "setUsesAdaptiveColorMappingForDarkAppearance:") retVoid [argCULong (if value then 1 else 0)]

-- | @- automaticTextCompletionEnabled@
automaticTextCompletionEnabled :: IsNSTextView nsTextView => nsTextView -> IO Bool
automaticTextCompletionEnabled nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "automaticTextCompletionEnabled") retCULong []

-- | @- setAutomaticTextCompletionEnabled:@
setAutomaticTextCompletionEnabled :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAutomaticTextCompletionEnabled nsTextView  value =
  sendMsg nsTextView (mkSelector "setAutomaticTextCompletionEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsCharacterPickerTouchBarItem@
allowsCharacterPickerTouchBarItem :: IsNSTextView nsTextView => nsTextView -> IO Bool
allowsCharacterPickerTouchBarItem nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "allowsCharacterPickerTouchBarItem") retCULong []

-- | @- setAllowsCharacterPickerTouchBarItem:@
setAllowsCharacterPickerTouchBarItem :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAllowsCharacterPickerTouchBarItem nsTextView  value =
  sendMsg nsTextView (mkSelector "setAllowsCharacterPickerTouchBarItem:") retVoid [argCULong (if value then 1 else 0)]

-- | ************************* Smart copy/paste/delete/substitution support **************************
--
-- ObjC selector: @- smartInsertDeleteEnabled@
smartInsertDeleteEnabled :: IsNSTextView nsTextView => nsTextView -> IO Bool
smartInsertDeleteEnabled nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "smartInsertDeleteEnabled") retCULong []

-- | ************************* Smart copy/paste/delete/substitution support **************************
--
-- ObjC selector: @- setSmartInsertDeleteEnabled:@
setSmartInsertDeleteEnabled :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setSmartInsertDeleteEnabled nsTextView  value =
  sendMsg nsTextView (mkSelector "setSmartInsertDeleteEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- automaticQuoteSubstitutionEnabled@
automaticQuoteSubstitutionEnabled :: IsNSTextView nsTextView => nsTextView -> IO Bool
automaticQuoteSubstitutionEnabled nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "automaticQuoteSubstitutionEnabled") retCULong []

-- | @- setAutomaticQuoteSubstitutionEnabled:@
setAutomaticQuoteSubstitutionEnabled :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAutomaticQuoteSubstitutionEnabled nsTextView  value =
  sendMsg nsTextView (mkSelector "setAutomaticQuoteSubstitutionEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- automaticLinkDetectionEnabled@
automaticLinkDetectionEnabled :: IsNSTextView nsTextView => nsTextView -> IO Bool
automaticLinkDetectionEnabled nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "automaticLinkDetectionEnabled") retCULong []

-- | @- setAutomaticLinkDetectionEnabled:@
setAutomaticLinkDetectionEnabled :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAutomaticLinkDetectionEnabled nsTextView  value =
  sendMsg nsTextView (mkSelector "setAutomaticLinkDetectionEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- automaticDataDetectionEnabled@
automaticDataDetectionEnabled :: IsNSTextView nsTextView => nsTextView -> IO Bool
automaticDataDetectionEnabled nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "automaticDataDetectionEnabled") retCULong []

-- | @- setAutomaticDataDetectionEnabled:@
setAutomaticDataDetectionEnabled :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAutomaticDataDetectionEnabled nsTextView  value =
  sendMsg nsTextView (mkSelector "setAutomaticDataDetectionEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- automaticDashSubstitutionEnabled@
automaticDashSubstitutionEnabled :: IsNSTextView nsTextView => nsTextView -> IO Bool
automaticDashSubstitutionEnabled nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "automaticDashSubstitutionEnabled") retCULong []

-- | @- setAutomaticDashSubstitutionEnabled:@
setAutomaticDashSubstitutionEnabled :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAutomaticDashSubstitutionEnabled nsTextView  value =
  sendMsg nsTextView (mkSelector "setAutomaticDashSubstitutionEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- automaticTextReplacementEnabled@
automaticTextReplacementEnabled :: IsNSTextView nsTextView => nsTextView -> IO Bool
automaticTextReplacementEnabled nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "automaticTextReplacementEnabled") retCULong []

-- | @- setAutomaticTextReplacementEnabled:@
setAutomaticTextReplacementEnabled :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAutomaticTextReplacementEnabled nsTextView  value =
  sendMsg nsTextView (mkSelector "setAutomaticTextReplacementEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- automaticSpellingCorrectionEnabled@
automaticSpellingCorrectionEnabled :: IsNSTextView nsTextView => nsTextView -> IO Bool
automaticSpellingCorrectionEnabled nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "automaticSpellingCorrectionEnabled") retCULong []

-- | @- setAutomaticSpellingCorrectionEnabled:@
setAutomaticSpellingCorrectionEnabled :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAutomaticSpellingCorrectionEnabled nsTextView  value =
  sendMsg nsTextView (mkSelector "setAutomaticSpellingCorrectionEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- enabledTextCheckingTypes@
enabledTextCheckingTypes :: IsNSTextView nsTextView => nsTextView -> IO CULong
enabledTextCheckingTypes nsTextView  =
  sendMsg nsTextView (mkSelector "enabledTextCheckingTypes") retCULong []

-- | @- setEnabledTextCheckingTypes:@
setEnabledTextCheckingTypes :: IsNSTextView nsTextView => nsTextView -> CULong -> IO ()
setEnabledTextCheckingTypes nsTextView  value =
  sendMsg nsTextView (mkSelector "setEnabledTextCheckingTypes:") retVoid [argCULong (fromIntegral value)]

-- | @- usesFindPanel@
usesFindPanel :: IsNSTextView nsTextView => nsTextView -> IO Bool
usesFindPanel nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "usesFindPanel") retCULong []

-- | @- setUsesFindPanel:@
setUsesFindPanel :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setUsesFindPanel nsTextView  value =
  sendMsg nsTextView (mkSelector "setUsesFindPanel:") retVoid [argCULong (if value then 1 else 0)]

-- | @- usesFindBar@
usesFindBar :: IsNSTextView nsTextView => nsTextView -> IO Bool
usesFindBar nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "usesFindBar") retCULong []

-- | @- setUsesFindBar:@
setUsesFindBar :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setUsesFindBar nsTextView  value =
  sendMsg nsTextView (mkSelector "setUsesFindBar:") retVoid [argCULong (if value then 1 else 0)]

-- | @- incrementalSearchingEnabled@
incrementalSearchingEnabled :: IsNSTextView nsTextView => nsTextView -> IO Bool
incrementalSearchingEnabled nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "incrementalSearchingEnabled") retCULong []

-- | @- setIncrementalSearchingEnabled:@
setIncrementalSearchingEnabled :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setIncrementalSearchingEnabled nsTextView  value =
  sendMsg nsTextView (mkSelector "setIncrementalSearchingEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- inlinePredictionType@
inlinePredictionType :: IsNSTextView nsTextView => nsTextView -> IO NSTextInputTraitType
inlinePredictionType nsTextView  =
  fmap (coerce :: CLong -> NSTextInputTraitType) $ sendMsg nsTextView (mkSelector "inlinePredictionType") retCLong []

-- | @- setInlinePredictionType:@
setInlinePredictionType :: IsNSTextView nsTextView => nsTextView -> NSTextInputTraitType -> IO ()
setInlinePredictionType nsTextView  value =
  sendMsg nsTextView (mkSelector "setInlinePredictionType:") retVoid [argCLong (coerce value)]

-- | @- mathExpressionCompletionType@
mathExpressionCompletionType :: IsNSTextView nsTextView => nsTextView -> IO NSTextInputTraitType
mathExpressionCompletionType nsTextView  =
  fmap (coerce :: CLong -> NSTextInputTraitType) $ sendMsg nsTextView (mkSelector "mathExpressionCompletionType") retCLong []

-- | @- setMathExpressionCompletionType:@
setMathExpressionCompletionType :: IsNSTextView nsTextView => nsTextView -> NSTextInputTraitType -> IO ()
setMathExpressionCompletionType nsTextView  value =
  sendMsg nsTextView (mkSelector "setMathExpressionCompletionType:") retVoid [argCLong (coerce value)]

-- | ************************* Selected/Marked range **************************
--
-- ObjC selector: @- selectedRanges@
selectedRanges :: IsNSTextView nsTextView => nsTextView -> IO (Id NSArray)
selectedRanges nsTextView  =
  sendMsg nsTextView (mkSelector "selectedRanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | ************************* Selected/Marked range **************************
--
-- ObjC selector: @- setSelectedRanges:@
setSelectedRanges :: (IsNSTextView nsTextView, IsNSArray value) => nsTextView -> value -> IO ()
setSelectedRanges nsTextView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextView (mkSelector "setSelectedRanges:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- selectionAffinity@
selectionAffinity :: IsNSTextView nsTextView => nsTextView -> IO NSSelectionAffinity
selectionAffinity nsTextView  =
  fmap (coerce :: CULong -> NSSelectionAffinity) $ sendMsg nsTextView (mkSelector "selectionAffinity") retCULong []

-- | @- selectionGranularity@
selectionGranularity :: IsNSTextView nsTextView => nsTextView -> IO NSSelectionGranularity
selectionGranularity nsTextView  =
  fmap (coerce :: CULong -> NSSelectionGranularity) $ sendMsg nsTextView (mkSelector "selectionGranularity") retCULong []

-- | @- setSelectionGranularity:@
setSelectionGranularity :: IsNSTextView nsTextView => nsTextView -> NSSelectionGranularity -> IO ()
setSelectionGranularity nsTextView  value =
  sendMsg nsTextView (mkSelector "setSelectionGranularity:") retVoid [argCULong (coerce value)]

-- | @- selectedTextAttributes@
selectedTextAttributes :: IsNSTextView nsTextView => nsTextView -> IO (Id NSDictionary)
selectedTextAttributes nsTextView  =
  sendMsg nsTextView (mkSelector "selectedTextAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSelectedTextAttributes:@
setSelectedTextAttributes :: (IsNSTextView nsTextView, IsNSDictionary value) => nsTextView -> value -> IO ()
setSelectedTextAttributes nsTextView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextView (mkSelector "setSelectedTextAttributes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- insertionPointColor@
insertionPointColor :: IsNSTextView nsTextView => nsTextView -> IO (Id NSColor)
insertionPointColor nsTextView  =
  sendMsg nsTextView (mkSelector "insertionPointColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInsertionPointColor:@
setInsertionPointColor :: (IsNSTextView nsTextView, IsNSColor value) => nsTextView -> value -> IO ()
setInsertionPointColor nsTextView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextView (mkSelector "setInsertionPointColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- markedTextAttributes@
markedTextAttributes :: IsNSTextView nsTextView => nsTextView -> IO (Id NSDictionary)
markedTextAttributes nsTextView  =
  sendMsg nsTextView (mkSelector "markedTextAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMarkedTextAttributes:@
setMarkedTextAttributes :: (IsNSTextView nsTextView, IsNSDictionary value) => nsTextView -> value -> IO ()
setMarkedTextAttributes nsTextView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextView (mkSelector "setMarkedTextAttributes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- linkTextAttributes@
linkTextAttributes :: IsNSTextView nsTextView => nsTextView -> IO (Id NSDictionary)
linkTextAttributes nsTextView  =
  sendMsg nsTextView (mkSelector "linkTextAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLinkTextAttributes:@
setLinkTextAttributes :: (IsNSTextView nsTextView, IsNSDictionary value) => nsTextView -> value -> IO ()
setLinkTextAttributes nsTextView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextView (mkSelector "setLinkTextAttributes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- displaysLinkToolTips@
displaysLinkToolTips :: IsNSTextView nsTextView => nsTextView -> IO Bool
displaysLinkToolTips nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "displaysLinkToolTips") retCULong []

-- | @- setDisplaysLinkToolTips:@
setDisplaysLinkToolTips :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setDisplaysLinkToolTips nsTextView  value =
  sendMsg nsTextView (mkSelector "setDisplaysLinkToolTips:") retVoid [argCULong (if value then 1 else 0)]

-- | *********************** Glyph info support ************************
--
-- ObjC selector: @- acceptsGlyphInfo@
acceptsGlyphInfo :: IsNSTextView nsTextView => nsTextView -> IO Bool
acceptsGlyphInfo nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "acceptsGlyphInfo") retCULong []

-- | *********************** Glyph info support ************************
--
-- ObjC selector: @- setAcceptsGlyphInfo:@
setAcceptsGlyphInfo :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAcceptsGlyphInfo nsTextView  value =
  sendMsg nsTextView (mkSelector "setAcceptsGlyphInfo:") retVoid [argCULong (if value then 1 else 0)]

-- | ************************* Other NSTextView methods **************************
--
-- ObjC selector: @- usesRuler@
usesRuler :: IsNSTextView nsTextView => nsTextView -> IO Bool
usesRuler nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "usesRuler") retCULong []

-- | ************************* Other NSTextView methods **************************
--
-- ObjC selector: @- setUsesRuler:@
setUsesRuler :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setUsesRuler nsTextView  value =
  sendMsg nsTextView (mkSelector "setUsesRuler:") retVoid [argCULong (if value then 1 else 0)]

-- | @- usesInspectorBar@
usesInspectorBar :: IsNSTextView nsTextView => nsTextView -> IO Bool
usesInspectorBar nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "usesInspectorBar") retCULong []

-- | @- setUsesInspectorBar:@
setUsesInspectorBar :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setUsesInspectorBar nsTextView  value =
  sendMsg nsTextView (mkSelector "setUsesInspectorBar:") retVoid [argCULong (if value then 1 else 0)]

-- | @- continuousSpellCheckingEnabled@
continuousSpellCheckingEnabled :: IsNSTextView nsTextView => nsTextView -> IO Bool
continuousSpellCheckingEnabled nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "continuousSpellCheckingEnabled") retCULong []

-- | @- setContinuousSpellCheckingEnabled:@
setContinuousSpellCheckingEnabled :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setContinuousSpellCheckingEnabled nsTextView  value =
  sendMsg nsTextView (mkSelector "setContinuousSpellCheckingEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- spellCheckerDocumentTag@
spellCheckerDocumentTag :: IsNSTextView nsTextView => nsTextView -> IO CLong
spellCheckerDocumentTag nsTextView  =
  sendMsg nsTextView (mkSelector "spellCheckerDocumentTag") retCLong []

-- | @- grammarCheckingEnabled@
grammarCheckingEnabled :: IsNSTextView nsTextView => nsTextView -> IO Bool
grammarCheckingEnabled nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "grammarCheckingEnabled") retCULong []

-- | @- setGrammarCheckingEnabled:@
setGrammarCheckingEnabled :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setGrammarCheckingEnabled nsTextView  value =
  sendMsg nsTextView (mkSelector "setGrammarCheckingEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- typingAttributes@
typingAttributes :: IsNSTextView nsTextView => nsTextView -> IO (Id NSDictionary)
typingAttributes nsTextView  =
  sendMsg nsTextView (mkSelector "typingAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTypingAttributes:@
setTypingAttributes :: (IsNSTextView nsTextView, IsNSDictionary value) => nsTextView -> value -> IO ()
setTypingAttributes nsTextView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextView (mkSelector "setTypingAttributes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rangesForUserTextChange@
rangesForUserTextChange :: IsNSTextView nsTextView => nsTextView -> IO (Id NSArray)
rangesForUserTextChange nsTextView  =
  sendMsg nsTextView (mkSelector "rangesForUserTextChange") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rangesForUserCharacterAttributeChange@
rangesForUserCharacterAttributeChange :: IsNSTextView nsTextView => nsTextView -> IO (Id NSArray)
rangesForUserCharacterAttributeChange nsTextView  =
  sendMsg nsTextView (mkSelector "rangesForUserCharacterAttributeChange") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rangesForUserParagraphAttributeChange@
rangesForUserParagraphAttributeChange :: IsNSTextView nsTextView => nsTextView -> IO (Id NSArray)
rangesForUserParagraphAttributeChange nsTextView  =
  sendMsg nsTextView (mkSelector "rangesForUserParagraphAttributeChange") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rangeForUserTextChange@
rangeForUserTextChange :: IsNSTextView nsTextView => nsTextView -> IO NSRange
rangeForUserTextChange nsTextView  =
  sendMsgStret nsTextView (mkSelector "rangeForUserTextChange") retNSRange []

-- | @- rangeForUserCharacterAttributeChange@
rangeForUserCharacterAttributeChange :: IsNSTextView nsTextView => nsTextView -> IO NSRange
rangeForUserCharacterAttributeChange nsTextView  =
  sendMsgStret nsTextView (mkSelector "rangeForUserCharacterAttributeChange") retNSRange []

-- | @- rangeForUserParagraphAttributeChange@
rangeForUserParagraphAttributeChange :: IsNSTextView nsTextView => nsTextView -> IO NSRange
rangeForUserParagraphAttributeChange nsTextView  =
  sendMsgStret nsTextView (mkSelector "rangeForUserParagraphAttributeChange") retNSRange []

-- | @- allowsDocumentBackgroundColorChange@
allowsDocumentBackgroundColorChange :: IsNSTextView nsTextView => nsTextView -> IO Bool
allowsDocumentBackgroundColorChange nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "allowsDocumentBackgroundColorChange") retCULong []

-- | @- setAllowsDocumentBackgroundColorChange:@
setAllowsDocumentBackgroundColorChange :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAllowsDocumentBackgroundColorChange nsTextView  value =
  sendMsg nsTextView (mkSelector "setAllowsDocumentBackgroundColorChange:") retVoid [argCULong (if value then 1 else 0)]

-- | @- defaultParagraphStyle@
defaultParagraphStyle :: IsNSTextView nsTextView => nsTextView -> IO (Id NSParagraphStyle)
defaultParagraphStyle nsTextView  =
  sendMsg nsTextView (mkSelector "defaultParagraphStyle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDefaultParagraphStyle:@
setDefaultParagraphStyle :: (IsNSTextView nsTextView, IsNSParagraphStyle value) => nsTextView -> value -> IO ()
setDefaultParagraphStyle nsTextView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextView (mkSelector "setDefaultParagraphStyle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- allowsUndo@
allowsUndo :: IsNSTextView nsTextView => nsTextView -> IO Bool
allowsUndo nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "allowsUndo") retCULong []

-- | @- setAllowsUndo:@
setAllowsUndo :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAllowsUndo nsTextView  value =
  sendMsg nsTextView (mkSelector "setAllowsUndo:") retVoid [argCULong (if value then 1 else 0)]

-- | @- coalescingUndo@
coalescingUndo :: IsNSTextView nsTextView => nsTextView -> IO Bool
coalescingUndo nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "coalescingUndo") retCULong []

-- | @- allowsImageEditing@
allowsImageEditing :: IsNSTextView nsTextView => nsTextView -> IO Bool
allowsImageEditing nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "allowsImageEditing") retCULong []

-- | @- setAllowsImageEditing:@
setAllowsImageEditing :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAllowsImageEditing nsTextView  value =
  sendMsg nsTextView (mkSelector "setAllowsImageEditing:") retVoid [argCULong (if value then 1 else 0)]

-- | @- usesRolloverButtonForSelection@
usesRolloverButtonForSelection :: IsNSTextView nsTextView => nsTextView -> IO Bool
usesRolloverButtonForSelection nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "usesRolloverButtonForSelection") retCULong []

-- | @- setUsesRolloverButtonForSelection:@
setUsesRolloverButtonForSelection :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setUsesRolloverButtonForSelection nsTextView  value =
  sendMsg nsTextView (mkSelector "setUsesRolloverButtonForSelection:") retVoid [argCULong (if value then 1 else 0)]

-- | @- editable@
editable :: IsNSTextView nsTextView => nsTextView -> IO Bool
editable nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "editable") retCULong []

-- | @- setEditable:@
setEditable :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setEditable nsTextView  value =
  sendMsg nsTextView (mkSelector "setEditable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- selectable@
selectable :: IsNSTextView nsTextView => nsTextView -> IO Bool
selectable nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "selectable") retCULong []

-- | @- setSelectable:@
setSelectable :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setSelectable nsTextView  value =
  sendMsg nsTextView (mkSelector "setSelectable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- richText@
richText :: IsNSTextView nsTextView => nsTextView -> IO Bool
richText nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "richText") retCULong []

-- | @- setRichText:@
setRichText :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setRichText nsTextView  value =
  sendMsg nsTextView (mkSelector "setRichText:") retVoid [argCULong (if value then 1 else 0)]

-- | @- importsGraphics@
importsGraphics :: IsNSTextView nsTextView => nsTextView -> IO Bool
importsGraphics nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "importsGraphics") retCULong []

-- | @- setImportsGraphics:@
setImportsGraphics :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setImportsGraphics nsTextView  value =
  sendMsg nsTextView (mkSelector "setImportsGraphics:") retVoid [argCULong (if value then 1 else 0)]

-- | @- drawsBackground@
drawsBackground :: IsNSTextView nsTextView => nsTextView -> IO Bool
drawsBackground nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "drawsBackground") retCULong []

-- | @- setDrawsBackground:@
setDrawsBackground :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setDrawsBackground nsTextView  value =
  sendMsg nsTextView (mkSelector "setDrawsBackground:") retVoid [argCULong (if value then 1 else 0)]

-- | @- backgroundColor@
backgroundColor :: IsNSTextView nsTextView => nsTextView -> IO (Id NSColor)
backgroundColor nsTextView  =
  sendMsg nsTextView (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSTextView nsTextView, IsNSColor value) => nsTextView -> value -> IO ()
setBackgroundColor nsTextView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextView (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fieldEditor@
fieldEditor :: IsNSTextView nsTextView => nsTextView -> IO Bool
fieldEditor nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "fieldEditor") retCULong []

-- | @- setFieldEditor:@
setFieldEditor :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setFieldEditor nsTextView  value =
  sendMsg nsTextView (mkSelector "setFieldEditor:") retVoid [argCULong (if value then 1 else 0)]

-- | @- usesFontPanel@
usesFontPanel :: IsNSTextView nsTextView => nsTextView -> IO Bool
usesFontPanel nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "usesFontPanel") retCULong []

-- | @- setUsesFontPanel:@
setUsesFontPanel :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setUsesFontPanel nsTextView  value =
  sendMsg nsTextView (mkSelector "setUsesFontPanel:") retVoid [argCULong (if value then 1 else 0)]

-- | @- rulerVisible@
rulerVisible :: IsNSTextView nsTextView => nsTextView -> IO Bool
rulerVisible nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "rulerVisible") retCULong []

-- | @- setRulerVisible:@
setRulerVisible :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setRulerVisible nsTextView  value =
  sendMsg nsTextView (mkSelector "setRulerVisible:") retVoid [argCULong (if value then 1 else 0)]

-- | @- writingToolsActive@
writingToolsActive :: IsNSTextView nsTextView => nsTextView -> IO Bool
writingToolsActive nsTextView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextView (mkSelector "writingToolsActive") retCULong []

-- | @- writingToolsBehavior@
writingToolsBehavior :: IsNSTextView nsTextView => nsTextView -> IO NSWritingToolsBehavior
writingToolsBehavior nsTextView  =
  fmap (coerce :: CLong -> NSWritingToolsBehavior) $ sendMsg nsTextView (mkSelector "writingToolsBehavior") retCLong []

-- | @- setWritingToolsBehavior:@
setWritingToolsBehavior :: IsNSTextView nsTextView => nsTextView -> NSWritingToolsBehavior -> IO ()
setWritingToolsBehavior nsTextView  value =
  sendMsg nsTextView (mkSelector "setWritingToolsBehavior:") retVoid [argCLong (coerce value)]

-- | @- allowedWritingToolsResultOptions@
allowedWritingToolsResultOptions :: IsNSTextView nsTextView => nsTextView -> IO NSWritingToolsResultOptions
allowedWritingToolsResultOptions nsTextView  =
  fmap (coerce :: CULong -> NSWritingToolsResultOptions) $ sendMsg nsTextView (mkSelector "allowedWritingToolsResultOptions") retCULong []

-- | @- setAllowedWritingToolsResultOptions:@
setAllowedWritingToolsResultOptions :: IsNSTextView nsTextView => nsTextView -> NSWritingToolsResultOptions -> IO ()
setAllowedWritingToolsResultOptions nsTextView  value =
  sendMsg nsTextView (mkSelector "setAllowedWritingToolsResultOptions:") retVoid [argCULong (coerce value)]

-- | @- acceptableDragTypes@
acceptableDragTypes :: IsNSTextView nsTextView => nsTextView -> IO (Id NSArray)
acceptableDragTypes nsTextView  =
  sendMsg nsTextView (mkSelector "acceptableDragTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | ***************** Pasteboard support (mainly for subclassers) ******************
--
-- ObjC selector: @- writablePasteboardTypes@
writablePasteboardTypes :: IsNSTextView nsTextView => nsTextView -> IO (Id NSArray)
writablePasteboardTypes nsTextView  =
  sendMsg nsTextView (mkSelector "writablePasteboardTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- readablePasteboardTypes@
readablePasteboardTypes :: IsNSTextView nsTextView => nsTextView -> IO (Id NSArray)
readablePasteboardTypes nsTextView  =
  sendMsg nsTextView (mkSelector "readablePasteboardTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rangeForUserCompletion@
rangeForUserCompletion :: IsNSTextView nsTextView => nsTextView -> IO NSRange
rangeForUserCompletion nsTextView  =
  sendMsgStret nsTextView (mkSelector "rangeForUserCompletion") retNSRange []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:textContainer:@
initWithFrame_textContainerSelector :: Selector
initWithFrame_textContainerSelector = mkSelector "initWithFrame:textContainer:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @initUsingTextLayoutManager:@
initUsingTextLayoutManagerSelector :: Selector
initUsingTextLayoutManagerSelector = mkSelector "initUsingTextLayoutManager:"

-- | @Selector@ for @textViewUsingTextLayoutManager:@
textViewUsingTextLayoutManagerSelector :: Selector
textViewUsingTextLayoutManagerSelector = mkSelector "textViewUsingTextLayoutManager:"

-- | @Selector@ for @replaceTextContainer:@
replaceTextContainerSelector :: Selector
replaceTextContainerSelector = mkSelector "replaceTextContainer:"

-- | @Selector@ for @invalidateTextContainerOrigin@
invalidateTextContainerOriginSelector :: Selector
invalidateTextContainerOriginSelector = mkSelector "invalidateTextContainerOrigin"

-- | @Selector@ for @insertText:@
insertTextSelector :: Selector
insertTextSelector = mkSelector "insertText:"

-- | @Selector@ for @setConstrainedFrameSize:@
setConstrainedFrameSizeSelector :: Selector
setConstrainedFrameSizeSelector = mkSelector "setConstrainedFrameSize:"

-- | @Selector@ for @setAlignment:range:@
setAlignment_rangeSelector :: Selector
setAlignment_rangeSelector = mkSelector "setAlignment:range:"

-- | @Selector@ for @setBaseWritingDirection:range:@
setBaseWritingDirection_rangeSelector :: Selector
setBaseWritingDirection_rangeSelector = mkSelector "setBaseWritingDirection:range:"

-- | @Selector@ for @turnOffKerning:@
turnOffKerningSelector :: Selector
turnOffKerningSelector = mkSelector "turnOffKerning:"

-- | @Selector@ for @tightenKerning:@
tightenKerningSelector :: Selector
tightenKerningSelector = mkSelector "tightenKerning:"

-- | @Selector@ for @loosenKerning:@
loosenKerningSelector :: Selector
loosenKerningSelector = mkSelector "loosenKerning:"

-- | @Selector@ for @useStandardKerning:@
useStandardKerningSelector :: Selector
useStandardKerningSelector = mkSelector "useStandardKerning:"

-- | @Selector@ for @turnOffLigatures:@
turnOffLigaturesSelector :: Selector
turnOffLigaturesSelector = mkSelector "turnOffLigatures:"

-- | @Selector@ for @useStandardLigatures:@
useStandardLigaturesSelector :: Selector
useStandardLigaturesSelector = mkSelector "useStandardLigatures:"

-- | @Selector@ for @useAllLigatures:@
useAllLigaturesSelector :: Selector
useAllLigaturesSelector = mkSelector "useAllLigatures:"

-- | @Selector@ for @raiseBaseline:@
raiseBaselineSelector :: Selector
raiseBaselineSelector = mkSelector "raiseBaseline:"

-- | @Selector@ for @lowerBaseline:@
lowerBaselineSelector :: Selector
lowerBaselineSelector = mkSelector "lowerBaseline:"

-- | @Selector@ for @toggleTraditionalCharacterShape:@
toggleTraditionalCharacterShapeSelector :: Selector
toggleTraditionalCharacterShapeSelector = mkSelector "toggleTraditionalCharacterShape:"

-- | @Selector@ for @outline:@
outlineSelector :: Selector
outlineSelector = mkSelector "outline:"

-- | @Selector@ for @performFindPanelAction:@
performFindPanelActionSelector :: Selector
performFindPanelActionSelector = mkSelector "performFindPanelAction:"

-- | @Selector@ for @alignJustified:@
alignJustifiedSelector :: Selector
alignJustifiedSelector = mkSelector "alignJustified:"

-- | @Selector@ for @changeColor:@
changeColorSelector :: Selector
changeColorSelector = mkSelector "changeColor:"

-- | @Selector@ for @changeAttributes:@
changeAttributesSelector :: Selector
changeAttributesSelector = mkSelector "changeAttributes:"

-- | @Selector@ for @changeDocumentBackgroundColor:@
changeDocumentBackgroundColorSelector :: Selector
changeDocumentBackgroundColorSelector = mkSelector "changeDocumentBackgroundColor:"

-- | @Selector@ for @orderFrontSpacingPanel:@
orderFrontSpacingPanelSelector :: Selector
orderFrontSpacingPanelSelector = mkSelector "orderFrontSpacingPanel:"

-- | @Selector@ for @orderFrontLinkPanel:@
orderFrontLinkPanelSelector :: Selector
orderFrontLinkPanelSelector = mkSelector "orderFrontLinkPanel:"

-- | @Selector@ for @orderFrontListPanel:@
orderFrontListPanelSelector :: Selector
orderFrontListPanelSelector = mkSelector "orderFrontListPanel:"

-- | @Selector@ for @orderFrontTablePanel:@
orderFrontTablePanelSelector :: Selector
orderFrontTablePanelSelector = mkSelector "orderFrontTablePanel:"

-- | @Selector@ for @rulerView:didMoveMarker:@
rulerView_didMoveMarkerSelector :: Selector
rulerView_didMoveMarkerSelector = mkSelector "rulerView:didMoveMarker:"

-- | @Selector@ for @rulerView:didRemoveMarker:@
rulerView_didRemoveMarkerSelector :: Selector
rulerView_didRemoveMarkerSelector = mkSelector "rulerView:didRemoveMarker:"

-- | @Selector@ for @rulerView:didAddMarker:@
rulerView_didAddMarkerSelector :: Selector
rulerView_didAddMarkerSelector = mkSelector "rulerView:didAddMarker:"

-- | @Selector@ for @rulerView:shouldMoveMarker:@
rulerView_shouldMoveMarkerSelector :: Selector
rulerView_shouldMoveMarkerSelector = mkSelector "rulerView:shouldMoveMarker:"

-- | @Selector@ for @rulerView:shouldAddMarker:@
rulerView_shouldAddMarkerSelector :: Selector
rulerView_shouldAddMarkerSelector = mkSelector "rulerView:shouldAddMarker:"

-- | @Selector@ for @rulerView:willMoveMarker:toLocation:@
rulerView_willMoveMarker_toLocationSelector :: Selector
rulerView_willMoveMarker_toLocationSelector = mkSelector "rulerView:willMoveMarker:toLocation:"

-- | @Selector@ for @rulerView:shouldRemoveMarker:@
rulerView_shouldRemoveMarkerSelector :: Selector
rulerView_shouldRemoveMarkerSelector = mkSelector "rulerView:shouldRemoveMarker:"

-- | @Selector@ for @rulerView:willAddMarker:atLocation:@
rulerView_willAddMarker_atLocationSelector :: Selector
rulerView_willAddMarker_atLocationSelector = mkSelector "rulerView:willAddMarker:atLocation:"

-- | @Selector@ for @rulerView:handleMouseDown:@
rulerView_handleMouseDownSelector :: Selector
rulerView_handleMouseDownSelector = mkSelector "rulerView:handleMouseDown:"

-- | @Selector@ for @setNeedsDisplayInRect:avoidAdditionalLayout:@
setNeedsDisplayInRect_avoidAdditionalLayoutSelector :: Selector
setNeedsDisplayInRect_avoidAdditionalLayoutSelector = mkSelector "setNeedsDisplayInRect:avoidAdditionalLayout:"

-- | @Selector@ for @drawInsertionPointInRect:color:turnedOn:@
drawInsertionPointInRect_color_turnedOnSelector :: Selector
drawInsertionPointInRect_color_turnedOnSelector = mkSelector "drawInsertionPointInRect:color:turnedOn:"

-- | @Selector@ for @drawViewBackgroundInRect:@
drawViewBackgroundInRectSelector :: Selector
drawViewBackgroundInRectSelector = mkSelector "drawViewBackgroundInRect:"

-- | @Selector@ for @updateRuler@
updateRulerSelector :: Selector
updateRulerSelector = mkSelector "updateRuler"

-- | @Selector@ for @updateFontPanel@
updateFontPanelSelector :: Selector
updateFontPanelSelector = mkSelector "updateFontPanel"

-- | @Selector@ for @updateDragTypeRegistration@
updateDragTypeRegistrationSelector :: Selector
updateDragTypeRegistrationSelector = mkSelector "updateDragTypeRegistration"

-- | @Selector@ for @selectionRangeForProposedRange:granularity:@
selectionRangeForProposedRange_granularitySelector :: Selector
selectionRangeForProposedRange_granularitySelector = mkSelector "selectionRangeForProposedRange:granularity:"

-- | @Selector@ for @clickedOnLink:atIndex:@
clickedOnLink_atIndexSelector :: Selector
clickedOnLink_atIndexSelector = mkSelector "clickedOnLink:atIndex:"

-- | @Selector@ for @startSpeaking:@
startSpeakingSelector :: Selector
startSpeakingSelector = mkSelector "startSpeaking:"

-- | @Selector@ for @stopSpeaking:@
stopSpeakingSelector :: Selector
stopSpeakingSelector = mkSelector "stopSpeaking:"

-- | @Selector@ for @setLayoutOrientation:@
setLayoutOrientationSelector :: Selector
setLayoutOrientationSelector = mkSelector "setLayoutOrientation:"

-- | @Selector@ for @changeLayoutOrientation:@
changeLayoutOrientationSelector :: Selector
changeLayoutOrientationSelector = mkSelector "changeLayoutOrientation:"

-- | @Selector@ for @characterIndexForInsertionAtPoint:@
characterIndexForInsertionAtPointSelector :: Selector
characterIndexForInsertionAtPointSelector = mkSelector "characterIndexForInsertionAtPoint:"

-- | @Selector@ for @performValidatedReplacementInRange:withAttributedString:@
performValidatedReplacementInRange_withAttributedStringSelector :: Selector
performValidatedReplacementInRange_withAttributedStringSelector = mkSelector "performValidatedReplacementInRange:withAttributedString:"

-- | @Selector@ for @toggleBaseWritingDirection:@
toggleBaseWritingDirectionSelector :: Selector
toggleBaseWritingDirectionSelector = mkSelector "toggleBaseWritingDirection:"

-- | @Selector@ for @drawTextHighlightBackgroundForTextRange:origin:@
drawTextHighlightBackgroundForTextRange_originSelector :: Selector
drawTextHighlightBackgroundForTextRange_originSelector = mkSelector "drawTextHighlightBackgroundForTextRange:origin:"

-- | @Selector@ for @highlight:@
highlightSelector :: Selector
highlightSelector = mkSelector "highlight:"

-- | @Selector@ for @scrollableTextView@
scrollableTextViewSelector :: Selector
scrollableTextViewSelector = mkSelector "scrollableTextView"

-- | @Selector@ for @fieldEditor@
fieldEditorSelector :: Selector
fieldEditorSelector = mkSelector "fieldEditor"

-- | @Selector@ for @scrollableDocumentContentTextView@
scrollableDocumentContentTextViewSelector :: Selector
scrollableDocumentContentTextViewSelector = mkSelector "scrollableDocumentContentTextView"

-- | @Selector@ for @scrollablePlainDocumentContentTextView@
scrollablePlainDocumentContentTextViewSelector :: Selector
scrollablePlainDocumentContentTextViewSelector = mkSelector "scrollablePlainDocumentContentTextView"

-- | @Selector@ for @toggleAutomaticTextCompletion:@
toggleAutomaticTextCompletionSelector :: Selector
toggleAutomaticTextCompletionSelector = mkSelector "toggleAutomaticTextCompletion:"

-- | @Selector@ for @updateTouchBarItemIdentifiers@
updateTouchBarItemIdentifiersSelector :: Selector
updateTouchBarItemIdentifiersSelector = mkSelector "updateTouchBarItemIdentifiers"

-- | @Selector@ for @updateTextTouchBarItems@
updateTextTouchBarItemsSelector :: Selector
updateTextTouchBarItemsSelector = mkSelector "updateTextTouchBarItems"

-- | @Selector@ for @updateCandidates@
updateCandidatesSelector :: Selector
updateCandidatesSelector = mkSelector "updateCandidates"

-- | @Selector@ for @orderFrontSharingServicePicker:@
orderFrontSharingServicePickerSelector :: Selector
orderFrontSharingServicePickerSelector = mkSelector "orderFrontSharingServicePicker:"

-- | @Selector@ for @toggleQuickLookPreviewPanel:@
toggleQuickLookPreviewPanelSelector :: Selector
toggleQuickLookPreviewPanelSelector = mkSelector "toggleQuickLookPreviewPanel:"

-- | @Selector@ for @quickLookPreviewableItemsInRanges:@
quickLookPreviewableItemsInRangesSelector :: Selector
quickLookPreviewableItemsInRangesSelector = mkSelector "quickLookPreviewableItemsInRanges:"

-- | @Selector@ for @updateQuickLookPreviewPanel@
updateQuickLookPreviewPanelSelector :: Selector
updateQuickLookPreviewPanelSelector = mkSelector "updateQuickLookPreviewPanel"

-- | @Selector@ for @smartDeleteRangeForProposedRange:@
smartDeleteRangeForProposedRangeSelector :: Selector
smartDeleteRangeForProposedRangeSelector = mkSelector "smartDeleteRangeForProposedRange:"

-- | @Selector@ for @toggleSmartInsertDelete:@
toggleSmartInsertDeleteSelector :: Selector
toggleSmartInsertDeleteSelector = mkSelector "toggleSmartInsertDelete:"

-- | @Selector@ for @smartInsertForString:replacingRange:beforeString:afterString:@
smartInsertForString_replacingRange_beforeString_afterStringSelector :: Selector
smartInsertForString_replacingRange_beforeString_afterStringSelector = mkSelector "smartInsertForString:replacingRange:beforeString:afterString:"

-- | @Selector@ for @smartInsertBeforeStringForString:replacingRange:@
smartInsertBeforeStringForString_replacingRangeSelector :: Selector
smartInsertBeforeStringForString_replacingRangeSelector = mkSelector "smartInsertBeforeStringForString:replacingRange:"

-- | @Selector@ for @smartInsertAfterStringForString:replacingRange:@
smartInsertAfterStringForString_replacingRangeSelector :: Selector
smartInsertAfterStringForString_replacingRangeSelector = mkSelector "smartInsertAfterStringForString:replacingRange:"

-- | @Selector@ for @toggleAutomaticQuoteSubstitution:@
toggleAutomaticQuoteSubstitutionSelector :: Selector
toggleAutomaticQuoteSubstitutionSelector = mkSelector "toggleAutomaticQuoteSubstitution:"

-- | @Selector@ for @toggleAutomaticLinkDetection:@
toggleAutomaticLinkDetectionSelector :: Selector
toggleAutomaticLinkDetectionSelector = mkSelector "toggleAutomaticLinkDetection:"

-- | @Selector@ for @toggleAutomaticDataDetection:@
toggleAutomaticDataDetectionSelector :: Selector
toggleAutomaticDataDetectionSelector = mkSelector "toggleAutomaticDataDetection:"

-- | @Selector@ for @toggleAutomaticDashSubstitution:@
toggleAutomaticDashSubstitutionSelector :: Selector
toggleAutomaticDashSubstitutionSelector = mkSelector "toggleAutomaticDashSubstitution:"

-- | @Selector@ for @toggleAutomaticTextReplacement:@
toggleAutomaticTextReplacementSelector :: Selector
toggleAutomaticTextReplacementSelector = mkSelector "toggleAutomaticTextReplacement:"

-- | @Selector@ for @toggleAutomaticSpellingCorrection:@
toggleAutomaticSpellingCorrectionSelector :: Selector
toggleAutomaticSpellingCorrectionSelector = mkSelector "toggleAutomaticSpellingCorrection:"

-- | @Selector@ for @checkTextInRange:types:options:@
checkTextInRange_types_optionsSelector :: Selector
checkTextInRange_types_optionsSelector = mkSelector "checkTextInRange:types:options:"

-- | @Selector@ for @handleTextCheckingResults:forRange:types:options:orthography:wordCount:@
handleTextCheckingResults_forRange_types_options_orthography_wordCountSelector :: Selector
handleTextCheckingResults_forRange_types_options_orthography_wordCountSelector = mkSelector "handleTextCheckingResults:forRange:types:options:orthography:wordCount:"

-- | @Selector@ for @orderFrontSubstitutionsPanel:@
orderFrontSubstitutionsPanelSelector :: Selector
orderFrontSubstitutionsPanelSelector = mkSelector "orderFrontSubstitutionsPanel:"

-- | @Selector@ for @checkTextInSelection:@
checkTextInSelectionSelector :: Selector
checkTextInSelectionSelector = mkSelector "checkTextInSelection:"

-- | @Selector@ for @checkTextInDocument:@
checkTextInDocumentSelector :: Selector
checkTextInDocumentSelector = mkSelector "checkTextInDocument:"

-- | @Selector@ for @setSelectedRanges:affinity:stillSelecting:@
setSelectedRanges_affinity_stillSelectingSelector :: Selector
setSelectedRanges_affinity_stillSelectingSelector = mkSelector "setSelectedRanges:affinity:stillSelecting:"

-- | @Selector@ for @setSelectedRange:affinity:stillSelecting:@
setSelectedRange_affinity_stillSelectingSelector :: Selector
setSelectedRange_affinity_stillSelectingSelector = mkSelector "setSelectedRange:affinity:stillSelecting:"

-- | @Selector@ for @updateInsertionPointStateAndRestartTimer:@
updateInsertionPointStateAndRestartTimerSelector :: Selector
updateInsertionPointStateAndRestartTimerSelector = mkSelector "updateInsertionPointStateAndRestartTimer:"

-- | @Selector@ for @toggleContinuousSpellChecking:@
toggleContinuousSpellCheckingSelector :: Selector
toggleContinuousSpellCheckingSelector = mkSelector "toggleContinuousSpellChecking:"

-- | @Selector@ for @toggleGrammarChecking:@
toggleGrammarCheckingSelector :: Selector
toggleGrammarCheckingSelector = mkSelector "toggleGrammarChecking:"

-- | @Selector@ for @setSpellingState:range:@
setSpellingState_rangeSelector :: Selector
setSpellingState_rangeSelector = mkSelector "setSpellingState:range:"

-- | @Selector@ for @shouldChangeTextInRanges:replacementStrings:@
shouldChangeTextInRanges_replacementStringsSelector :: Selector
shouldChangeTextInRanges_replacementStringsSelector = mkSelector "shouldChangeTextInRanges:replacementStrings:"

-- | @Selector@ for @shouldChangeTextInRange:replacementString:@
shouldChangeTextInRange_replacementStringSelector :: Selector
shouldChangeTextInRange_replacementStringSelector = mkSelector "shouldChangeTextInRange:replacementString:"

-- | @Selector@ for @didChangeText@
didChangeTextSelector :: Selector
didChangeTextSelector = mkSelector "didChangeText"

-- | @Selector@ for @breakUndoCoalescing@
breakUndoCoalescingSelector :: Selector
breakUndoCoalescingSelector = mkSelector "breakUndoCoalescing"

-- | @Selector@ for @showFindIndicatorForRange:@
showFindIndicatorForRangeSelector :: Selector
showFindIndicatorForRangeSelector = mkSelector "showFindIndicatorForRange:"

-- | @Selector@ for @setSelectedRange:@
setSelectedRangeSelector :: Selector
setSelectedRangeSelector = mkSelector "setSelectedRange:"

-- | @Selector@ for @dragSelectionWithEvent:offset:slideBack:@
dragSelectionWithEvent_offset_slideBackSelector :: Selector
dragSelectionWithEvent_offset_slideBackSelector = mkSelector "dragSelectionWithEvent:offset:slideBack:"

-- | @Selector@ for @dragImageForSelectionWithEvent:origin:@
dragImageForSelectionWithEvent_originSelector :: Selector
dragImageForSelectionWithEvent_originSelector = mkSelector "dragImageForSelectionWithEvent:origin:"

-- | @Selector@ for @dragOperationForDraggingInfo:type:@
dragOperationForDraggingInfo_typeSelector :: Selector
dragOperationForDraggingInfo_typeSelector = mkSelector "dragOperationForDraggingInfo:type:"

-- | @Selector@ for @cleanUpAfterDragOperation@
cleanUpAfterDragOperationSelector :: Selector
cleanUpAfterDragOperationSelector = mkSelector "cleanUpAfterDragOperation"

-- | @Selector@ for @writeSelectionToPasteboard:type:@
writeSelectionToPasteboard_typeSelector :: Selector
writeSelectionToPasteboard_typeSelector = mkSelector "writeSelectionToPasteboard:type:"

-- | @Selector@ for @writeSelectionToPasteboard:types:@
writeSelectionToPasteboard_typesSelector :: Selector
writeSelectionToPasteboard_typesSelector = mkSelector "writeSelectionToPasteboard:types:"

-- | @Selector@ for @preferredPasteboardTypeFromArray:restrictedToTypesFromArray:@
preferredPasteboardTypeFromArray_restrictedToTypesFromArraySelector :: Selector
preferredPasteboardTypeFromArray_restrictedToTypesFromArraySelector = mkSelector "preferredPasteboardTypeFromArray:restrictedToTypesFromArray:"

-- | @Selector@ for @readSelectionFromPasteboard:type:@
readSelectionFromPasteboard_typeSelector :: Selector
readSelectionFromPasteboard_typeSelector = mkSelector "readSelectionFromPasteboard:type:"

-- | @Selector@ for @readSelectionFromPasteboard:@
readSelectionFromPasteboardSelector :: Selector
readSelectionFromPasteboardSelector = mkSelector "readSelectionFromPasteboard:"

-- | @Selector@ for @registerForServices@
registerForServicesSelector :: Selector
registerForServicesSelector = mkSelector "registerForServices"

-- | @Selector@ for @validRequestorForSendType:returnType:@
validRequestorForSendType_returnTypeSelector :: Selector
validRequestorForSendType_returnTypeSelector = mkSelector "validRequestorForSendType:returnType:"

-- | @Selector@ for @pasteAsPlainText:@
pasteAsPlainTextSelector :: Selector
pasteAsPlainTextSelector = mkSelector "pasteAsPlainText:"

-- | @Selector@ for @pasteAsRichText:@
pasteAsRichTextSelector :: Selector
pasteAsRichTextSelector = mkSelector "pasteAsRichText:"

-- | @Selector@ for @complete:@
completeSelector :: Selector
completeSelector = mkSelector "complete:"

-- | @Selector@ for @completionsForPartialWordRange:indexOfSelectedItem:@
completionsForPartialWordRange_indexOfSelectedItemSelector :: Selector
completionsForPartialWordRange_indexOfSelectedItemSelector = mkSelector "completionsForPartialWordRange:indexOfSelectedItem:"

-- | @Selector@ for @insertCompletion:forPartialWordRange:movement:isFinal:@
insertCompletion_forPartialWordRange_movement_isFinalSelector :: Selector
insertCompletion_forPartialWordRange_movement_isFinalSelector = mkSelector "insertCompletion:forPartialWordRange:movement:isFinal:"

-- | @Selector@ for @textContainer@
textContainerSelector :: Selector
textContainerSelector = mkSelector "textContainer"

-- | @Selector@ for @setTextContainer:@
setTextContainerSelector :: Selector
setTextContainerSelector = mkSelector "setTextContainer:"

-- | @Selector@ for @textContainerInset@
textContainerInsetSelector :: Selector
textContainerInsetSelector = mkSelector "textContainerInset"

-- | @Selector@ for @setTextContainerInset:@
setTextContainerInsetSelector :: Selector
setTextContainerInsetSelector = mkSelector "setTextContainerInset:"

-- | @Selector@ for @textContainerOrigin@
textContainerOriginSelector :: Selector
textContainerOriginSelector = mkSelector "textContainerOrigin"

-- | @Selector@ for @layoutManager@
layoutManagerSelector :: Selector
layoutManagerSelector = mkSelector "layoutManager"

-- | @Selector@ for @textStorage@
textStorageSelector :: Selector
textStorageSelector = mkSelector "textStorage"

-- | @Selector@ for @shouldDrawInsertionPoint@
shouldDrawInsertionPointSelector :: Selector
shouldDrawInsertionPointSelector = mkSelector "shouldDrawInsertionPoint"

-- | @Selector@ for @stronglyReferencesTextStorage@
stronglyReferencesTextStorageSelector :: Selector
stronglyReferencesTextStorageSelector = mkSelector "stronglyReferencesTextStorage"

-- | @Selector@ for @usesAdaptiveColorMappingForDarkAppearance@
usesAdaptiveColorMappingForDarkAppearanceSelector :: Selector
usesAdaptiveColorMappingForDarkAppearanceSelector = mkSelector "usesAdaptiveColorMappingForDarkAppearance"

-- | @Selector@ for @setUsesAdaptiveColorMappingForDarkAppearance:@
setUsesAdaptiveColorMappingForDarkAppearanceSelector :: Selector
setUsesAdaptiveColorMappingForDarkAppearanceSelector = mkSelector "setUsesAdaptiveColorMappingForDarkAppearance:"

-- | @Selector@ for @automaticTextCompletionEnabled@
automaticTextCompletionEnabledSelector :: Selector
automaticTextCompletionEnabledSelector = mkSelector "automaticTextCompletionEnabled"

-- | @Selector@ for @setAutomaticTextCompletionEnabled:@
setAutomaticTextCompletionEnabledSelector :: Selector
setAutomaticTextCompletionEnabledSelector = mkSelector "setAutomaticTextCompletionEnabled:"

-- | @Selector@ for @allowsCharacterPickerTouchBarItem@
allowsCharacterPickerTouchBarItemSelector :: Selector
allowsCharacterPickerTouchBarItemSelector = mkSelector "allowsCharacterPickerTouchBarItem"

-- | @Selector@ for @setAllowsCharacterPickerTouchBarItem:@
setAllowsCharacterPickerTouchBarItemSelector :: Selector
setAllowsCharacterPickerTouchBarItemSelector = mkSelector "setAllowsCharacterPickerTouchBarItem:"

-- | @Selector@ for @smartInsertDeleteEnabled@
smartInsertDeleteEnabledSelector :: Selector
smartInsertDeleteEnabledSelector = mkSelector "smartInsertDeleteEnabled"

-- | @Selector@ for @setSmartInsertDeleteEnabled:@
setSmartInsertDeleteEnabledSelector :: Selector
setSmartInsertDeleteEnabledSelector = mkSelector "setSmartInsertDeleteEnabled:"

-- | @Selector@ for @automaticQuoteSubstitutionEnabled@
automaticQuoteSubstitutionEnabledSelector :: Selector
automaticQuoteSubstitutionEnabledSelector = mkSelector "automaticQuoteSubstitutionEnabled"

-- | @Selector@ for @setAutomaticQuoteSubstitutionEnabled:@
setAutomaticQuoteSubstitutionEnabledSelector :: Selector
setAutomaticQuoteSubstitutionEnabledSelector = mkSelector "setAutomaticQuoteSubstitutionEnabled:"

-- | @Selector@ for @automaticLinkDetectionEnabled@
automaticLinkDetectionEnabledSelector :: Selector
automaticLinkDetectionEnabledSelector = mkSelector "automaticLinkDetectionEnabled"

-- | @Selector@ for @setAutomaticLinkDetectionEnabled:@
setAutomaticLinkDetectionEnabledSelector :: Selector
setAutomaticLinkDetectionEnabledSelector = mkSelector "setAutomaticLinkDetectionEnabled:"

-- | @Selector@ for @automaticDataDetectionEnabled@
automaticDataDetectionEnabledSelector :: Selector
automaticDataDetectionEnabledSelector = mkSelector "automaticDataDetectionEnabled"

-- | @Selector@ for @setAutomaticDataDetectionEnabled:@
setAutomaticDataDetectionEnabledSelector :: Selector
setAutomaticDataDetectionEnabledSelector = mkSelector "setAutomaticDataDetectionEnabled:"

-- | @Selector@ for @automaticDashSubstitutionEnabled@
automaticDashSubstitutionEnabledSelector :: Selector
automaticDashSubstitutionEnabledSelector = mkSelector "automaticDashSubstitutionEnabled"

-- | @Selector@ for @setAutomaticDashSubstitutionEnabled:@
setAutomaticDashSubstitutionEnabledSelector :: Selector
setAutomaticDashSubstitutionEnabledSelector = mkSelector "setAutomaticDashSubstitutionEnabled:"

-- | @Selector@ for @automaticTextReplacementEnabled@
automaticTextReplacementEnabledSelector :: Selector
automaticTextReplacementEnabledSelector = mkSelector "automaticTextReplacementEnabled"

-- | @Selector@ for @setAutomaticTextReplacementEnabled:@
setAutomaticTextReplacementEnabledSelector :: Selector
setAutomaticTextReplacementEnabledSelector = mkSelector "setAutomaticTextReplacementEnabled:"

-- | @Selector@ for @automaticSpellingCorrectionEnabled@
automaticSpellingCorrectionEnabledSelector :: Selector
automaticSpellingCorrectionEnabledSelector = mkSelector "automaticSpellingCorrectionEnabled"

-- | @Selector@ for @setAutomaticSpellingCorrectionEnabled:@
setAutomaticSpellingCorrectionEnabledSelector :: Selector
setAutomaticSpellingCorrectionEnabledSelector = mkSelector "setAutomaticSpellingCorrectionEnabled:"

-- | @Selector@ for @enabledTextCheckingTypes@
enabledTextCheckingTypesSelector :: Selector
enabledTextCheckingTypesSelector = mkSelector "enabledTextCheckingTypes"

-- | @Selector@ for @setEnabledTextCheckingTypes:@
setEnabledTextCheckingTypesSelector :: Selector
setEnabledTextCheckingTypesSelector = mkSelector "setEnabledTextCheckingTypes:"

-- | @Selector@ for @usesFindPanel@
usesFindPanelSelector :: Selector
usesFindPanelSelector = mkSelector "usesFindPanel"

-- | @Selector@ for @setUsesFindPanel:@
setUsesFindPanelSelector :: Selector
setUsesFindPanelSelector = mkSelector "setUsesFindPanel:"

-- | @Selector@ for @usesFindBar@
usesFindBarSelector :: Selector
usesFindBarSelector = mkSelector "usesFindBar"

-- | @Selector@ for @setUsesFindBar:@
setUsesFindBarSelector :: Selector
setUsesFindBarSelector = mkSelector "setUsesFindBar:"

-- | @Selector@ for @incrementalSearchingEnabled@
incrementalSearchingEnabledSelector :: Selector
incrementalSearchingEnabledSelector = mkSelector "incrementalSearchingEnabled"

-- | @Selector@ for @setIncrementalSearchingEnabled:@
setIncrementalSearchingEnabledSelector :: Selector
setIncrementalSearchingEnabledSelector = mkSelector "setIncrementalSearchingEnabled:"

-- | @Selector@ for @inlinePredictionType@
inlinePredictionTypeSelector :: Selector
inlinePredictionTypeSelector = mkSelector "inlinePredictionType"

-- | @Selector@ for @setInlinePredictionType:@
setInlinePredictionTypeSelector :: Selector
setInlinePredictionTypeSelector = mkSelector "setInlinePredictionType:"

-- | @Selector@ for @mathExpressionCompletionType@
mathExpressionCompletionTypeSelector :: Selector
mathExpressionCompletionTypeSelector = mkSelector "mathExpressionCompletionType"

-- | @Selector@ for @setMathExpressionCompletionType:@
setMathExpressionCompletionTypeSelector :: Selector
setMathExpressionCompletionTypeSelector = mkSelector "setMathExpressionCompletionType:"

-- | @Selector@ for @selectedRanges@
selectedRangesSelector :: Selector
selectedRangesSelector = mkSelector "selectedRanges"

-- | @Selector@ for @setSelectedRanges:@
setSelectedRangesSelector :: Selector
setSelectedRangesSelector = mkSelector "setSelectedRanges:"

-- | @Selector@ for @selectionAffinity@
selectionAffinitySelector :: Selector
selectionAffinitySelector = mkSelector "selectionAffinity"

-- | @Selector@ for @selectionGranularity@
selectionGranularitySelector :: Selector
selectionGranularitySelector = mkSelector "selectionGranularity"

-- | @Selector@ for @setSelectionGranularity:@
setSelectionGranularitySelector :: Selector
setSelectionGranularitySelector = mkSelector "setSelectionGranularity:"

-- | @Selector@ for @selectedTextAttributes@
selectedTextAttributesSelector :: Selector
selectedTextAttributesSelector = mkSelector "selectedTextAttributes"

-- | @Selector@ for @setSelectedTextAttributes:@
setSelectedTextAttributesSelector :: Selector
setSelectedTextAttributesSelector = mkSelector "setSelectedTextAttributes:"

-- | @Selector@ for @insertionPointColor@
insertionPointColorSelector :: Selector
insertionPointColorSelector = mkSelector "insertionPointColor"

-- | @Selector@ for @setInsertionPointColor:@
setInsertionPointColorSelector :: Selector
setInsertionPointColorSelector = mkSelector "setInsertionPointColor:"

-- | @Selector@ for @markedTextAttributes@
markedTextAttributesSelector :: Selector
markedTextAttributesSelector = mkSelector "markedTextAttributes"

-- | @Selector@ for @setMarkedTextAttributes:@
setMarkedTextAttributesSelector :: Selector
setMarkedTextAttributesSelector = mkSelector "setMarkedTextAttributes:"

-- | @Selector@ for @linkTextAttributes@
linkTextAttributesSelector :: Selector
linkTextAttributesSelector = mkSelector "linkTextAttributes"

-- | @Selector@ for @setLinkTextAttributes:@
setLinkTextAttributesSelector :: Selector
setLinkTextAttributesSelector = mkSelector "setLinkTextAttributes:"

-- | @Selector@ for @displaysLinkToolTips@
displaysLinkToolTipsSelector :: Selector
displaysLinkToolTipsSelector = mkSelector "displaysLinkToolTips"

-- | @Selector@ for @setDisplaysLinkToolTips:@
setDisplaysLinkToolTipsSelector :: Selector
setDisplaysLinkToolTipsSelector = mkSelector "setDisplaysLinkToolTips:"

-- | @Selector@ for @acceptsGlyphInfo@
acceptsGlyphInfoSelector :: Selector
acceptsGlyphInfoSelector = mkSelector "acceptsGlyphInfo"

-- | @Selector@ for @setAcceptsGlyphInfo:@
setAcceptsGlyphInfoSelector :: Selector
setAcceptsGlyphInfoSelector = mkSelector "setAcceptsGlyphInfo:"

-- | @Selector@ for @usesRuler@
usesRulerSelector :: Selector
usesRulerSelector = mkSelector "usesRuler"

-- | @Selector@ for @setUsesRuler:@
setUsesRulerSelector :: Selector
setUsesRulerSelector = mkSelector "setUsesRuler:"

-- | @Selector@ for @usesInspectorBar@
usesInspectorBarSelector :: Selector
usesInspectorBarSelector = mkSelector "usesInspectorBar"

-- | @Selector@ for @setUsesInspectorBar:@
setUsesInspectorBarSelector :: Selector
setUsesInspectorBarSelector = mkSelector "setUsesInspectorBar:"

-- | @Selector@ for @continuousSpellCheckingEnabled@
continuousSpellCheckingEnabledSelector :: Selector
continuousSpellCheckingEnabledSelector = mkSelector "continuousSpellCheckingEnabled"

-- | @Selector@ for @setContinuousSpellCheckingEnabled:@
setContinuousSpellCheckingEnabledSelector :: Selector
setContinuousSpellCheckingEnabledSelector = mkSelector "setContinuousSpellCheckingEnabled:"

-- | @Selector@ for @spellCheckerDocumentTag@
spellCheckerDocumentTagSelector :: Selector
spellCheckerDocumentTagSelector = mkSelector "spellCheckerDocumentTag"

-- | @Selector@ for @grammarCheckingEnabled@
grammarCheckingEnabledSelector :: Selector
grammarCheckingEnabledSelector = mkSelector "grammarCheckingEnabled"

-- | @Selector@ for @setGrammarCheckingEnabled:@
setGrammarCheckingEnabledSelector :: Selector
setGrammarCheckingEnabledSelector = mkSelector "setGrammarCheckingEnabled:"

-- | @Selector@ for @typingAttributes@
typingAttributesSelector :: Selector
typingAttributesSelector = mkSelector "typingAttributes"

-- | @Selector@ for @setTypingAttributes:@
setTypingAttributesSelector :: Selector
setTypingAttributesSelector = mkSelector "setTypingAttributes:"

-- | @Selector@ for @rangesForUserTextChange@
rangesForUserTextChangeSelector :: Selector
rangesForUserTextChangeSelector = mkSelector "rangesForUserTextChange"

-- | @Selector@ for @rangesForUserCharacterAttributeChange@
rangesForUserCharacterAttributeChangeSelector :: Selector
rangesForUserCharacterAttributeChangeSelector = mkSelector "rangesForUserCharacterAttributeChange"

-- | @Selector@ for @rangesForUserParagraphAttributeChange@
rangesForUserParagraphAttributeChangeSelector :: Selector
rangesForUserParagraphAttributeChangeSelector = mkSelector "rangesForUserParagraphAttributeChange"

-- | @Selector@ for @rangeForUserTextChange@
rangeForUserTextChangeSelector :: Selector
rangeForUserTextChangeSelector = mkSelector "rangeForUserTextChange"

-- | @Selector@ for @rangeForUserCharacterAttributeChange@
rangeForUserCharacterAttributeChangeSelector :: Selector
rangeForUserCharacterAttributeChangeSelector = mkSelector "rangeForUserCharacterAttributeChange"

-- | @Selector@ for @rangeForUserParagraphAttributeChange@
rangeForUserParagraphAttributeChangeSelector :: Selector
rangeForUserParagraphAttributeChangeSelector = mkSelector "rangeForUserParagraphAttributeChange"

-- | @Selector@ for @allowsDocumentBackgroundColorChange@
allowsDocumentBackgroundColorChangeSelector :: Selector
allowsDocumentBackgroundColorChangeSelector = mkSelector "allowsDocumentBackgroundColorChange"

-- | @Selector@ for @setAllowsDocumentBackgroundColorChange:@
setAllowsDocumentBackgroundColorChangeSelector :: Selector
setAllowsDocumentBackgroundColorChangeSelector = mkSelector "setAllowsDocumentBackgroundColorChange:"

-- | @Selector@ for @defaultParagraphStyle@
defaultParagraphStyleSelector :: Selector
defaultParagraphStyleSelector = mkSelector "defaultParagraphStyle"

-- | @Selector@ for @setDefaultParagraphStyle:@
setDefaultParagraphStyleSelector :: Selector
setDefaultParagraphStyleSelector = mkSelector "setDefaultParagraphStyle:"

-- | @Selector@ for @allowsUndo@
allowsUndoSelector :: Selector
allowsUndoSelector = mkSelector "allowsUndo"

-- | @Selector@ for @setAllowsUndo:@
setAllowsUndoSelector :: Selector
setAllowsUndoSelector = mkSelector "setAllowsUndo:"

-- | @Selector@ for @coalescingUndo@
coalescingUndoSelector :: Selector
coalescingUndoSelector = mkSelector "coalescingUndo"

-- | @Selector@ for @allowsImageEditing@
allowsImageEditingSelector :: Selector
allowsImageEditingSelector = mkSelector "allowsImageEditing"

-- | @Selector@ for @setAllowsImageEditing:@
setAllowsImageEditingSelector :: Selector
setAllowsImageEditingSelector = mkSelector "setAllowsImageEditing:"

-- | @Selector@ for @usesRolloverButtonForSelection@
usesRolloverButtonForSelectionSelector :: Selector
usesRolloverButtonForSelectionSelector = mkSelector "usesRolloverButtonForSelection"

-- | @Selector@ for @setUsesRolloverButtonForSelection:@
setUsesRolloverButtonForSelectionSelector :: Selector
setUsesRolloverButtonForSelectionSelector = mkSelector "setUsesRolloverButtonForSelection:"

-- | @Selector@ for @editable@
editableSelector :: Selector
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @selectable@
selectableSelector :: Selector
selectableSelector = mkSelector "selectable"

-- | @Selector@ for @setSelectable:@
setSelectableSelector :: Selector
setSelectableSelector = mkSelector "setSelectable:"

-- | @Selector@ for @richText@
richTextSelector :: Selector
richTextSelector = mkSelector "richText"

-- | @Selector@ for @setRichText:@
setRichTextSelector :: Selector
setRichTextSelector = mkSelector "setRichText:"

-- | @Selector@ for @importsGraphics@
importsGraphicsSelector :: Selector
importsGraphicsSelector = mkSelector "importsGraphics"

-- | @Selector@ for @setImportsGraphics:@
setImportsGraphicsSelector :: Selector
setImportsGraphicsSelector = mkSelector "setImportsGraphics:"

-- | @Selector@ for @drawsBackground@
drawsBackgroundSelector :: Selector
drawsBackgroundSelector = mkSelector "drawsBackground"

-- | @Selector@ for @setDrawsBackground:@
setDrawsBackgroundSelector :: Selector
setDrawsBackgroundSelector = mkSelector "setDrawsBackground:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @setFieldEditor:@
setFieldEditorSelector :: Selector
setFieldEditorSelector = mkSelector "setFieldEditor:"

-- | @Selector@ for @usesFontPanel@
usesFontPanelSelector :: Selector
usesFontPanelSelector = mkSelector "usesFontPanel"

-- | @Selector@ for @setUsesFontPanel:@
setUsesFontPanelSelector :: Selector
setUsesFontPanelSelector = mkSelector "setUsesFontPanel:"

-- | @Selector@ for @rulerVisible@
rulerVisibleSelector :: Selector
rulerVisibleSelector = mkSelector "rulerVisible"

-- | @Selector@ for @setRulerVisible:@
setRulerVisibleSelector :: Selector
setRulerVisibleSelector = mkSelector "setRulerVisible:"

-- | @Selector@ for @writingToolsActive@
writingToolsActiveSelector :: Selector
writingToolsActiveSelector = mkSelector "writingToolsActive"

-- | @Selector@ for @writingToolsBehavior@
writingToolsBehaviorSelector :: Selector
writingToolsBehaviorSelector = mkSelector "writingToolsBehavior"

-- | @Selector@ for @setWritingToolsBehavior:@
setWritingToolsBehaviorSelector :: Selector
setWritingToolsBehaviorSelector = mkSelector "setWritingToolsBehavior:"

-- | @Selector@ for @allowedWritingToolsResultOptions@
allowedWritingToolsResultOptionsSelector :: Selector
allowedWritingToolsResultOptionsSelector = mkSelector "allowedWritingToolsResultOptions"

-- | @Selector@ for @setAllowedWritingToolsResultOptions:@
setAllowedWritingToolsResultOptionsSelector :: Selector
setAllowedWritingToolsResultOptionsSelector = mkSelector "setAllowedWritingToolsResultOptions:"

-- | @Selector@ for @acceptableDragTypes@
acceptableDragTypesSelector :: Selector
acceptableDragTypesSelector = mkSelector "acceptableDragTypes"

-- | @Selector@ for @writablePasteboardTypes@
writablePasteboardTypesSelector :: Selector
writablePasteboardTypesSelector = mkSelector "writablePasteboardTypes"

-- | @Selector@ for @readablePasteboardTypes@
readablePasteboardTypesSelector :: Selector
readablePasteboardTypesSelector = mkSelector "readablePasteboardTypes"

-- | @Selector@ for @rangeForUserCompletion@
rangeForUserCompletionSelector :: Selector
rangeForUserCompletionSelector = mkSelector "rangeForUserCompletion"

