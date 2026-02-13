{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , textLayoutManager
  , textContentStorage
  , shouldDrawInsertionPoint
  , stronglyReferencesTextStorage
  , usesAdaptiveColorMappingForDarkAppearance
  , setUsesAdaptiveColorMappingForDarkAppearance
  , textHighlightAttributes
  , setTextHighlightAttributes
  , automaticTextCompletionEnabled
  , setAutomaticTextCompletionEnabled
  , allowsCharacterPickerTouchBarItem
  , setAllowsCharacterPickerTouchBarItem
  , candidateListTouchBarItem
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
  , delegate
  , setDelegate
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
  , allowedInputSourceLocales
  , setAllowedInputSourceLocales
  , writingToolsActive
  , writingToolsBehavior
  , setWritingToolsBehavior
  , allowedWritingToolsResultOptions
  , setAllowedWritingToolsResultOptions
  , acceptableDragTypes
  , writablePasteboardTypes
  , readablePasteboardTypes
  , rangeForUserCompletion
  , acceptableDragTypesSelector
  , acceptsGlyphInfoSelector
  , alignJustifiedSelector
  , allowedInputSourceLocalesSelector
  , allowedWritingToolsResultOptionsSelector
  , allowsCharacterPickerTouchBarItemSelector
  , allowsDocumentBackgroundColorChangeSelector
  , allowsImageEditingSelector
  , allowsUndoSelector
  , automaticDashSubstitutionEnabledSelector
  , automaticDataDetectionEnabledSelector
  , automaticLinkDetectionEnabledSelector
  , automaticQuoteSubstitutionEnabledSelector
  , automaticSpellingCorrectionEnabledSelector
  , automaticTextCompletionEnabledSelector
  , automaticTextReplacementEnabledSelector
  , backgroundColorSelector
  , breakUndoCoalescingSelector
  , candidateListTouchBarItemSelector
  , changeAttributesSelector
  , changeColorSelector
  , changeDocumentBackgroundColorSelector
  , changeLayoutOrientationSelector
  , characterIndexForInsertionAtPointSelector
  , checkTextInDocumentSelector
  , checkTextInRange_types_optionsSelector
  , checkTextInSelectionSelector
  , cleanUpAfterDragOperationSelector
  , clickedOnLink_atIndexSelector
  , coalescingUndoSelector
  , completeSelector
  , completionsForPartialWordRange_indexOfSelectedItemSelector
  , continuousSpellCheckingEnabledSelector
  , defaultParagraphStyleSelector
  , delegateSelector
  , didChangeTextSelector
  , displaysLinkToolTipsSelector
  , dragImageForSelectionWithEvent_originSelector
  , dragOperationForDraggingInfo_typeSelector
  , dragSelectionWithEvent_offset_slideBackSelector
  , drawInsertionPointInRect_color_turnedOnSelector
  , drawTextHighlightBackgroundForTextRange_originSelector
  , drawViewBackgroundInRectSelector
  , drawsBackgroundSelector
  , editableSelector
  , enabledTextCheckingTypesSelector
  , fieldEditorSelector
  , grammarCheckingEnabledSelector
  , handleTextCheckingResults_forRange_types_options_orthography_wordCountSelector
  , highlightSelector
  , importsGraphicsSelector
  , incrementalSearchingEnabledSelector
  , initUsingTextLayoutManagerSelector
  , initWithCoderSelector
  , initWithFrameSelector
  , initWithFrame_textContainerSelector
  , inlinePredictionTypeSelector
  , insertCompletion_forPartialWordRange_movement_isFinalSelector
  , insertTextSelector
  , insertionPointColorSelector
  , invalidateTextContainerOriginSelector
  , layoutManagerSelector
  , linkTextAttributesSelector
  , loosenKerningSelector
  , lowerBaselineSelector
  , markedTextAttributesSelector
  , mathExpressionCompletionTypeSelector
  , nsTextViewFieldEditorSelector
  , orderFrontLinkPanelSelector
  , orderFrontListPanelSelector
  , orderFrontSharingServicePickerSelector
  , orderFrontSpacingPanelSelector
  , orderFrontSubstitutionsPanelSelector
  , orderFrontTablePanelSelector
  , outlineSelector
  , pasteAsPlainTextSelector
  , pasteAsRichTextSelector
  , performFindPanelActionSelector
  , performValidatedReplacementInRange_withAttributedStringSelector
  , preferredPasteboardTypeFromArray_restrictedToTypesFromArraySelector
  , quickLookPreviewableItemsInRangesSelector
  , raiseBaselineSelector
  , rangeForUserCharacterAttributeChangeSelector
  , rangeForUserCompletionSelector
  , rangeForUserParagraphAttributeChangeSelector
  , rangeForUserTextChangeSelector
  , rangesForUserCharacterAttributeChangeSelector
  , rangesForUserParagraphAttributeChangeSelector
  , rangesForUserTextChangeSelector
  , readSelectionFromPasteboardSelector
  , readSelectionFromPasteboard_typeSelector
  , readablePasteboardTypesSelector
  , registerForServicesSelector
  , replaceTextContainerSelector
  , richTextSelector
  , rulerView_didAddMarkerSelector
  , rulerView_didMoveMarkerSelector
  , rulerView_didRemoveMarkerSelector
  , rulerView_handleMouseDownSelector
  , rulerView_shouldAddMarkerSelector
  , rulerView_shouldMoveMarkerSelector
  , rulerView_shouldRemoveMarkerSelector
  , rulerView_willAddMarker_atLocationSelector
  , rulerView_willMoveMarker_toLocationSelector
  , rulerVisibleSelector
  , scrollableDocumentContentTextViewSelector
  , scrollablePlainDocumentContentTextViewSelector
  , scrollableTextViewSelector
  , selectableSelector
  , selectedRangesSelector
  , selectedTextAttributesSelector
  , selectionAffinitySelector
  , selectionGranularitySelector
  , selectionRangeForProposedRange_granularitySelector
  , setAcceptsGlyphInfoSelector
  , setAlignment_rangeSelector
  , setAllowedInputSourceLocalesSelector
  , setAllowedWritingToolsResultOptionsSelector
  , setAllowsCharacterPickerTouchBarItemSelector
  , setAllowsDocumentBackgroundColorChangeSelector
  , setAllowsImageEditingSelector
  , setAllowsUndoSelector
  , setAutomaticDashSubstitutionEnabledSelector
  , setAutomaticDataDetectionEnabledSelector
  , setAutomaticLinkDetectionEnabledSelector
  , setAutomaticQuoteSubstitutionEnabledSelector
  , setAutomaticSpellingCorrectionEnabledSelector
  , setAutomaticTextCompletionEnabledSelector
  , setAutomaticTextReplacementEnabledSelector
  , setBackgroundColorSelector
  , setBaseWritingDirection_rangeSelector
  , setConstrainedFrameSizeSelector
  , setContinuousSpellCheckingEnabledSelector
  , setDefaultParagraphStyleSelector
  , setDelegateSelector
  , setDisplaysLinkToolTipsSelector
  , setDrawsBackgroundSelector
  , setEditableSelector
  , setEnabledTextCheckingTypesSelector
  , setFieldEditorSelector
  , setGrammarCheckingEnabledSelector
  , setImportsGraphicsSelector
  , setIncrementalSearchingEnabledSelector
  , setInlinePredictionTypeSelector
  , setInsertionPointColorSelector
  , setLayoutOrientationSelector
  , setLinkTextAttributesSelector
  , setMarkedTextAttributesSelector
  , setMathExpressionCompletionTypeSelector
  , setNeedsDisplayInRect_avoidAdditionalLayoutSelector
  , setRichTextSelector
  , setRulerVisibleSelector
  , setSelectableSelector
  , setSelectedRangeSelector
  , setSelectedRange_affinity_stillSelectingSelector
  , setSelectedRangesSelector
  , setSelectedRanges_affinity_stillSelectingSelector
  , setSelectedTextAttributesSelector
  , setSelectionGranularitySelector
  , setSmartInsertDeleteEnabledSelector
  , setSpellingState_rangeSelector
  , setTextContainerInsetSelector
  , setTextContainerSelector
  , setTextHighlightAttributesSelector
  , setTypingAttributesSelector
  , setUsesAdaptiveColorMappingForDarkAppearanceSelector
  , setUsesFindBarSelector
  , setUsesFindPanelSelector
  , setUsesFontPanelSelector
  , setUsesInspectorBarSelector
  , setUsesRolloverButtonForSelectionSelector
  , setUsesRulerSelector
  , setWritingToolsBehaviorSelector
  , shouldChangeTextInRange_replacementStringSelector
  , shouldChangeTextInRanges_replacementStringsSelector
  , shouldDrawInsertionPointSelector
  , showFindIndicatorForRangeSelector
  , smartDeleteRangeForProposedRangeSelector
  , smartInsertAfterStringForString_replacingRangeSelector
  , smartInsertBeforeStringForString_replacingRangeSelector
  , smartInsertDeleteEnabledSelector
  , smartInsertForString_replacingRange_beforeString_afterStringSelector
  , spellCheckerDocumentTagSelector
  , startSpeakingSelector
  , stopSpeakingSelector
  , stronglyReferencesTextStorageSelector
  , textContainerInsetSelector
  , textContainerOriginSelector
  , textContainerSelector
  , textContentStorageSelector
  , textHighlightAttributesSelector
  , textLayoutManagerSelector
  , textStorageSelector
  , textViewUsingTextLayoutManagerSelector
  , tightenKerningSelector
  , toggleAutomaticDashSubstitutionSelector
  , toggleAutomaticDataDetectionSelector
  , toggleAutomaticLinkDetectionSelector
  , toggleAutomaticQuoteSubstitutionSelector
  , toggleAutomaticSpellingCorrectionSelector
  , toggleAutomaticTextCompletionSelector
  , toggleAutomaticTextReplacementSelector
  , toggleBaseWritingDirectionSelector
  , toggleContinuousSpellCheckingSelector
  , toggleGrammarCheckingSelector
  , toggleQuickLookPreviewPanelSelector
  , toggleSmartInsertDeleteSelector
  , toggleTraditionalCharacterShapeSelector
  , turnOffKerningSelector
  , turnOffLigaturesSelector
  , typingAttributesSelector
  , updateCandidatesSelector
  , updateDragTypeRegistrationSelector
  , updateFontPanelSelector
  , updateInsertionPointStateAndRestartTimerSelector
  , updateQuickLookPreviewPanelSelector
  , updateRulerSelector
  , updateTextTouchBarItemsSelector
  , updateTouchBarItemIdentifiersSelector
  , useAllLigaturesSelector
  , useStandardKerningSelector
  , useStandardLigaturesSelector
  , usesAdaptiveColorMappingForDarkAppearanceSelector
  , usesFindBarSelector
  , usesFindPanelSelector
  , usesFontPanelSelector
  , usesInspectorBarSelector
  , usesRolloverButtonForSelectionSelector
  , usesRulerSelector
  , validRequestorForSendType_returnTypeSelector
  , writablePasteboardTypesSelector
  , writeSelectionToPasteboard_typeSelector
  , writeSelectionToPasteboard_typesSelector
  , writingToolsActiveSelector
  , writingToolsBehaviorSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithFrame_textContainer nsTextView frameRect container =
  sendOwnedMessage nsTextView initWithFrame_textContainerSelector frameRect (toNSTextContainer container)

-- | @- initWithCoder:@
initWithCoder :: (IsNSTextView nsTextView, IsNSCoder coder) => nsTextView -> coder -> IO (Id NSTextView)
initWithCoder nsTextView coder =
  sendOwnedMessage nsTextView initWithCoderSelector (toNSCoder coder)

-- | @- initWithFrame:@
initWithFrame :: IsNSTextView nsTextView => nsTextView -> NSRect -> IO (Id NSTextView)
initWithFrame nsTextView frameRect =
  sendOwnedMessage nsTextView initWithFrameSelector frameRect

-- | @- initUsingTextLayoutManager:@
initUsingTextLayoutManager :: IsNSTextView nsTextView => nsTextView -> Bool -> IO (Id NSTextView)
initUsingTextLayoutManager nsTextView usingTextLayoutManager =
  sendOwnedMessage nsTextView initUsingTextLayoutManagerSelector usingTextLayoutManager

-- | @+ textViewUsingTextLayoutManager:@
textViewUsingTextLayoutManager :: Bool -> IO (Id NSTextView)
textViewUsingTextLayoutManager usingTextLayoutManager =
  do
    cls' <- getRequiredClass "NSTextView"
    sendClassMessage cls' textViewUsingTextLayoutManagerSelector usingTextLayoutManager

-- | @- replaceTextContainer:@
replaceTextContainer :: (IsNSTextView nsTextView, IsNSTextContainer newContainer) => nsTextView -> newContainer -> IO ()
replaceTextContainer nsTextView newContainer =
  sendMessage nsTextView replaceTextContainerSelector (toNSTextContainer newContainer)

-- | @- invalidateTextContainerOrigin@
invalidateTextContainerOrigin :: IsNSTextView nsTextView => nsTextView -> IO ()
invalidateTextContainerOrigin nsTextView =
  sendMessage nsTextView invalidateTextContainerOriginSelector

-- | *********************** Key binding entry-point ************************
--
-- ObjC selector: @- insertText:@
insertText :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
insertText nsTextView insertString =
  sendMessage nsTextView insertTextSelector insertString

-- | ************************* Sizing methods **************************
--
-- ObjC selector: @- setConstrainedFrameSize:@
setConstrainedFrameSize :: IsNSTextView nsTextView => nsTextView -> NSSize -> IO ()
setConstrainedFrameSize nsTextView desiredSize =
  sendMessage nsTextView setConstrainedFrameSizeSelector desiredSize

-- | @- setAlignment:range:@
setAlignment_range :: IsNSTextView nsTextView => nsTextView -> NSTextAlignment -> NSRange -> IO ()
setAlignment_range nsTextView alignment range =
  sendMessage nsTextView setAlignment_rangeSelector alignment range

-- | @- setBaseWritingDirection:range:@
setBaseWritingDirection_range :: IsNSTextView nsTextView => nsTextView -> NSWritingDirection -> NSRange -> IO ()
setBaseWritingDirection_range nsTextView writingDirection range =
  sendMessage nsTextView setBaseWritingDirection_rangeSelector writingDirection range

-- | ************************* New Font menu commands **************************
--
-- ObjC selector: @- turnOffKerning:@
turnOffKerning :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
turnOffKerning nsTextView sender =
  sendMessage nsTextView turnOffKerningSelector sender

-- | @- tightenKerning:@
tightenKerning :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
tightenKerning nsTextView sender =
  sendMessage nsTextView tightenKerningSelector sender

-- | @- loosenKerning:@
loosenKerning :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
loosenKerning nsTextView sender =
  sendMessage nsTextView loosenKerningSelector sender

-- | @- useStandardKerning:@
useStandardKerning :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
useStandardKerning nsTextView sender =
  sendMessage nsTextView useStandardKerningSelector sender

-- | @- turnOffLigatures:@
turnOffLigatures :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
turnOffLigatures nsTextView sender =
  sendMessage nsTextView turnOffLigaturesSelector sender

-- | @- useStandardLigatures:@
useStandardLigatures :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
useStandardLigatures nsTextView sender =
  sendMessage nsTextView useStandardLigaturesSelector sender

-- | @- useAllLigatures:@
useAllLigatures :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
useAllLigatures nsTextView sender =
  sendMessage nsTextView useAllLigaturesSelector sender

-- | @- raiseBaseline:@
raiseBaseline :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
raiseBaseline nsTextView sender =
  sendMessage nsTextView raiseBaselineSelector sender

-- | @- lowerBaseline:@
lowerBaseline :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
lowerBaseline nsTextView sender =
  sendMessage nsTextView lowerBaselineSelector sender

-- | @- toggleTraditionalCharacterShape:@
toggleTraditionalCharacterShape :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleTraditionalCharacterShape nsTextView sender =
  sendMessage nsTextView toggleTraditionalCharacterShapeSelector sender

-- | @- outline:@
outline :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
outline nsTextView sender =
  sendMessage nsTextView outlineSelector sender

-- | ************************* Find menu commands **************************
--
-- ObjC selector: @- performFindPanelAction:@
performFindPanelAction :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
performFindPanelAction nsTextView sender =
  sendMessage nsTextView performFindPanelActionSelector sender

-- | ************************* New Text commands **************************
--
-- ObjC selector: @- alignJustified:@
alignJustified :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
alignJustified nsTextView sender =
  sendMessage nsTextView alignJustifiedSelector sender

-- | @- changeColor:@
changeColor :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
changeColor nsTextView sender =
  sendMessage nsTextView changeColorSelector sender

-- | @- changeAttributes:@
changeAttributes :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
changeAttributes nsTextView sender =
  sendMessage nsTextView changeAttributesSelector sender

-- | @- changeDocumentBackgroundColor:@
changeDocumentBackgroundColor :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
changeDocumentBackgroundColor nsTextView sender =
  sendMessage nsTextView changeDocumentBackgroundColorSelector sender

-- | @- orderFrontSpacingPanel:@
orderFrontSpacingPanel :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
orderFrontSpacingPanel nsTextView sender =
  sendMessage nsTextView orderFrontSpacingPanelSelector sender

-- | @- orderFrontLinkPanel:@
orderFrontLinkPanel :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
orderFrontLinkPanel nsTextView sender =
  sendMessage nsTextView orderFrontLinkPanelSelector sender

-- | @- orderFrontListPanel:@
orderFrontListPanel :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
orderFrontListPanel nsTextView sender =
  sendMessage nsTextView orderFrontListPanelSelector sender

-- | @- orderFrontTablePanel:@
orderFrontTablePanel :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
orderFrontTablePanel nsTextView sender =
  sendMessage nsTextView orderFrontTablePanelSelector sender

-- | ************************* Ruler support **************************
--
-- ObjC selector: @- rulerView:didMoveMarker:@
rulerView_didMoveMarker :: (IsNSTextView nsTextView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsTextView -> ruler -> marker -> IO ()
rulerView_didMoveMarker nsTextView ruler marker =
  sendMessage nsTextView rulerView_didMoveMarkerSelector (toNSRulerView ruler) (toNSRulerMarker marker)

-- | @- rulerView:didRemoveMarker:@
rulerView_didRemoveMarker :: (IsNSTextView nsTextView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsTextView -> ruler -> marker -> IO ()
rulerView_didRemoveMarker nsTextView ruler marker =
  sendMessage nsTextView rulerView_didRemoveMarkerSelector (toNSRulerView ruler) (toNSRulerMarker marker)

-- | @- rulerView:didAddMarker:@
rulerView_didAddMarker :: (IsNSTextView nsTextView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsTextView -> ruler -> marker -> IO ()
rulerView_didAddMarker nsTextView ruler marker =
  sendMessage nsTextView rulerView_didAddMarkerSelector (toNSRulerView ruler) (toNSRulerMarker marker)

-- | @- rulerView:shouldMoveMarker:@
rulerView_shouldMoveMarker :: (IsNSTextView nsTextView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsTextView -> ruler -> marker -> IO Bool
rulerView_shouldMoveMarker nsTextView ruler marker =
  sendMessage nsTextView rulerView_shouldMoveMarkerSelector (toNSRulerView ruler) (toNSRulerMarker marker)

-- | @- rulerView:shouldAddMarker:@
rulerView_shouldAddMarker :: (IsNSTextView nsTextView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsTextView -> ruler -> marker -> IO Bool
rulerView_shouldAddMarker nsTextView ruler marker =
  sendMessage nsTextView rulerView_shouldAddMarkerSelector (toNSRulerView ruler) (toNSRulerMarker marker)

-- | @- rulerView:willMoveMarker:toLocation:@
rulerView_willMoveMarker_toLocation :: (IsNSTextView nsTextView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsTextView -> ruler -> marker -> CDouble -> IO CDouble
rulerView_willMoveMarker_toLocation nsTextView ruler marker location =
  sendMessage nsTextView rulerView_willMoveMarker_toLocationSelector (toNSRulerView ruler) (toNSRulerMarker marker) location

-- | @- rulerView:shouldRemoveMarker:@
rulerView_shouldRemoveMarker :: (IsNSTextView nsTextView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsTextView -> ruler -> marker -> IO Bool
rulerView_shouldRemoveMarker nsTextView ruler marker =
  sendMessage nsTextView rulerView_shouldRemoveMarkerSelector (toNSRulerView ruler) (toNSRulerMarker marker)

-- | @- rulerView:willAddMarker:atLocation:@
rulerView_willAddMarker_atLocation :: (IsNSTextView nsTextView, IsNSRulerView ruler, IsNSRulerMarker marker) => nsTextView -> ruler -> marker -> CDouble -> IO CDouble
rulerView_willAddMarker_atLocation nsTextView ruler marker location =
  sendMessage nsTextView rulerView_willAddMarker_atLocationSelector (toNSRulerView ruler) (toNSRulerMarker marker) location

-- | @- rulerView:handleMouseDown:@
rulerView_handleMouseDown :: (IsNSTextView nsTextView, IsNSRulerView ruler, IsNSEvent event) => nsTextView -> ruler -> event -> IO ()
rulerView_handleMouseDown nsTextView ruler event =
  sendMessage nsTextView rulerView_handleMouseDownSelector (toNSRulerView ruler) (toNSEvent event)

-- | ************************* Fine display control **************************
--
-- ObjC selector: @- setNeedsDisplayInRect:avoidAdditionalLayout:@
setNeedsDisplayInRect_avoidAdditionalLayout :: IsNSTextView nsTextView => nsTextView -> NSRect -> Bool -> IO ()
setNeedsDisplayInRect_avoidAdditionalLayout nsTextView rect flag =
  sendMessage nsTextView setNeedsDisplayInRect_avoidAdditionalLayoutSelector rect flag

-- | @- drawInsertionPointInRect:color:turnedOn:@
drawInsertionPointInRect_color_turnedOn :: (IsNSTextView nsTextView, IsNSColor color) => nsTextView -> NSRect -> color -> Bool -> IO ()
drawInsertionPointInRect_color_turnedOn nsTextView rect color flag =
  sendMessage nsTextView drawInsertionPointInRect_color_turnedOnSelector rect (toNSColor color) flag

-- | @- drawViewBackgroundInRect:@
drawViewBackgroundInRect :: IsNSTextView nsTextView => nsTextView -> NSRect -> IO ()
drawViewBackgroundInRect nsTextView rect =
  sendMessage nsTextView drawViewBackgroundInRectSelector rect

-- | ************************* Especially for subclassers **************************
--
-- ObjC selector: @- updateRuler@
updateRuler :: IsNSTextView nsTextView => nsTextView -> IO ()
updateRuler nsTextView =
  sendMessage nsTextView updateRulerSelector

-- | @- updateFontPanel@
updateFontPanel :: IsNSTextView nsTextView => nsTextView -> IO ()
updateFontPanel nsTextView =
  sendMessage nsTextView updateFontPanelSelector

-- | @- updateDragTypeRegistration@
updateDragTypeRegistration :: IsNSTextView nsTextView => nsTextView -> IO ()
updateDragTypeRegistration nsTextView =
  sendMessage nsTextView updateDragTypeRegistrationSelector

-- | @- selectionRangeForProposedRange:granularity:@
selectionRangeForProposedRange_granularity :: IsNSTextView nsTextView => nsTextView -> NSRange -> NSSelectionGranularity -> IO NSRange
selectionRangeForProposedRange_granularity nsTextView proposedCharRange granularity =
  sendMessage nsTextView selectionRangeForProposedRange_granularitySelector proposedCharRange granularity

-- | ************************* Especially for subclassers **************************
--
-- ObjC selector: @- clickedOnLink:atIndex:@
clickedOnLink_atIndex :: IsNSTextView nsTextView => nsTextView -> RawId -> CULong -> IO ()
clickedOnLink_atIndex nsTextView link charIndex =
  sendMessage nsTextView clickedOnLink_atIndexSelector link charIndex

-- | *********************** Speech support ************************
--
-- ObjC selector: @- startSpeaking:@
startSpeaking :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
startSpeaking nsTextView sender =
  sendMessage nsTextView startSpeakingSelector sender

-- | @- stopSpeaking:@
stopSpeaking :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
stopSpeaking nsTextView sender =
  sendMessage nsTextView stopSpeakingSelector sender

-- | @- setLayoutOrientation:@
setLayoutOrientation :: IsNSTextView nsTextView => nsTextView -> NSTextLayoutOrientation -> IO ()
setLayoutOrientation nsTextView orientation =
  sendMessage nsTextView setLayoutOrientationSelector orientation

-- | @- changeLayoutOrientation:@
changeLayoutOrientation :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
changeLayoutOrientation nsTextView sender =
  sendMessage nsTextView changeLayoutOrientationSelector sender

-- | *********************** Helper for subclassers ************************
--
-- ObjC selector: @- characterIndexForInsertionAtPoint:@
characterIndexForInsertionAtPoint :: IsNSTextView nsTextView => nsTextView -> NSPoint -> IO CULong
characterIndexForInsertionAtPoint nsTextView point =
  sendMessage nsTextView characterIndexForInsertionAtPointSelector point

-- | @- performValidatedReplacementInRange:withAttributedString:@
performValidatedReplacementInRange_withAttributedString :: (IsNSTextView nsTextView, IsNSAttributedString attributedString) => nsTextView -> NSRange -> attributedString -> IO Bool
performValidatedReplacementInRange_withAttributedString nsTextView range attributedString =
  sendMessage nsTextView performValidatedReplacementInRange_withAttributedStringSelector range (toNSAttributedString attributedString)

-- | @- toggleBaseWritingDirection:@
toggleBaseWritingDirection :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleBaseWritingDirection nsTextView sender =
  sendMessage nsTextView toggleBaseWritingDirectionSelector sender

-- | @- drawTextHighlightBackgroundForTextRange:origin:@
drawTextHighlightBackgroundForTextRange_origin :: (IsNSTextView nsTextView, IsNSTextRange textRange) => nsTextView -> textRange -> NSPoint -> IO ()
drawTextHighlightBackgroundForTextRange_origin nsTextView textRange origin =
  sendMessage nsTextView drawTextHighlightBackgroundForTextRange_originSelector (toNSTextRange textRange) origin

-- | An action for toggling @NSTextHighlightStyleAttributeName@ in the receiver’s selected range. The sender should be a menu item with a @representedObject@ of type (@NSTextHighlightColorScheme@).
--
-- ObjC selector: @- highlight:@
highlight :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
highlight nsTextView sender =
  sendMessage nsTextView highlightSelector sender

-- | @+ scrollableTextView@
scrollableTextView :: IO (Id NSScrollView)
scrollableTextView  =
  do
    cls' <- getRequiredClass "NSTextView"
    sendClassMessage cls' scrollableTextViewSelector

-- | @+ fieldEditor@
nsTextViewFieldEditor :: IO (Id NSTextView)
nsTextViewFieldEditor  =
  do
    cls' <- getRequiredClass "NSTextView"
    sendClassMessage cls' nsTextViewFieldEditorSelector

-- | @+ scrollableDocumentContentTextView@
scrollableDocumentContentTextView :: IO (Id NSScrollView)
scrollableDocumentContentTextView  =
  do
    cls' <- getRequiredClass "NSTextView"
    sendClassMessage cls' scrollableDocumentContentTextViewSelector

-- | @+ scrollablePlainDocumentContentTextView@
scrollablePlainDocumentContentTextView :: IO (Id NSScrollView)
scrollablePlainDocumentContentTextView  =
  do
    cls' <- getRequiredClass "NSTextView"
    sendClassMessage cls' scrollablePlainDocumentContentTextViewSelector

-- | @- toggleAutomaticTextCompletion:@
toggleAutomaticTextCompletion :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleAutomaticTextCompletion nsTextView sender =
  sendMessage nsTextView toggleAutomaticTextCompletionSelector sender

-- | @- updateTouchBarItemIdentifiers@
updateTouchBarItemIdentifiers :: IsNSTextView nsTextView => nsTextView -> IO ()
updateTouchBarItemIdentifiers nsTextView =
  sendMessage nsTextView updateTouchBarItemIdentifiersSelector

-- | @- updateTextTouchBarItems@
updateTextTouchBarItems :: IsNSTextView nsTextView => nsTextView -> IO ()
updateTextTouchBarItems nsTextView =
  sendMessage nsTextView updateTextTouchBarItemsSelector

-- | @- updateCandidates@
updateCandidates :: IsNSTextView nsTextView => nsTextView -> IO ()
updateCandidates nsTextView =
  sendMessage nsTextView updateCandidatesSelector

-- | ************************* NSSharingService support **************************
--
-- ObjC selector: @- orderFrontSharingServicePicker:@
orderFrontSharingServicePicker :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
orderFrontSharingServicePicker nsTextView sender =
  sendMessage nsTextView orderFrontSharingServicePickerSelector sender

-- | ************************* Quick Look support **************************
--
-- ObjC selector: @- toggleQuickLookPreviewPanel:@
toggleQuickLookPreviewPanel :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleQuickLookPreviewPanel nsTextView sender =
  sendMessage nsTextView toggleQuickLookPreviewPanelSelector sender

-- | @- quickLookPreviewableItemsInRanges:@
quickLookPreviewableItemsInRanges :: (IsNSTextView nsTextView, IsNSArray ranges) => nsTextView -> ranges -> IO (Id NSArray)
quickLookPreviewableItemsInRanges nsTextView ranges =
  sendMessage nsTextView quickLookPreviewableItemsInRangesSelector (toNSArray ranges)

-- | @- updateQuickLookPreviewPanel@
updateQuickLookPreviewPanel :: IsNSTextView nsTextView => nsTextView -> IO ()
updateQuickLookPreviewPanel nsTextView =
  sendMessage nsTextView updateQuickLookPreviewPanelSelector

-- | @- smartDeleteRangeForProposedRange:@
smartDeleteRangeForProposedRange :: IsNSTextView nsTextView => nsTextView -> NSRange -> IO NSRange
smartDeleteRangeForProposedRange nsTextView proposedCharRange =
  sendMessage nsTextView smartDeleteRangeForProposedRangeSelector proposedCharRange

-- | @- toggleSmartInsertDelete:@
toggleSmartInsertDelete :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleSmartInsertDelete nsTextView sender =
  sendMessage nsTextView toggleSmartInsertDeleteSelector sender

-- | @- smartInsertForString:replacingRange:beforeString:afterString:@
smartInsertForString_replacingRange_beforeString_afterString :: (IsNSTextView nsTextView, IsNSString pasteString, IsNSString beforeString, IsNSString afterString) => nsTextView -> pasteString -> NSRange -> beforeString -> afterString -> IO ()
smartInsertForString_replacingRange_beforeString_afterString nsTextView pasteString charRangeToReplace beforeString afterString =
  sendMessage nsTextView smartInsertForString_replacingRange_beforeString_afterStringSelector (toNSString pasteString) charRangeToReplace (toNSString beforeString) (toNSString afterString)

-- | @- smartInsertBeforeStringForString:replacingRange:@
smartInsertBeforeStringForString_replacingRange :: (IsNSTextView nsTextView, IsNSString pasteString) => nsTextView -> pasteString -> NSRange -> IO (Id NSString)
smartInsertBeforeStringForString_replacingRange nsTextView pasteString charRangeToReplace =
  sendMessage nsTextView smartInsertBeforeStringForString_replacingRangeSelector (toNSString pasteString) charRangeToReplace

-- | @- smartInsertAfterStringForString:replacingRange:@
smartInsertAfterStringForString_replacingRange :: (IsNSTextView nsTextView, IsNSString pasteString) => nsTextView -> pasteString -> NSRange -> IO (Id NSString)
smartInsertAfterStringForString_replacingRange nsTextView pasteString charRangeToReplace =
  sendMessage nsTextView smartInsertAfterStringForString_replacingRangeSelector (toNSString pasteString) charRangeToReplace

-- | @- toggleAutomaticQuoteSubstitution:@
toggleAutomaticQuoteSubstitution :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleAutomaticQuoteSubstitution nsTextView sender =
  sendMessage nsTextView toggleAutomaticQuoteSubstitutionSelector sender

-- | @- toggleAutomaticLinkDetection:@
toggleAutomaticLinkDetection :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleAutomaticLinkDetection nsTextView sender =
  sendMessage nsTextView toggleAutomaticLinkDetectionSelector sender

-- | @- toggleAutomaticDataDetection:@
toggleAutomaticDataDetection :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleAutomaticDataDetection nsTextView sender =
  sendMessage nsTextView toggleAutomaticDataDetectionSelector sender

-- | @- toggleAutomaticDashSubstitution:@
toggleAutomaticDashSubstitution :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleAutomaticDashSubstitution nsTextView sender =
  sendMessage nsTextView toggleAutomaticDashSubstitutionSelector sender

-- | @- toggleAutomaticTextReplacement:@
toggleAutomaticTextReplacement :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleAutomaticTextReplacement nsTextView sender =
  sendMessage nsTextView toggleAutomaticTextReplacementSelector sender

-- | @- toggleAutomaticSpellingCorrection:@
toggleAutomaticSpellingCorrection :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleAutomaticSpellingCorrection nsTextView sender =
  sendMessage nsTextView toggleAutomaticSpellingCorrectionSelector sender

-- | @- checkTextInRange:types:options:@
checkTextInRange_types_options :: (IsNSTextView nsTextView, IsNSDictionary options) => nsTextView -> NSRange -> CULong -> options -> IO ()
checkTextInRange_types_options nsTextView range checkingTypes options =
  sendMessage nsTextView checkTextInRange_types_optionsSelector range checkingTypes (toNSDictionary options)

-- | @- handleTextCheckingResults:forRange:types:options:orthography:wordCount:@
handleTextCheckingResults_forRange_types_options_orthography_wordCount :: (IsNSTextView nsTextView, IsNSArray results, IsNSDictionary options, IsNSOrthography orthography) => nsTextView -> results -> NSRange -> CULong -> options -> orthography -> CLong -> IO ()
handleTextCheckingResults_forRange_types_options_orthography_wordCount nsTextView results range checkingTypes options orthography wordCount =
  sendMessage nsTextView handleTextCheckingResults_forRange_types_options_orthography_wordCountSelector (toNSArray results) range checkingTypes (toNSDictionary options) (toNSOrthography orthography) wordCount

-- | @- orderFrontSubstitutionsPanel:@
orderFrontSubstitutionsPanel :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
orderFrontSubstitutionsPanel nsTextView sender =
  sendMessage nsTextView orderFrontSubstitutionsPanelSelector sender

-- | @- checkTextInSelection:@
checkTextInSelection :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
checkTextInSelection nsTextView sender =
  sendMessage nsTextView checkTextInSelectionSelector sender

-- | @- checkTextInDocument:@
checkTextInDocument :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
checkTextInDocument nsTextView sender =
  sendMessage nsTextView checkTextInDocumentSelector sender

-- | @- setSelectedRanges:affinity:stillSelecting:@
setSelectedRanges_affinity_stillSelecting :: (IsNSTextView nsTextView, IsNSArray ranges) => nsTextView -> ranges -> NSSelectionAffinity -> Bool -> IO ()
setSelectedRanges_affinity_stillSelecting nsTextView ranges affinity stillSelectingFlag =
  sendMessage nsTextView setSelectedRanges_affinity_stillSelectingSelector (toNSArray ranges) affinity stillSelectingFlag

-- | @- setSelectedRange:affinity:stillSelecting:@
setSelectedRange_affinity_stillSelecting :: IsNSTextView nsTextView => nsTextView -> NSRange -> NSSelectionAffinity -> Bool -> IO ()
setSelectedRange_affinity_stillSelecting nsTextView charRange affinity stillSelectingFlag =
  sendMessage nsTextView setSelectedRange_affinity_stillSelectingSelector charRange affinity stillSelectingFlag

-- | @- updateInsertionPointStateAndRestartTimer:@
updateInsertionPointStateAndRestartTimer :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
updateInsertionPointStateAndRestartTimer nsTextView restartFlag =
  sendMessage nsTextView updateInsertionPointStateAndRestartTimerSelector restartFlag

-- | @- toggleContinuousSpellChecking:@
toggleContinuousSpellChecking :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleContinuousSpellChecking nsTextView sender =
  sendMessage nsTextView toggleContinuousSpellCheckingSelector sender

-- | @- toggleGrammarChecking:@
toggleGrammarChecking :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
toggleGrammarChecking nsTextView sender =
  sendMessage nsTextView toggleGrammarCheckingSelector sender

-- | @- setSpellingState:range:@
setSpellingState_range :: IsNSTextView nsTextView => nsTextView -> CLong -> NSRange -> IO ()
setSpellingState_range nsTextView value charRange =
  sendMessage nsTextView setSpellingState_rangeSelector value charRange

-- | @- shouldChangeTextInRanges:replacementStrings:@
shouldChangeTextInRanges_replacementStrings :: (IsNSTextView nsTextView, IsNSArray affectedRanges, IsNSArray replacementStrings) => nsTextView -> affectedRanges -> replacementStrings -> IO Bool
shouldChangeTextInRanges_replacementStrings nsTextView affectedRanges replacementStrings =
  sendMessage nsTextView shouldChangeTextInRanges_replacementStringsSelector (toNSArray affectedRanges) (toNSArray replacementStrings)

-- | @- shouldChangeTextInRange:replacementString:@
shouldChangeTextInRange_replacementString :: (IsNSTextView nsTextView, IsNSString replacementString) => nsTextView -> NSRange -> replacementString -> IO Bool
shouldChangeTextInRange_replacementString nsTextView affectedCharRange replacementString =
  sendMessage nsTextView shouldChangeTextInRange_replacementStringSelector affectedCharRange (toNSString replacementString)

-- | @- didChangeText@
didChangeText :: IsNSTextView nsTextView => nsTextView -> IO ()
didChangeText nsTextView =
  sendMessage nsTextView didChangeTextSelector

-- | @- breakUndoCoalescing@
breakUndoCoalescing :: IsNSTextView nsTextView => nsTextView -> IO ()
breakUndoCoalescing nsTextView =
  sendMessage nsTextView breakUndoCoalescingSelector

-- | @- showFindIndicatorForRange:@
showFindIndicatorForRange :: IsNSTextView nsTextView => nsTextView -> NSRange -> IO ()
showFindIndicatorForRange nsTextView charRange =
  sendMessage nsTextView showFindIndicatorForRangeSelector charRange

-- | @- setSelectedRange:@
setSelectedRange :: IsNSTextView nsTextView => nsTextView -> NSRange -> IO ()
setSelectedRange nsTextView charRange =
  sendMessage nsTextView setSelectedRangeSelector charRange

-- | @- dragSelectionWithEvent:offset:slideBack:@
dragSelectionWithEvent_offset_slideBack :: (IsNSTextView nsTextView, IsNSEvent event) => nsTextView -> event -> NSSize -> Bool -> IO Bool
dragSelectionWithEvent_offset_slideBack nsTextView event mouseOffset slideBack =
  sendMessage nsTextView dragSelectionWithEvent_offset_slideBackSelector (toNSEvent event) mouseOffset slideBack

-- | @- dragImageForSelectionWithEvent:origin:@
dragImageForSelectionWithEvent_origin :: (IsNSTextView nsTextView, IsNSEvent event) => nsTextView -> event -> Ptr NSPoint -> IO (Id NSImage)
dragImageForSelectionWithEvent_origin nsTextView event origin =
  sendMessage nsTextView dragImageForSelectionWithEvent_originSelector (toNSEvent event) origin

-- | @- dragOperationForDraggingInfo:type:@
dragOperationForDraggingInfo_type :: (IsNSTextView nsTextView, IsNSString type_) => nsTextView -> RawId -> type_ -> IO NSDragOperation
dragOperationForDraggingInfo_type nsTextView dragInfo type_ =
  sendMessage nsTextView dragOperationForDraggingInfo_typeSelector dragInfo (toNSString type_)

-- | @- cleanUpAfterDragOperation@
cleanUpAfterDragOperation :: IsNSTextView nsTextView => nsTextView -> IO ()
cleanUpAfterDragOperation nsTextView =
  sendMessage nsTextView cleanUpAfterDragOperationSelector

-- | @- writeSelectionToPasteboard:type:@
writeSelectionToPasteboard_type :: (IsNSTextView nsTextView, IsNSPasteboard pboard, IsNSString type_) => nsTextView -> pboard -> type_ -> IO Bool
writeSelectionToPasteboard_type nsTextView pboard type_ =
  sendMessage nsTextView writeSelectionToPasteboard_typeSelector (toNSPasteboard pboard) (toNSString type_)

-- | @- writeSelectionToPasteboard:types:@
writeSelectionToPasteboard_types :: (IsNSTextView nsTextView, IsNSPasteboard pboard, IsNSArray types) => nsTextView -> pboard -> types -> IO Bool
writeSelectionToPasteboard_types nsTextView pboard types =
  sendMessage nsTextView writeSelectionToPasteboard_typesSelector (toNSPasteboard pboard) (toNSArray types)

-- | @- preferredPasteboardTypeFromArray:restrictedToTypesFromArray:@
preferredPasteboardTypeFromArray_restrictedToTypesFromArray :: (IsNSTextView nsTextView, IsNSArray availableTypes, IsNSArray allowedTypes) => nsTextView -> availableTypes -> allowedTypes -> IO (Id NSString)
preferredPasteboardTypeFromArray_restrictedToTypesFromArray nsTextView availableTypes allowedTypes =
  sendMessage nsTextView preferredPasteboardTypeFromArray_restrictedToTypesFromArraySelector (toNSArray availableTypes) (toNSArray allowedTypes)

-- | @- readSelectionFromPasteboard:type:@
readSelectionFromPasteboard_type :: (IsNSTextView nsTextView, IsNSPasteboard pboard, IsNSString type_) => nsTextView -> pboard -> type_ -> IO Bool
readSelectionFromPasteboard_type nsTextView pboard type_ =
  sendMessage nsTextView readSelectionFromPasteboard_typeSelector (toNSPasteboard pboard) (toNSString type_)

-- | @- readSelectionFromPasteboard:@
readSelectionFromPasteboard :: (IsNSTextView nsTextView, IsNSPasteboard pboard) => nsTextView -> pboard -> IO Bool
readSelectionFromPasteboard nsTextView pboard =
  sendMessage nsTextView readSelectionFromPasteboardSelector (toNSPasteboard pboard)

-- | @+ registerForServices@
registerForServices :: IO ()
registerForServices  =
  do
    cls' <- getRequiredClass "NSTextView"
    sendClassMessage cls' registerForServicesSelector

-- | @- validRequestorForSendType:returnType:@
validRequestorForSendType_returnType :: (IsNSTextView nsTextView, IsNSString sendType, IsNSString returnType) => nsTextView -> sendType -> returnType -> IO RawId
validRequestorForSendType_returnType nsTextView sendType returnType =
  sendMessage nsTextView validRequestorForSendType_returnTypeSelector (toNSString sendType) (toNSString returnType)

-- | @- pasteAsPlainText:@
pasteAsPlainText :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
pasteAsPlainText nsTextView sender =
  sendMessage nsTextView pasteAsPlainTextSelector sender

-- | @- pasteAsRichText:@
pasteAsRichText :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
pasteAsRichText nsTextView sender =
  sendMessage nsTextView pasteAsRichTextSelector sender

-- | *********************** Completion support ********************
--
-- ObjC selector: @- complete:@
complete :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
complete nsTextView sender =
  sendMessage nsTextView completeSelector sender

-- | @- completionsForPartialWordRange:indexOfSelectedItem:@
completionsForPartialWordRange_indexOfSelectedItem :: IsNSTextView nsTextView => nsTextView -> NSRange -> Ptr CLong -> IO (Id NSArray)
completionsForPartialWordRange_indexOfSelectedItem nsTextView charRange index =
  sendMessage nsTextView completionsForPartialWordRange_indexOfSelectedItemSelector charRange index

-- | @- insertCompletion:forPartialWordRange:movement:isFinal:@
insertCompletion_forPartialWordRange_movement_isFinal :: (IsNSTextView nsTextView, IsNSString word) => nsTextView -> word -> NSRange -> CLong -> Bool -> IO ()
insertCompletion_forPartialWordRange_movement_isFinal nsTextView word charRange movement flag =
  sendMessage nsTextView insertCompletion_forPartialWordRange_movement_isFinalSelector (toNSString word) charRange movement flag

-- | *************** Get/Set the container and other stuff ****************
--
-- ObjC selector: @- textContainer@
textContainer :: IsNSTextView nsTextView => nsTextView -> IO (Id NSTextContainer)
textContainer nsTextView =
  sendMessage nsTextView textContainerSelector

-- | *************** Get/Set the container and other stuff ****************
--
-- ObjC selector: @- setTextContainer:@
setTextContainer :: (IsNSTextView nsTextView, IsNSTextContainer value) => nsTextView -> value -> IO ()
setTextContainer nsTextView value =
  sendMessage nsTextView setTextContainerSelector (toNSTextContainer value)

-- | @- textContainerInset@
textContainerInset :: IsNSTextView nsTextView => nsTextView -> IO NSSize
textContainerInset nsTextView =
  sendMessage nsTextView textContainerInsetSelector

-- | @- setTextContainerInset:@
setTextContainerInset :: IsNSTextView nsTextView => nsTextView -> NSSize -> IO ()
setTextContainerInset nsTextView value =
  sendMessage nsTextView setTextContainerInsetSelector value

-- | @- textContainerOrigin@
textContainerOrigin :: IsNSTextView nsTextView => nsTextView -> IO NSPoint
textContainerOrigin nsTextView =
  sendMessage nsTextView textContainerOriginSelector

-- | @- layoutManager@
layoutManager :: IsNSTextView nsTextView => nsTextView -> IO (Id NSLayoutManager)
layoutManager nsTextView =
  sendMessage nsTextView layoutManagerSelector

-- | @- textStorage@
textStorage :: IsNSTextView nsTextView => nsTextView -> IO (Id NSTextStorage)
textStorage nsTextView =
  sendMessage nsTextView textStorageSelector

-- | @- textLayoutManager@
textLayoutManager :: IsNSTextView nsTextView => nsTextView -> IO (Id NSTextLayoutManager)
textLayoutManager nsTextView =
  sendMessage nsTextView textLayoutManagerSelector

-- | @- textContentStorage@
textContentStorage :: IsNSTextView nsTextView => nsTextView -> IO (Id NSTextContentStorage)
textContentStorage nsTextView =
  sendMessage nsTextView textContentStorageSelector

-- | @- shouldDrawInsertionPoint@
shouldDrawInsertionPoint :: IsNSTextView nsTextView => nsTextView -> IO Bool
shouldDrawInsertionPoint nsTextView =
  sendMessage nsTextView shouldDrawInsertionPointSelector

-- | ************************** Ownership policy ***************************
--
-- ObjC selector: @+ stronglyReferencesTextStorage@
stronglyReferencesTextStorage :: IO Bool
stronglyReferencesTextStorage  =
  do
    cls' <- getRequiredClass "NSTextView"
    sendClassMessage cls' stronglyReferencesTextStorageSelector

-- | @- usesAdaptiveColorMappingForDarkAppearance@
usesAdaptiveColorMappingForDarkAppearance :: IsNSTextView nsTextView => nsTextView -> IO Bool
usesAdaptiveColorMappingForDarkAppearance nsTextView =
  sendMessage nsTextView usesAdaptiveColorMappingForDarkAppearanceSelector

-- | @- setUsesAdaptiveColorMappingForDarkAppearance:@
setUsesAdaptiveColorMappingForDarkAppearance :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setUsesAdaptiveColorMappingForDarkAppearance nsTextView value =
  sendMessage nsTextView setUsesAdaptiveColorMappingForDarkAppearanceSelector value

-- | ************************* Text Highlight  support **************************
--
-- ObjC selector: @- textHighlightAttributes@
textHighlightAttributes :: IsNSTextView nsTextView => nsTextView -> IO (Id NSDictionary)
textHighlightAttributes nsTextView =
  sendMessage nsTextView textHighlightAttributesSelector

-- | ************************* Text Highlight  support **************************
--
-- ObjC selector: @- setTextHighlightAttributes:@
setTextHighlightAttributes :: (IsNSTextView nsTextView, IsNSDictionary value) => nsTextView -> value -> IO ()
setTextHighlightAttributes nsTextView value =
  sendMessage nsTextView setTextHighlightAttributesSelector (toNSDictionary value)

-- | @- automaticTextCompletionEnabled@
automaticTextCompletionEnabled :: IsNSTextView nsTextView => nsTextView -> IO Bool
automaticTextCompletionEnabled nsTextView =
  sendMessage nsTextView automaticTextCompletionEnabledSelector

-- | @- setAutomaticTextCompletionEnabled:@
setAutomaticTextCompletionEnabled :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAutomaticTextCompletionEnabled nsTextView value =
  sendMessage nsTextView setAutomaticTextCompletionEnabledSelector value

-- | @- allowsCharacterPickerTouchBarItem@
allowsCharacterPickerTouchBarItem :: IsNSTextView nsTextView => nsTextView -> IO Bool
allowsCharacterPickerTouchBarItem nsTextView =
  sendMessage nsTextView allowsCharacterPickerTouchBarItemSelector

-- | @- setAllowsCharacterPickerTouchBarItem:@
setAllowsCharacterPickerTouchBarItem :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAllowsCharacterPickerTouchBarItem nsTextView value =
  sendMessage nsTextView setAllowsCharacterPickerTouchBarItemSelector value

-- | @- candidateListTouchBarItem@
candidateListTouchBarItem :: IsNSTextView nsTextView => nsTextView -> IO (Id NSCandidateListTouchBarItem)
candidateListTouchBarItem nsTextView =
  sendMessage nsTextView candidateListTouchBarItemSelector

-- | ************************* Smart copy/paste/delete/substitution support **************************
--
-- ObjC selector: @- smartInsertDeleteEnabled@
smartInsertDeleteEnabled :: IsNSTextView nsTextView => nsTextView -> IO Bool
smartInsertDeleteEnabled nsTextView =
  sendMessage nsTextView smartInsertDeleteEnabledSelector

-- | ************************* Smart copy/paste/delete/substitution support **************************
--
-- ObjC selector: @- setSmartInsertDeleteEnabled:@
setSmartInsertDeleteEnabled :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setSmartInsertDeleteEnabled nsTextView value =
  sendMessage nsTextView setSmartInsertDeleteEnabledSelector value

-- | @- automaticQuoteSubstitutionEnabled@
automaticQuoteSubstitutionEnabled :: IsNSTextView nsTextView => nsTextView -> IO Bool
automaticQuoteSubstitutionEnabled nsTextView =
  sendMessage nsTextView automaticQuoteSubstitutionEnabledSelector

-- | @- setAutomaticQuoteSubstitutionEnabled:@
setAutomaticQuoteSubstitutionEnabled :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAutomaticQuoteSubstitutionEnabled nsTextView value =
  sendMessage nsTextView setAutomaticQuoteSubstitutionEnabledSelector value

-- | @- automaticLinkDetectionEnabled@
automaticLinkDetectionEnabled :: IsNSTextView nsTextView => nsTextView -> IO Bool
automaticLinkDetectionEnabled nsTextView =
  sendMessage nsTextView automaticLinkDetectionEnabledSelector

-- | @- setAutomaticLinkDetectionEnabled:@
setAutomaticLinkDetectionEnabled :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAutomaticLinkDetectionEnabled nsTextView value =
  sendMessage nsTextView setAutomaticLinkDetectionEnabledSelector value

-- | @- automaticDataDetectionEnabled@
automaticDataDetectionEnabled :: IsNSTextView nsTextView => nsTextView -> IO Bool
automaticDataDetectionEnabled nsTextView =
  sendMessage nsTextView automaticDataDetectionEnabledSelector

-- | @- setAutomaticDataDetectionEnabled:@
setAutomaticDataDetectionEnabled :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAutomaticDataDetectionEnabled nsTextView value =
  sendMessage nsTextView setAutomaticDataDetectionEnabledSelector value

-- | @- automaticDashSubstitutionEnabled@
automaticDashSubstitutionEnabled :: IsNSTextView nsTextView => nsTextView -> IO Bool
automaticDashSubstitutionEnabled nsTextView =
  sendMessage nsTextView automaticDashSubstitutionEnabledSelector

-- | @- setAutomaticDashSubstitutionEnabled:@
setAutomaticDashSubstitutionEnabled :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAutomaticDashSubstitutionEnabled nsTextView value =
  sendMessage nsTextView setAutomaticDashSubstitutionEnabledSelector value

-- | @- automaticTextReplacementEnabled@
automaticTextReplacementEnabled :: IsNSTextView nsTextView => nsTextView -> IO Bool
automaticTextReplacementEnabled nsTextView =
  sendMessage nsTextView automaticTextReplacementEnabledSelector

-- | @- setAutomaticTextReplacementEnabled:@
setAutomaticTextReplacementEnabled :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAutomaticTextReplacementEnabled nsTextView value =
  sendMessage nsTextView setAutomaticTextReplacementEnabledSelector value

-- | @- automaticSpellingCorrectionEnabled@
automaticSpellingCorrectionEnabled :: IsNSTextView nsTextView => nsTextView -> IO Bool
automaticSpellingCorrectionEnabled nsTextView =
  sendMessage nsTextView automaticSpellingCorrectionEnabledSelector

-- | @- setAutomaticSpellingCorrectionEnabled:@
setAutomaticSpellingCorrectionEnabled :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAutomaticSpellingCorrectionEnabled nsTextView value =
  sendMessage nsTextView setAutomaticSpellingCorrectionEnabledSelector value

-- | @- enabledTextCheckingTypes@
enabledTextCheckingTypes :: IsNSTextView nsTextView => nsTextView -> IO CULong
enabledTextCheckingTypes nsTextView =
  sendMessage nsTextView enabledTextCheckingTypesSelector

-- | @- setEnabledTextCheckingTypes:@
setEnabledTextCheckingTypes :: IsNSTextView nsTextView => nsTextView -> CULong -> IO ()
setEnabledTextCheckingTypes nsTextView value =
  sendMessage nsTextView setEnabledTextCheckingTypesSelector value

-- | @- usesFindPanel@
usesFindPanel :: IsNSTextView nsTextView => nsTextView -> IO Bool
usesFindPanel nsTextView =
  sendMessage nsTextView usesFindPanelSelector

-- | @- setUsesFindPanel:@
setUsesFindPanel :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setUsesFindPanel nsTextView value =
  sendMessage nsTextView setUsesFindPanelSelector value

-- | @- usesFindBar@
usesFindBar :: IsNSTextView nsTextView => nsTextView -> IO Bool
usesFindBar nsTextView =
  sendMessage nsTextView usesFindBarSelector

-- | @- setUsesFindBar:@
setUsesFindBar :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setUsesFindBar nsTextView value =
  sendMessage nsTextView setUsesFindBarSelector value

-- | @- incrementalSearchingEnabled@
incrementalSearchingEnabled :: IsNSTextView nsTextView => nsTextView -> IO Bool
incrementalSearchingEnabled nsTextView =
  sendMessage nsTextView incrementalSearchingEnabledSelector

-- | @- setIncrementalSearchingEnabled:@
setIncrementalSearchingEnabled :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setIncrementalSearchingEnabled nsTextView value =
  sendMessage nsTextView setIncrementalSearchingEnabledSelector value

-- | @- inlinePredictionType@
inlinePredictionType :: IsNSTextView nsTextView => nsTextView -> IO NSTextInputTraitType
inlinePredictionType nsTextView =
  sendMessage nsTextView inlinePredictionTypeSelector

-- | @- setInlinePredictionType:@
setInlinePredictionType :: IsNSTextView nsTextView => nsTextView -> NSTextInputTraitType -> IO ()
setInlinePredictionType nsTextView value =
  sendMessage nsTextView setInlinePredictionTypeSelector value

-- | @- mathExpressionCompletionType@
mathExpressionCompletionType :: IsNSTextView nsTextView => nsTextView -> IO NSTextInputTraitType
mathExpressionCompletionType nsTextView =
  sendMessage nsTextView mathExpressionCompletionTypeSelector

-- | @- setMathExpressionCompletionType:@
setMathExpressionCompletionType :: IsNSTextView nsTextView => nsTextView -> NSTextInputTraitType -> IO ()
setMathExpressionCompletionType nsTextView value =
  sendMessage nsTextView setMathExpressionCompletionTypeSelector value

-- | ************************* Selected/Marked range **************************
--
-- ObjC selector: @- selectedRanges@
selectedRanges :: IsNSTextView nsTextView => nsTextView -> IO (Id NSArray)
selectedRanges nsTextView =
  sendMessage nsTextView selectedRangesSelector

-- | ************************* Selected/Marked range **************************
--
-- ObjC selector: @- setSelectedRanges:@
setSelectedRanges :: (IsNSTextView nsTextView, IsNSArray value) => nsTextView -> value -> IO ()
setSelectedRanges nsTextView value =
  sendMessage nsTextView setSelectedRangesSelector (toNSArray value)

-- | @- selectionAffinity@
selectionAffinity :: IsNSTextView nsTextView => nsTextView -> IO NSSelectionAffinity
selectionAffinity nsTextView =
  sendMessage nsTextView selectionAffinitySelector

-- | @- selectionGranularity@
selectionGranularity :: IsNSTextView nsTextView => nsTextView -> IO NSSelectionGranularity
selectionGranularity nsTextView =
  sendMessage nsTextView selectionGranularitySelector

-- | @- setSelectionGranularity:@
setSelectionGranularity :: IsNSTextView nsTextView => nsTextView -> NSSelectionGranularity -> IO ()
setSelectionGranularity nsTextView value =
  sendMessage nsTextView setSelectionGranularitySelector value

-- | @- selectedTextAttributes@
selectedTextAttributes :: IsNSTextView nsTextView => nsTextView -> IO (Id NSDictionary)
selectedTextAttributes nsTextView =
  sendMessage nsTextView selectedTextAttributesSelector

-- | @- setSelectedTextAttributes:@
setSelectedTextAttributes :: (IsNSTextView nsTextView, IsNSDictionary value) => nsTextView -> value -> IO ()
setSelectedTextAttributes nsTextView value =
  sendMessage nsTextView setSelectedTextAttributesSelector (toNSDictionary value)

-- | @- insertionPointColor@
insertionPointColor :: IsNSTextView nsTextView => nsTextView -> IO (Id NSColor)
insertionPointColor nsTextView =
  sendMessage nsTextView insertionPointColorSelector

-- | @- setInsertionPointColor:@
setInsertionPointColor :: (IsNSTextView nsTextView, IsNSColor value) => nsTextView -> value -> IO ()
setInsertionPointColor nsTextView value =
  sendMessage nsTextView setInsertionPointColorSelector (toNSColor value)

-- | @- markedTextAttributes@
markedTextAttributes :: IsNSTextView nsTextView => nsTextView -> IO (Id NSDictionary)
markedTextAttributes nsTextView =
  sendMessage nsTextView markedTextAttributesSelector

-- | @- setMarkedTextAttributes:@
setMarkedTextAttributes :: (IsNSTextView nsTextView, IsNSDictionary value) => nsTextView -> value -> IO ()
setMarkedTextAttributes nsTextView value =
  sendMessage nsTextView setMarkedTextAttributesSelector (toNSDictionary value)

-- | @- linkTextAttributes@
linkTextAttributes :: IsNSTextView nsTextView => nsTextView -> IO (Id NSDictionary)
linkTextAttributes nsTextView =
  sendMessage nsTextView linkTextAttributesSelector

-- | @- setLinkTextAttributes:@
setLinkTextAttributes :: (IsNSTextView nsTextView, IsNSDictionary value) => nsTextView -> value -> IO ()
setLinkTextAttributes nsTextView value =
  sendMessage nsTextView setLinkTextAttributesSelector (toNSDictionary value)

-- | @- displaysLinkToolTips@
displaysLinkToolTips :: IsNSTextView nsTextView => nsTextView -> IO Bool
displaysLinkToolTips nsTextView =
  sendMessage nsTextView displaysLinkToolTipsSelector

-- | @- setDisplaysLinkToolTips:@
setDisplaysLinkToolTips :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setDisplaysLinkToolTips nsTextView value =
  sendMessage nsTextView setDisplaysLinkToolTipsSelector value

-- | *********************** Glyph info support ************************
--
-- ObjC selector: @- acceptsGlyphInfo@
acceptsGlyphInfo :: IsNSTextView nsTextView => nsTextView -> IO Bool
acceptsGlyphInfo nsTextView =
  sendMessage nsTextView acceptsGlyphInfoSelector

-- | *********************** Glyph info support ************************
--
-- ObjC selector: @- setAcceptsGlyphInfo:@
setAcceptsGlyphInfo :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAcceptsGlyphInfo nsTextView value =
  sendMessage nsTextView setAcceptsGlyphInfoSelector value

-- | ************************* Other NSTextView methods **************************
--
-- ObjC selector: @- usesRuler@
usesRuler :: IsNSTextView nsTextView => nsTextView -> IO Bool
usesRuler nsTextView =
  sendMessage nsTextView usesRulerSelector

-- | ************************* Other NSTextView methods **************************
--
-- ObjC selector: @- setUsesRuler:@
setUsesRuler :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setUsesRuler nsTextView value =
  sendMessage nsTextView setUsesRulerSelector value

-- | @- usesInspectorBar@
usesInspectorBar :: IsNSTextView nsTextView => nsTextView -> IO Bool
usesInspectorBar nsTextView =
  sendMessage nsTextView usesInspectorBarSelector

-- | @- setUsesInspectorBar:@
setUsesInspectorBar :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setUsesInspectorBar nsTextView value =
  sendMessage nsTextView setUsesInspectorBarSelector value

-- | @- continuousSpellCheckingEnabled@
continuousSpellCheckingEnabled :: IsNSTextView nsTextView => nsTextView -> IO Bool
continuousSpellCheckingEnabled nsTextView =
  sendMessage nsTextView continuousSpellCheckingEnabledSelector

-- | @- setContinuousSpellCheckingEnabled:@
setContinuousSpellCheckingEnabled :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setContinuousSpellCheckingEnabled nsTextView value =
  sendMessage nsTextView setContinuousSpellCheckingEnabledSelector value

-- | @- spellCheckerDocumentTag@
spellCheckerDocumentTag :: IsNSTextView nsTextView => nsTextView -> IO CLong
spellCheckerDocumentTag nsTextView =
  sendMessage nsTextView spellCheckerDocumentTagSelector

-- | @- grammarCheckingEnabled@
grammarCheckingEnabled :: IsNSTextView nsTextView => nsTextView -> IO Bool
grammarCheckingEnabled nsTextView =
  sendMessage nsTextView grammarCheckingEnabledSelector

-- | @- setGrammarCheckingEnabled:@
setGrammarCheckingEnabled :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setGrammarCheckingEnabled nsTextView value =
  sendMessage nsTextView setGrammarCheckingEnabledSelector value

-- | @- typingAttributes@
typingAttributes :: IsNSTextView nsTextView => nsTextView -> IO (Id NSDictionary)
typingAttributes nsTextView =
  sendMessage nsTextView typingAttributesSelector

-- | @- setTypingAttributes:@
setTypingAttributes :: (IsNSTextView nsTextView, IsNSDictionary value) => nsTextView -> value -> IO ()
setTypingAttributes nsTextView value =
  sendMessage nsTextView setTypingAttributesSelector (toNSDictionary value)

-- | @- rangesForUserTextChange@
rangesForUserTextChange :: IsNSTextView nsTextView => nsTextView -> IO (Id NSArray)
rangesForUserTextChange nsTextView =
  sendMessage nsTextView rangesForUserTextChangeSelector

-- | @- rangesForUserCharacterAttributeChange@
rangesForUserCharacterAttributeChange :: IsNSTextView nsTextView => nsTextView -> IO (Id NSArray)
rangesForUserCharacterAttributeChange nsTextView =
  sendMessage nsTextView rangesForUserCharacterAttributeChangeSelector

-- | @- rangesForUserParagraphAttributeChange@
rangesForUserParagraphAttributeChange :: IsNSTextView nsTextView => nsTextView -> IO (Id NSArray)
rangesForUserParagraphAttributeChange nsTextView =
  sendMessage nsTextView rangesForUserParagraphAttributeChangeSelector

-- | @- rangeForUserTextChange@
rangeForUserTextChange :: IsNSTextView nsTextView => nsTextView -> IO NSRange
rangeForUserTextChange nsTextView =
  sendMessage nsTextView rangeForUserTextChangeSelector

-- | @- rangeForUserCharacterAttributeChange@
rangeForUserCharacterAttributeChange :: IsNSTextView nsTextView => nsTextView -> IO NSRange
rangeForUserCharacterAttributeChange nsTextView =
  sendMessage nsTextView rangeForUserCharacterAttributeChangeSelector

-- | @- rangeForUserParagraphAttributeChange@
rangeForUserParagraphAttributeChange :: IsNSTextView nsTextView => nsTextView -> IO NSRange
rangeForUserParagraphAttributeChange nsTextView =
  sendMessage nsTextView rangeForUserParagraphAttributeChangeSelector

-- | @- allowsDocumentBackgroundColorChange@
allowsDocumentBackgroundColorChange :: IsNSTextView nsTextView => nsTextView -> IO Bool
allowsDocumentBackgroundColorChange nsTextView =
  sendMessage nsTextView allowsDocumentBackgroundColorChangeSelector

-- | @- setAllowsDocumentBackgroundColorChange:@
setAllowsDocumentBackgroundColorChange :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAllowsDocumentBackgroundColorChange nsTextView value =
  sendMessage nsTextView setAllowsDocumentBackgroundColorChangeSelector value

-- | @- defaultParagraphStyle@
defaultParagraphStyle :: IsNSTextView nsTextView => nsTextView -> IO (Id NSParagraphStyle)
defaultParagraphStyle nsTextView =
  sendMessage nsTextView defaultParagraphStyleSelector

-- | @- setDefaultParagraphStyle:@
setDefaultParagraphStyle :: (IsNSTextView nsTextView, IsNSParagraphStyle value) => nsTextView -> value -> IO ()
setDefaultParagraphStyle nsTextView value =
  sendMessage nsTextView setDefaultParagraphStyleSelector (toNSParagraphStyle value)

-- | @- allowsUndo@
allowsUndo :: IsNSTextView nsTextView => nsTextView -> IO Bool
allowsUndo nsTextView =
  sendMessage nsTextView allowsUndoSelector

-- | @- setAllowsUndo:@
setAllowsUndo :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAllowsUndo nsTextView value =
  sendMessage nsTextView setAllowsUndoSelector value

-- | @- coalescingUndo@
coalescingUndo :: IsNSTextView nsTextView => nsTextView -> IO Bool
coalescingUndo nsTextView =
  sendMessage nsTextView coalescingUndoSelector

-- | @- allowsImageEditing@
allowsImageEditing :: IsNSTextView nsTextView => nsTextView -> IO Bool
allowsImageEditing nsTextView =
  sendMessage nsTextView allowsImageEditingSelector

-- | @- setAllowsImageEditing:@
setAllowsImageEditing :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setAllowsImageEditing nsTextView value =
  sendMessage nsTextView setAllowsImageEditingSelector value

-- | @- usesRolloverButtonForSelection@
usesRolloverButtonForSelection :: IsNSTextView nsTextView => nsTextView -> IO Bool
usesRolloverButtonForSelection nsTextView =
  sendMessage nsTextView usesRolloverButtonForSelectionSelector

-- | @- setUsesRolloverButtonForSelection:@
setUsesRolloverButtonForSelection :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setUsesRolloverButtonForSelection nsTextView value =
  sendMessage nsTextView setUsesRolloverButtonForSelectionSelector value

-- | ************************* NSText methods **************************
--
-- ObjC selector: @- delegate@
delegate :: IsNSTextView nsTextView => nsTextView -> IO RawId
delegate nsTextView =
  sendMessage nsTextView delegateSelector

-- | ************************* NSText methods **************************
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsNSTextView nsTextView => nsTextView -> RawId -> IO ()
setDelegate nsTextView value =
  sendMessage nsTextView setDelegateSelector value

-- | @- editable@
editable :: IsNSTextView nsTextView => nsTextView -> IO Bool
editable nsTextView =
  sendMessage nsTextView editableSelector

-- | @- setEditable:@
setEditable :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setEditable nsTextView value =
  sendMessage nsTextView setEditableSelector value

-- | @- selectable@
selectable :: IsNSTextView nsTextView => nsTextView -> IO Bool
selectable nsTextView =
  sendMessage nsTextView selectableSelector

-- | @- setSelectable:@
setSelectable :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setSelectable nsTextView value =
  sendMessage nsTextView setSelectableSelector value

-- | @- richText@
richText :: IsNSTextView nsTextView => nsTextView -> IO Bool
richText nsTextView =
  sendMessage nsTextView richTextSelector

-- | @- setRichText:@
setRichText :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setRichText nsTextView value =
  sendMessage nsTextView setRichTextSelector value

-- | @- importsGraphics@
importsGraphics :: IsNSTextView nsTextView => nsTextView -> IO Bool
importsGraphics nsTextView =
  sendMessage nsTextView importsGraphicsSelector

-- | @- setImportsGraphics:@
setImportsGraphics :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setImportsGraphics nsTextView value =
  sendMessage nsTextView setImportsGraphicsSelector value

-- | @- drawsBackground@
drawsBackground :: IsNSTextView nsTextView => nsTextView -> IO Bool
drawsBackground nsTextView =
  sendMessage nsTextView drawsBackgroundSelector

-- | @- setDrawsBackground:@
setDrawsBackground :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setDrawsBackground nsTextView value =
  sendMessage nsTextView setDrawsBackgroundSelector value

-- | @- backgroundColor@
backgroundColor :: IsNSTextView nsTextView => nsTextView -> IO (Id NSColor)
backgroundColor nsTextView =
  sendMessage nsTextView backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSTextView nsTextView, IsNSColor value) => nsTextView -> value -> IO ()
setBackgroundColor nsTextView value =
  sendMessage nsTextView setBackgroundColorSelector (toNSColor value)

-- | @- fieldEditor@
fieldEditor :: IsNSTextView nsTextView => nsTextView -> IO Bool
fieldEditor nsTextView =
  sendMessage nsTextView fieldEditorSelector

-- | @- setFieldEditor:@
setFieldEditor :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setFieldEditor nsTextView value =
  sendMessage nsTextView setFieldEditorSelector value

-- | @- usesFontPanel@
usesFontPanel :: IsNSTextView nsTextView => nsTextView -> IO Bool
usesFontPanel nsTextView =
  sendMessage nsTextView usesFontPanelSelector

-- | @- setUsesFontPanel:@
setUsesFontPanel :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setUsesFontPanel nsTextView value =
  sendMessage nsTextView setUsesFontPanelSelector value

-- | @- rulerVisible@
rulerVisible :: IsNSTextView nsTextView => nsTextView -> IO Bool
rulerVisible nsTextView =
  sendMessage nsTextView rulerVisibleSelector

-- | @- setRulerVisible:@
setRulerVisible :: IsNSTextView nsTextView => nsTextView -> Bool -> IO ()
setRulerVisible nsTextView value =
  sendMessage nsTextView setRulerVisibleSelector value

-- | ************************* Input Source support **************************
--
-- ObjC selector: @- allowedInputSourceLocales@
allowedInputSourceLocales :: IsNSTextView nsTextView => nsTextView -> IO (Id NSArray)
allowedInputSourceLocales nsTextView =
  sendMessage nsTextView allowedInputSourceLocalesSelector

-- | ************************* Input Source support **************************
--
-- ObjC selector: @- setAllowedInputSourceLocales:@
setAllowedInputSourceLocales :: (IsNSTextView nsTextView, IsNSArray value) => nsTextView -> value -> IO ()
setAllowedInputSourceLocales nsTextView value =
  sendMessage nsTextView setAllowedInputSourceLocalesSelector (toNSArray value)

-- | @- writingToolsActive@
writingToolsActive :: IsNSTextView nsTextView => nsTextView -> IO Bool
writingToolsActive nsTextView =
  sendMessage nsTextView writingToolsActiveSelector

-- | @- writingToolsBehavior@
writingToolsBehavior :: IsNSTextView nsTextView => nsTextView -> IO NSWritingToolsBehavior
writingToolsBehavior nsTextView =
  sendMessage nsTextView writingToolsBehaviorSelector

-- | @- setWritingToolsBehavior:@
setWritingToolsBehavior :: IsNSTextView nsTextView => nsTextView -> NSWritingToolsBehavior -> IO ()
setWritingToolsBehavior nsTextView value =
  sendMessage nsTextView setWritingToolsBehaviorSelector value

-- | @- allowedWritingToolsResultOptions@
allowedWritingToolsResultOptions :: IsNSTextView nsTextView => nsTextView -> IO NSWritingToolsResultOptions
allowedWritingToolsResultOptions nsTextView =
  sendMessage nsTextView allowedWritingToolsResultOptionsSelector

-- | @- setAllowedWritingToolsResultOptions:@
setAllowedWritingToolsResultOptions :: IsNSTextView nsTextView => nsTextView -> NSWritingToolsResultOptions -> IO ()
setAllowedWritingToolsResultOptions nsTextView value =
  sendMessage nsTextView setAllowedWritingToolsResultOptionsSelector value

-- | @- acceptableDragTypes@
acceptableDragTypes :: IsNSTextView nsTextView => nsTextView -> IO (Id NSArray)
acceptableDragTypes nsTextView =
  sendMessage nsTextView acceptableDragTypesSelector

-- | ***************** Pasteboard support (mainly for subclassers) ******************
--
-- ObjC selector: @- writablePasteboardTypes@
writablePasteboardTypes :: IsNSTextView nsTextView => nsTextView -> IO (Id NSArray)
writablePasteboardTypes nsTextView =
  sendMessage nsTextView writablePasteboardTypesSelector

-- | @- readablePasteboardTypes@
readablePasteboardTypes :: IsNSTextView nsTextView => nsTextView -> IO (Id NSArray)
readablePasteboardTypes nsTextView =
  sendMessage nsTextView readablePasteboardTypesSelector

-- | @- rangeForUserCompletion@
rangeForUserCompletion :: IsNSTextView nsTextView => nsTextView -> IO NSRange
rangeForUserCompletion nsTextView =
  sendMessage nsTextView rangeForUserCompletionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:textContainer:@
initWithFrame_textContainerSelector :: Selector '[NSRect, Id NSTextContainer] (Id NSTextView)
initWithFrame_textContainerSelector = mkSelector "initWithFrame:textContainer:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSTextView)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector '[NSRect] (Id NSTextView)
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @initUsingTextLayoutManager:@
initUsingTextLayoutManagerSelector :: Selector '[Bool] (Id NSTextView)
initUsingTextLayoutManagerSelector = mkSelector "initUsingTextLayoutManager:"

-- | @Selector@ for @textViewUsingTextLayoutManager:@
textViewUsingTextLayoutManagerSelector :: Selector '[Bool] (Id NSTextView)
textViewUsingTextLayoutManagerSelector = mkSelector "textViewUsingTextLayoutManager:"

-- | @Selector@ for @replaceTextContainer:@
replaceTextContainerSelector :: Selector '[Id NSTextContainer] ()
replaceTextContainerSelector = mkSelector "replaceTextContainer:"

-- | @Selector@ for @invalidateTextContainerOrigin@
invalidateTextContainerOriginSelector :: Selector '[] ()
invalidateTextContainerOriginSelector = mkSelector "invalidateTextContainerOrigin"

-- | @Selector@ for @insertText:@
insertTextSelector :: Selector '[RawId] ()
insertTextSelector = mkSelector "insertText:"

-- | @Selector@ for @setConstrainedFrameSize:@
setConstrainedFrameSizeSelector :: Selector '[NSSize] ()
setConstrainedFrameSizeSelector = mkSelector "setConstrainedFrameSize:"

-- | @Selector@ for @setAlignment:range:@
setAlignment_rangeSelector :: Selector '[NSTextAlignment, NSRange] ()
setAlignment_rangeSelector = mkSelector "setAlignment:range:"

-- | @Selector@ for @setBaseWritingDirection:range:@
setBaseWritingDirection_rangeSelector :: Selector '[NSWritingDirection, NSRange] ()
setBaseWritingDirection_rangeSelector = mkSelector "setBaseWritingDirection:range:"

-- | @Selector@ for @turnOffKerning:@
turnOffKerningSelector :: Selector '[RawId] ()
turnOffKerningSelector = mkSelector "turnOffKerning:"

-- | @Selector@ for @tightenKerning:@
tightenKerningSelector :: Selector '[RawId] ()
tightenKerningSelector = mkSelector "tightenKerning:"

-- | @Selector@ for @loosenKerning:@
loosenKerningSelector :: Selector '[RawId] ()
loosenKerningSelector = mkSelector "loosenKerning:"

-- | @Selector@ for @useStandardKerning:@
useStandardKerningSelector :: Selector '[RawId] ()
useStandardKerningSelector = mkSelector "useStandardKerning:"

-- | @Selector@ for @turnOffLigatures:@
turnOffLigaturesSelector :: Selector '[RawId] ()
turnOffLigaturesSelector = mkSelector "turnOffLigatures:"

-- | @Selector@ for @useStandardLigatures:@
useStandardLigaturesSelector :: Selector '[RawId] ()
useStandardLigaturesSelector = mkSelector "useStandardLigatures:"

-- | @Selector@ for @useAllLigatures:@
useAllLigaturesSelector :: Selector '[RawId] ()
useAllLigaturesSelector = mkSelector "useAllLigatures:"

-- | @Selector@ for @raiseBaseline:@
raiseBaselineSelector :: Selector '[RawId] ()
raiseBaselineSelector = mkSelector "raiseBaseline:"

-- | @Selector@ for @lowerBaseline:@
lowerBaselineSelector :: Selector '[RawId] ()
lowerBaselineSelector = mkSelector "lowerBaseline:"

-- | @Selector@ for @toggleTraditionalCharacterShape:@
toggleTraditionalCharacterShapeSelector :: Selector '[RawId] ()
toggleTraditionalCharacterShapeSelector = mkSelector "toggleTraditionalCharacterShape:"

-- | @Selector@ for @outline:@
outlineSelector :: Selector '[RawId] ()
outlineSelector = mkSelector "outline:"

-- | @Selector@ for @performFindPanelAction:@
performFindPanelActionSelector :: Selector '[RawId] ()
performFindPanelActionSelector = mkSelector "performFindPanelAction:"

-- | @Selector@ for @alignJustified:@
alignJustifiedSelector :: Selector '[RawId] ()
alignJustifiedSelector = mkSelector "alignJustified:"

-- | @Selector@ for @changeColor:@
changeColorSelector :: Selector '[RawId] ()
changeColorSelector = mkSelector "changeColor:"

-- | @Selector@ for @changeAttributes:@
changeAttributesSelector :: Selector '[RawId] ()
changeAttributesSelector = mkSelector "changeAttributes:"

-- | @Selector@ for @changeDocumentBackgroundColor:@
changeDocumentBackgroundColorSelector :: Selector '[RawId] ()
changeDocumentBackgroundColorSelector = mkSelector "changeDocumentBackgroundColor:"

-- | @Selector@ for @orderFrontSpacingPanel:@
orderFrontSpacingPanelSelector :: Selector '[RawId] ()
orderFrontSpacingPanelSelector = mkSelector "orderFrontSpacingPanel:"

-- | @Selector@ for @orderFrontLinkPanel:@
orderFrontLinkPanelSelector :: Selector '[RawId] ()
orderFrontLinkPanelSelector = mkSelector "orderFrontLinkPanel:"

-- | @Selector@ for @orderFrontListPanel:@
orderFrontListPanelSelector :: Selector '[RawId] ()
orderFrontListPanelSelector = mkSelector "orderFrontListPanel:"

-- | @Selector@ for @orderFrontTablePanel:@
orderFrontTablePanelSelector :: Selector '[RawId] ()
orderFrontTablePanelSelector = mkSelector "orderFrontTablePanel:"

-- | @Selector@ for @rulerView:didMoveMarker:@
rulerView_didMoveMarkerSelector :: Selector '[Id NSRulerView, Id NSRulerMarker] ()
rulerView_didMoveMarkerSelector = mkSelector "rulerView:didMoveMarker:"

-- | @Selector@ for @rulerView:didRemoveMarker:@
rulerView_didRemoveMarkerSelector :: Selector '[Id NSRulerView, Id NSRulerMarker] ()
rulerView_didRemoveMarkerSelector = mkSelector "rulerView:didRemoveMarker:"

-- | @Selector@ for @rulerView:didAddMarker:@
rulerView_didAddMarkerSelector :: Selector '[Id NSRulerView, Id NSRulerMarker] ()
rulerView_didAddMarkerSelector = mkSelector "rulerView:didAddMarker:"

-- | @Selector@ for @rulerView:shouldMoveMarker:@
rulerView_shouldMoveMarkerSelector :: Selector '[Id NSRulerView, Id NSRulerMarker] Bool
rulerView_shouldMoveMarkerSelector = mkSelector "rulerView:shouldMoveMarker:"

-- | @Selector@ for @rulerView:shouldAddMarker:@
rulerView_shouldAddMarkerSelector :: Selector '[Id NSRulerView, Id NSRulerMarker] Bool
rulerView_shouldAddMarkerSelector = mkSelector "rulerView:shouldAddMarker:"

-- | @Selector@ for @rulerView:willMoveMarker:toLocation:@
rulerView_willMoveMarker_toLocationSelector :: Selector '[Id NSRulerView, Id NSRulerMarker, CDouble] CDouble
rulerView_willMoveMarker_toLocationSelector = mkSelector "rulerView:willMoveMarker:toLocation:"

-- | @Selector@ for @rulerView:shouldRemoveMarker:@
rulerView_shouldRemoveMarkerSelector :: Selector '[Id NSRulerView, Id NSRulerMarker] Bool
rulerView_shouldRemoveMarkerSelector = mkSelector "rulerView:shouldRemoveMarker:"

-- | @Selector@ for @rulerView:willAddMarker:atLocation:@
rulerView_willAddMarker_atLocationSelector :: Selector '[Id NSRulerView, Id NSRulerMarker, CDouble] CDouble
rulerView_willAddMarker_atLocationSelector = mkSelector "rulerView:willAddMarker:atLocation:"

-- | @Selector@ for @rulerView:handleMouseDown:@
rulerView_handleMouseDownSelector :: Selector '[Id NSRulerView, Id NSEvent] ()
rulerView_handleMouseDownSelector = mkSelector "rulerView:handleMouseDown:"

-- | @Selector@ for @setNeedsDisplayInRect:avoidAdditionalLayout:@
setNeedsDisplayInRect_avoidAdditionalLayoutSelector :: Selector '[NSRect, Bool] ()
setNeedsDisplayInRect_avoidAdditionalLayoutSelector = mkSelector "setNeedsDisplayInRect:avoidAdditionalLayout:"

-- | @Selector@ for @drawInsertionPointInRect:color:turnedOn:@
drawInsertionPointInRect_color_turnedOnSelector :: Selector '[NSRect, Id NSColor, Bool] ()
drawInsertionPointInRect_color_turnedOnSelector = mkSelector "drawInsertionPointInRect:color:turnedOn:"

-- | @Selector@ for @drawViewBackgroundInRect:@
drawViewBackgroundInRectSelector :: Selector '[NSRect] ()
drawViewBackgroundInRectSelector = mkSelector "drawViewBackgroundInRect:"

-- | @Selector@ for @updateRuler@
updateRulerSelector :: Selector '[] ()
updateRulerSelector = mkSelector "updateRuler"

-- | @Selector@ for @updateFontPanel@
updateFontPanelSelector :: Selector '[] ()
updateFontPanelSelector = mkSelector "updateFontPanel"

-- | @Selector@ for @updateDragTypeRegistration@
updateDragTypeRegistrationSelector :: Selector '[] ()
updateDragTypeRegistrationSelector = mkSelector "updateDragTypeRegistration"

-- | @Selector@ for @selectionRangeForProposedRange:granularity:@
selectionRangeForProposedRange_granularitySelector :: Selector '[NSRange, NSSelectionGranularity] NSRange
selectionRangeForProposedRange_granularitySelector = mkSelector "selectionRangeForProposedRange:granularity:"

-- | @Selector@ for @clickedOnLink:atIndex:@
clickedOnLink_atIndexSelector :: Selector '[RawId, CULong] ()
clickedOnLink_atIndexSelector = mkSelector "clickedOnLink:atIndex:"

-- | @Selector@ for @startSpeaking:@
startSpeakingSelector :: Selector '[RawId] ()
startSpeakingSelector = mkSelector "startSpeaking:"

-- | @Selector@ for @stopSpeaking:@
stopSpeakingSelector :: Selector '[RawId] ()
stopSpeakingSelector = mkSelector "stopSpeaking:"

-- | @Selector@ for @setLayoutOrientation:@
setLayoutOrientationSelector :: Selector '[NSTextLayoutOrientation] ()
setLayoutOrientationSelector = mkSelector "setLayoutOrientation:"

-- | @Selector@ for @changeLayoutOrientation:@
changeLayoutOrientationSelector :: Selector '[RawId] ()
changeLayoutOrientationSelector = mkSelector "changeLayoutOrientation:"

-- | @Selector@ for @characterIndexForInsertionAtPoint:@
characterIndexForInsertionAtPointSelector :: Selector '[NSPoint] CULong
characterIndexForInsertionAtPointSelector = mkSelector "characterIndexForInsertionAtPoint:"

-- | @Selector@ for @performValidatedReplacementInRange:withAttributedString:@
performValidatedReplacementInRange_withAttributedStringSelector :: Selector '[NSRange, Id NSAttributedString] Bool
performValidatedReplacementInRange_withAttributedStringSelector = mkSelector "performValidatedReplacementInRange:withAttributedString:"

-- | @Selector@ for @toggleBaseWritingDirection:@
toggleBaseWritingDirectionSelector :: Selector '[RawId] ()
toggleBaseWritingDirectionSelector = mkSelector "toggleBaseWritingDirection:"

-- | @Selector@ for @drawTextHighlightBackgroundForTextRange:origin:@
drawTextHighlightBackgroundForTextRange_originSelector :: Selector '[Id NSTextRange, NSPoint] ()
drawTextHighlightBackgroundForTextRange_originSelector = mkSelector "drawTextHighlightBackgroundForTextRange:origin:"

-- | @Selector@ for @highlight:@
highlightSelector :: Selector '[RawId] ()
highlightSelector = mkSelector "highlight:"

-- | @Selector@ for @scrollableTextView@
scrollableTextViewSelector :: Selector '[] (Id NSScrollView)
scrollableTextViewSelector = mkSelector "scrollableTextView"

-- | @Selector@ for @fieldEditor@
nsTextViewFieldEditorSelector :: Selector '[] (Id NSTextView)
nsTextViewFieldEditorSelector = mkSelector "fieldEditor"

-- | @Selector@ for @scrollableDocumentContentTextView@
scrollableDocumentContentTextViewSelector :: Selector '[] (Id NSScrollView)
scrollableDocumentContentTextViewSelector = mkSelector "scrollableDocumentContentTextView"

-- | @Selector@ for @scrollablePlainDocumentContentTextView@
scrollablePlainDocumentContentTextViewSelector :: Selector '[] (Id NSScrollView)
scrollablePlainDocumentContentTextViewSelector = mkSelector "scrollablePlainDocumentContentTextView"

-- | @Selector@ for @toggleAutomaticTextCompletion:@
toggleAutomaticTextCompletionSelector :: Selector '[RawId] ()
toggleAutomaticTextCompletionSelector = mkSelector "toggleAutomaticTextCompletion:"

-- | @Selector@ for @updateTouchBarItemIdentifiers@
updateTouchBarItemIdentifiersSelector :: Selector '[] ()
updateTouchBarItemIdentifiersSelector = mkSelector "updateTouchBarItemIdentifiers"

-- | @Selector@ for @updateTextTouchBarItems@
updateTextTouchBarItemsSelector :: Selector '[] ()
updateTextTouchBarItemsSelector = mkSelector "updateTextTouchBarItems"

-- | @Selector@ for @updateCandidates@
updateCandidatesSelector :: Selector '[] ()
updateCandidatesSelector = mkSelector "updateCandidates"

-- | @Selector@ for @orderFrontSharingServicePicker:@
orderFrontSharingServicePickerSelector :: Selector '[RawId] ()
orderFrontSharingServicePickerSelector = mkSelector "orderFrontSharingServicePicker:"

-- | @Selector@ for @toggleQuickLookPreviewPanel:@
toggleQuickLookPreviewPanelSelector :: Selector '[RawId] ()
toggleQuickLookPreviewPanelSelector = mkSelector "toggleQuickLookPreviewPanel:"

-- | @Selector@ for @quickLookPreviewableItemsInRanges:@
quickLookPreviewableItemsInRangesSelector :: Selector '[Id NSArray] (Id NSArray)
quickLookPreviewableItemsInRangesSelector = mkSelector "quickLookPreviewableItemsInRanges:"

-- | @Selector@ for @updateQuickLookPreviewPanel@
updateQuickLookPreviewPanelSelector :: Selector '[] ()
updateQuickLookPreviewPanelSelector = mkSelector "updateQuickLookPreviewPanel"

-- | @Selector@ for @smartDeleteRangeForProposedRange:@
smartDeleteRangeForProposedRangeSelector :: Selector '[NSRange] NSRange
smartDeleteRangeForProposedRangeSelector = mkSelector "smartDeleteRangeForProposedRange:"

-- | @Selector@ for @toggleSmartInsertDelete:@
toggleSmartInsertDeleteSelector :: Selector '[RawId] ()
toggleSmartInsertDeleteSelector = mkSelector "toggleSmartInsertDelete:"

-- | @Selector@ for @smartInsertForString:replacingRange:beforeString:afterString:@
smartInsertForString_replacingRange_beforeString_afterStringSelector :: Selector '[Id NSString, NSRange, Id NSString, Id NSString] ()
smartInsertForString_replacingRange_beforeString_afterStringSelector = mkSelector "smartInsertForString:replacingRange:beforeString:afterString:"

-- | @Selector@ for @smartInsertBeforeStringForString:replacingRange:@
smartInsertBeforeStringForString_replacingRangeSelector :: Selector '[Id NSString, NSRange] (Id NSString)
smartInsertBeforeStringForString_replacingRangeSelector = mkSelector "smartInsertBeforeStringForString:replacingRange:"

-- | @Selector@ for @smartInsertAfterStringForString:replacingRange:@
smartInsertAfterStringForString_replacingRangeSelector :: Selector '[Id NSString, NSRange] (Id NSString)
smartInsertAfterStringForString_replacingRangeSelector = mkSelector "smartInsertAfterStringForString:replacingRange:"

-- | @Selector@ for @toggleAutomaticQuoteSubstitution:@
toggleAutomaticQuoteSubstitutionSelector :: Selector '[RawId] ()
toggleAutomaticQuoteSubstitutionSelector = mkSelector "toggleAutomaticQuoteSubstitution:"

-- | @Selector@ for @toggleAutomaticLinkDetection:@
toggleAutomaticLinkDetectionSelector :: Selector '[RawId] ()
toggleAutomaticLinkDetectionSelector = mkSelector "toggleAutomaticLinkDetection:"

-- | @Selector@ for @toggleAutomaticDataDetection:@
toggleAutomaticDataDetectionSelector :: Selector '[RawId] ()
toggleAutomaticDataDetectionSelector = mkSelector "toggleAutomaticDataDetection:"

-- | @Selector@ for @toggleAutomaticDashSubstitution:@
toggleAutomaticDashSubstitutionSelector :: Selector '[RawId] ()
toggleAutomaticDashSubstitutionSelector = mkSelector "toggleAutomaticDashSubstitution:"

-- | @Selector@ for @toggleAutomaticTextReplacement:@
toggleAutomaticTextReplacementSelector :: Selector '[RawId] ()
toggleAutomaticTextReplacementSelector = mkSelector "toggleAutomaticTextReplacement:"

-- | @Selector@ for @toggleAutomaticSpellingCorrection:@
toggleAutomaticSpellingCorrectionSelector :: Selector '[RawId] ()
toggleAutomaticSpellingCorrectionSelector = mkSelector "toggleAutomaticSpellingCorrection:"

-- | @Selector@ for @checkTextInRange:types:options:@
checkTextInRange_types_optionsSelector :: Selector '[NSRange, CULong, Id NSDictionary] ()
checkTextInRange_types_optionsSelector = mkSelector "checkTextInRange:types:options:"

-- | @Selector@ for @handleTextCheckingResults:forRange:types:options:orthography:wordCount:@
handleTextCheckingResults_forRange_types_options_orthography_wordCountSelector :: Selector '[Id NSArray, NSRange, CULong, Id NSDictionary, Id NSOrthography, CLong] ()
handleTextCheckingResults_forRange_types_options_orthography_wordCountSelector = mkSelector "handleTextCheckingResults:forRange:types:options:orthography:wordCount:"

-- | @Selector@ for @orderFrontSubstitutionsPanel:@
orderFrontSubstitutionsPanelSelector :: Selector '[RawId] ()
orderFrontSubstitutionsPanelSelector = mkSelector "orderFrontSubstitutionsPanel:"

-- | @Selector@ for @checkTextInSelection:@
checkTextInSelectionSelector :: Selector '[RawId] ()
checkTextInSelectionSelector = mkSelector "checkTextInSelection:"

-- | @Selector@ for @checkTextInDocument:@
checkTextInDocumentSelector :: Selector '[RawId] ()
checkTextInDocumentSelector = mkSelector "checkTextInDocument:"

-- | @Selector@ for @setSelectedRanges:affinity:stillSelecting:@
setSelectedRanges_affinity_stillSelectingSelector :: Selector '[Id NSArray, NSSelectionAffinity, Bool] ()
setSelectedRanges_affinity_stillSelectingSelector = mkSelector "setSelectedRanges:affinity:stillSelecting:"

-- | @Selector@ for @setSelectedRange:affinity:stillSelecting:@
setSelectedRange_affinity_stillSelectingSelector :: Selector '[NSRange, NSSelectionAffinity, Bool] ()
setSelectedRange_affinity_stillSelectingSelector = mkSelector "setSelectedRange:affinity:stillSelecting:"

-- | @Selector@ for @updateInsertionPointStateAndRestartTimer:@
updateInsertionPointStateAndRestartTimerSelector :: Selector '[Bool] ()
updateInsertionPointStateAndRestartTimerSelector = mkSelector "updateInsertionPointStateAndRestartTimer:"

-- | @Selector@ for @toggleContinuousSpellChecking:@
toggleContinuousSpellCheckingSelector :: Selector '[RawId] ()
toggleContinuousSpellCheckingSelector = mkSelector "toggleContinuousSpellChecking:"

-- | @Selector@ for @toggleGrammarChecking:@
toggleGrammarCheckingSelector :: Selector '[RawId] ()
toggleGrammarCheckingSelector = mkSelector "toggleGrammarChecking:"

-- | @Selector@ for @setSpellingState:range:@
setSpellingState_rangeSelector :: Selector '[CLong, NSRange] ()
setSpellingState_rangeSelector = mkSelector "setSpellingState:range:"

-- | @Selector@ for @shouldChangeTextInRanges:replacementStrings:@
shouldChangeTextInRanges_replacementStringsSelector :: Selector '[Id NSArray, Id NSArray] Bool
shouldChangeTextInRanges_replacementStringsSelector = mkSelector "shouldChangeTextInRanges:replacementStrings:"

-- | @Selector@ for @shouldChangeTextInRange:replacementString:@
shouldChangeTextInRange_replacementStringSelector :: Selector '[NSRange, Id NSString] Bool
shouldChangeTextInRange_replacementStringSelector = mkSelector "shouldChangeTextInRange:replacementString:"

-- | @Selector@ for @didChangeText@
didChangeTextSelector :: Selector '[] ()
didChangeTextSelector = mkSelector "didChangeText"

-- | @Selector@ for @breakUndoCoalescing@
breakUndoCoalescingSelector :: Selector '[] ()
breakUndoCoalescingSelector = mkSelector "breakUndoCoalescing"

-- | @Selector@ for @showFindIndicatorForRange:@
showFindIndicatorForRangeSelector :: Selector '[NSRange] ()
showFindIndicatorForRangeSelector = mkSelector "showFindIndicatorForRange:"

-- | @Selector@ for @setSelectedRange:@
setSelectedRangeSelector :: Selector '[NSRange] ()
setSelectedRangeSelector = mkSelector "setSelectedRange:"

-- | @Selector@ for @dragSelectionWithEvent:offset:slideBack:@
dragSelectionWithEvent_offset_slideBackSelector :: Selector '[Id NSEvent, NSSize, Bool] Bool
dragSelectionWithEvent_offset_slideBackSelector = mkSelector "dragSelectionWithEvent:offset:slideBack:"

-- | @Selector@ for @dragImageForSelectionWithEvent:origin:@
dragImageForSelectionWithEvent_originSelector :: Selector '[Id NSEvent, Ptr NSPoint] (Id NSImage)
dragImageForSelectionWithEvent_originSelector = mkSelector "dragImageForSelectionWithEvent:origin:"

-- | @Selector@ for @dragOperationForDraggingInfo:type:@
dragOperationForDraggingInfo_typeSelector :: Selector '[RawId, Id NSString] NSDragOperation
dragOperationForDraggingInfo_typeSelector = mkSelector "dragOperationForDraggingInfo:type:"

-- | @Selector@ for @cleanUpAfterDragOperation@
cleanUpAfterDragOperationSelector :: Selector '[] ()
cleanUpAfterDragOperationSelector = mkSelector "cleanUpAfterDragOperation"

-- | @Selector@ for @writeSelectionToPasteboard:type:@
writeSelectionToPasteboard_typeSelector :: Selector '[Id NSPasteboard, Id NSString] Bool
writeSelectionToPasteboard_typeSelector = mkSelector "writeSelectionToPasteboard:type:"

-- | @Selector@ for @writeSelectionToPasteboard:types:@
writeSelectionToPasteboard_typesSelector :: Selector '[Id NSPasteboard, Id NSArray] Bool
writeSelectionToPasteboard_typesSelector = mkSelector "writeSelectionToPasteboard:types:"

-- | @Selector@ for @preferredPasteboardTypeFromArray:restrictedToTypesFromArray:@
preferredPasteboardTypeFromArray_restrictedToTypesFromArraySelector :: Selector '[Id NSArray, Id NSArray] (Id NSString)
preferredPasteboardTypeFromArray_restrictedToTypesFromArraySelector = mkSelector "preferredPasteboardTypeFromArray:restrictedToTypesFromArray:"

-- | @Selector@ for @readSelectionFromPasteboard:type:@
readSelectionFromPasteboard_typeSelector :: Selector '[Id NSPasteboard, Id NSString] Bool
readSelectionFromPasteboard_typeSelector = mkSelector "readSelectionFromPasteboard:type:"

-- | @Selector@ for @readSelectionFromPasteboard:@
readSelectionFromPasteboardSelector :: Selector '[Id NSPasteboard] Bool
readSelectionFromPasteboardSelector = mkSelector "readSelectionFromPasteboard:"

-- | @Selector@ for @registerForServices@
registerForServicesSelector :: Selector '[] ()
registerForServicesSelector = mkSelector "registerForServices"

-- | @Selector@ for @validRequestorForSendType:returnType:@
validRequestorForSendType_returnTypeSelector :: Selector '[Id NSString, Id NSString] RawId
validRequestorForSendType_returnTypeSelector = mkSelector "validRequestorForSendType:returnType:"

-- | @Selector@ for @pasteAsPlainText:@
pasteAsPlainTextSelector :: Selector '[RawId] ()
pasteAsPlainTextSelector = mkSelector "pasteAsPlainText:"

-- | @Selector@ for @pasteAsRichText:@
pasteAsRichTextSelector :: Selector '[RawId] ()
pasteAsRichTextSelector = mkSelector "pasteAsRichText:"

-- | @Selector@ for @complete:@
completeSelector :: Selector '[RawId] ()
completeSelector = mkSelector "complete:"

-- | @Selector@ for @completionsForPartialWordRange:indexOfSelectedItem:@
completionsForPartialWordRange_indexOfSelectedItemSelector :: Selector '[NSRange, Ptr CLong] (Id NSArray)
completionsForPartialWordRange_indexOfSelectedItemSelector = mkSelector "completionsForPartialWordRange:indexOfSelectedItem:"

-- | @Selector@ for @insertCompletion:forPartialWordRange:movement:isFinal:@
insertCompletion_forPartialWordRange_movement_isFinalSelector :: Selector '[Id NSString, NSRange, CLong, Bool] ()
insertCompletion_forPartialWordRange_movement_isFinalSelector = mkSelector "insertCompletion:forPartialWordRange:movement:isFinal:"

-- | @Selector@ for @textContainer@
textContainerSelector :: Selector '[] (Id NSTextContainer)
textContainerSelector = mkSelector "textContainer"

-- | @Selector@ for @setTextContainer:@
setTextContainerSelector :: Selector '[Id NSTextContainer] ()
setTextContainerSelector = mkSelector "setTextContainer:"

-- | @Selector@ for @textContainerInset@
textContainerInsetSelector :: Selector '[] NSSize
textContainerInsetSelector = mkSelector "textContainerInset"

-- | @Selector@ for @setTextContainerInset:@
setTextContainerInsetSelector :: Selector '[NSSize] ()
setTextContainerInsetSelector = mkSelector "setTextContainerInset:"

-- | @Selector@ for @textContainerOrigin@
textContainerOriginSelector :: Selector '[] NSPoint
textContainerOriginSelector = mkSelector "textContainerOrigin"

-- | @Selector@ for @layoutManager@
layoutManagerSelector :: Selector '[] (Id NSLayoutManager)
layoutManagerSelector = mkSelector "layoutManager"

-- | @Selector@ for @textStorage@
textStorageSelector :: Selector '[] (Id NSTextStorage)
textStorageSelector = mkSelector "textStorage"

-- | @Selector@ for @textLayoutManager@
textLayoutManagerSelector :: Selector '[] (Id NSTextLayoutManager)
textLayoutManagerSelector = mkSelector "textLayoutManager"

-- | @Selector@ for @textContentStorage@
textContentStorageSelector :: Selector '[] (Id NSTextContentStorage)
textContentStorageSelector = mkSelector "textContentStorage"

-- | @Selector@ for @shouldDrawInsertionPoint@
shouldDrawInsertionPointSelector :: Selector '[] Bool
shouldDrawInsertionPointSelector = mkSelector "shouldDrawInsertionPoint"

-- | @Selector@ for @stronglyReferencesTextStorage@
stronglyReferencesTextStorageSelector :: Selector '[] Bool
stronglyReferencesTextStorageSelector = mkSelector "stronglyReferencesTextStorage"

-- | @Selector@ for @usesAdaptiveColorMappingForDarkAppearance@
usesAdaptiveColorMappingForDarkAppearanceSelector :: Selector '[] Bool
usesAdaptiveColorMappingForDarkAppearanceSelector = mkSelector "usesAdaptiveColorMappingForDarkAppearance"

-- | @Selector@ for @setUsesAdaptiveColorMappingForDarkAppearance:@
setUsesAdaptiveColorMappingForDarkAppearanceSelector :: Selector '[Bool] ()
setUsesAdaptiveColorMappingForDarkAppearanceSelector = mkSelector "setUsesAdaptiveColorMappingForDarkAppearance:"

-- | @Selector@ for @textHighlightAttributes@
textHighlightAttributesSelector :: Selector '[] (Id NSDictionary)
textHighlightAttributesSelector = mkSelector "textHighlightAttributes"

-- | @Selector@ for @setTextHighlightAttributes:@
setTextHighlightAttributesSelector :: Selector '[Id NSDictionary] ()
setTextHighlightAttributesSelector = mkSelector "setTextHighlightAttributes:"

-- | @Selector@ for @automaticTextCompletionEnabled@
automaticTextCompletionEnabledSelector :: Selector '[] Bool
automaticTextCompletionEnabledSelector = mkSelector "automaticTextCompletionEnabled"

-- | @Selector@ for @setAutomaticTextCompletionEnabled:@
setAutomaticTextCompletionEnabledSelector :: Selector '[Bool] ()
setAutomaticTextCompletionEnabledSelector = mkSelector "setAutomaticTextCompletionEnabled:"

-- | @Selector@ for @allowsCharacterPickerTouchBarItem@
allowsCharacterPickerTouchBarItemSelector :: Selector '[] Bool
allowsCharacterPickerTouchBarItemSelector = mkSelector "allowsCharacterPickerTouchBarItem"

-- | @Selector@ for @setAllowsCharacterPickerTouchBarItem:@
setAllowsCharacterPickerTouchBarItemSelector :: Selector '[Bool] ()
setAllowsCharacterPickerTouchBarItemSelector = mkSelector "setAllowsCharacterPickerTouchBarItem:"

-- | @Selector@ for @candidateListTouchBarItem@
candidateListTouchBarItemSelector :: Selector '[] (Id NSCandidateListTouchBarItem)
candidateListTouchBarItemSelector = mkSelector "candidateListTouchBarItem"

-- | @Selector@ for @smartInsertDeleteEnabled@
smartInsertDeleteEnabledSelector :: Selector '[] Bool
smartInsertDeleteEnabledSelector = mkSelector "smartInsertDeleteEnabled"

-- | @Selector@ for @setSmartInsertDeleteEnabled:@
setSmartInsertDeleteEnabledSelector :: Selector '[Bool] ()
setSmartInsertDeleteEnabledSelector = mkSelector "setSmartInsertDeleteEnabled:"

-- | @Selector@ for @automaticQuoteSubstitutionEnabled@
automaticQuoteSubstitutionEnabledSelector :: Selector '[] Bool
automaticQuoteSubstitutionEnabledSelector = mkSelector "automaticQuoteSubstitutionEnabled"

-- | @Selector@ for @setAutomaticQuoteSubstitutionEnabled:@
setAutomaticQuoteSubstitutionEnabledSelector :: Selector '[Bool] ()
setAutomaticQuoteSubstitutionEnabledSelector = mkSelector "setAutomaticQuoteSubstitutionEnabled:"

-- | @Selector@ for @automaticLinkDetectionEnabled@
automaticLinkDetectionEnabledSelector :: Selector '[] Bool
automaticLinkDetectionEnabledSelector = mkSelector "automaticLinkDetectionEnabled"

-- | @Selector@ for @setAutomaticLinkDetectionEnabled:@
setAutomaticLinkDetectionEnabledSelector :: Selector '[Bool] ()
setAutomaticLinkDetectionEnabledSelector = mkSelector "setAutomaticLinkDetectionEnabled:"

-- | @Selector@ for @automaticDataDetectionEnabled@
automaticDataDetectionEnabledSelector :: Selector '[] Bool
automaticDataDetectionEnabledSelector = mkSelector "automaticDataDetectionEnabled"

-- | @Selector@ for @setAutomaticDataDetectionEnabled:@
setAutomaticDataDetectionEnabledSelector :: Selector '[Bool] ()
setAutomaticDataDetectionEnabledSelector = mkSelector "setAutomaticDataDetectionEnabled:"

-- | @Selector@ for @automaticDashSubstitutionEnabled@
automaticDashSubstitutionEnabledSelector :: Selector '[] Bool
automaticDashSubstitutionEnabledSelector = mkSelector "automaticDashSubstitutionEnabled"

-- | @Selector@ for @setAutomaticDashSubstitutionEnabled:@
setAutomaticDashSubstitutionEnabledSelector :: Selector '[Bool] ()
setAutomaticDashSubstitutionEnabledSelector = mkSelector "setAutomaticDashSubstitutionEnabled:"

-- | @Selector@ for @automaticTextReplacementEnabled@
automaticTextReplacementEnabledSelector :: Selector '[] Bool
automaticTextReplacementEnabledSelector = mkSelector "automaticTextReplacementEnabled"

-- | @Selector@ for @setAutomaticTextReplacementEnabled:@
setAutomaticTextReplacementEnabledSelector :: Selector '[Bool] ()
setAutomaticTextReplacementEnabledSelector = mkSelector "setAutomaticTextReplacementEnabled:"

-- | @Selector@ for @automaticSpellingCorrectionEnabled@
automaticSpellingCorrectionEnabledSelector :: Selector '[] Bool
automaticSpellingCorrectionEnabledSelector = mkSelector "automaticSpellingCorrectionEnabled"

-- | @Selector@ for @setAutomaticSpellingCorrectionEnabled:@
setAutomaticSpellingCorrectionEnabledSelector :: Selector '[Bool] ()
setAutomaticSpellingCorrectionEnabledSelector = mkSelector "setAutomaticSpellingCorrectionEnabled:"

-- | @Selector@ for @enabledTextCheckingTypes@
enabledTextCheckingTypesSelector :: Selector '[] CULong
enabledTextCheckingTypesSelector = mkSelector "enabledTextCheckingTypes"

-- | @Selector@ for @setEnabledTextCheckingTypes:@
setEnabledTextCheckingTypesSelector :: Selector '[CULong] ()
setEnabledTextCheckingTypesSelector = mkSelector "setEnabledTextCheckingTypes:"

-- | @Selector@ for @usesFindPanel@
usesFindPanelSelector :: Selector '[] Bool
usesFindPanelSelector = mkSelector "usesFindPanel"

-- | @Selector@ for @setUsesFindPanel:@
setUsesFindPanelSelector :: Selector '[Bool] ()
setUsesFindPanelSelector = mkSelector "setUsesFindPanel:"

-- | @Selector@ for @usesFindBar@
usesFindBarSelector :: Selector '[] Bool
usesFindBarSelector = mkSelector "usesFindBar"

-- | @Selector@ for @setUsesFindBar:@
setUsesFindBarSelector :: Selector '[Bool] ()
setUsesFindBarSelector = mkSelector "setUsesFindBar:"

-- | @Selector@ for @incrementalSearchingEnabled@
incrementalSearchingEnabledSelector :: Selector '[] Bool
incrementalSearchingEnabledSelector = mkSelector "incrementalSearchingEnabled"

-- | @Selector@ for @setIncrementalSearchingEnabled:@
setIncrementalSearchingEnabledSelector :: Selector '[Bool] ()
setIncrementalSearchingEnabledSelector = mkSelector "setIncrementalSearchingEnabled:"

-- | @Selector@ for @inlinePredictionType@
inlinePredictionTypeSelector :: Selector '[] NSTextInputTraitType
inlinePredictionTypeSelector = mkSelector "inlinePredictionType"

-- | @Selector@ for @setInlinePredictionType:@
setInlinePredictionTypeSelector :: Selector '[NSTextInputTraitType] ()
setInlinePredictionTypeSelector = mkSelector "setInlinePredictionType:"

-- | @Selector@ for @mathExpressionCompletionType@
mathExpressionCompletionTypeSelector :: Selector '[] NSTextInputTraitType
mathExpressionCompletionTypeSelector = mkSelector "mathExpressionCompletionType"

-- | @Selector@ for @setMathExpressionCompletionType:@
setMathExpressionCompletionTypeSelector :: Selector '[NSTextInputTraitType] ()
setMathExpressionCompletionTypeSelector = mkSelector "setMathExpressionCompletionType:"

-- | @Selector@ for @selectedRanges@
selectedRangesSelector :: Selector '[] (Id NSArray)
selectedRangesSelector = mkSelector "selectedRanges"

-- | @Selector@ for @setSelectedRanges:@
setSelectedRangesSelector :: Selector '[Id NSArray] ()
setSelectedRangesSelector = mkSelector "setSelectedRanges:"

-- | @Selector@ for @selectionAffinity@
selectionAffinitySelector :: Selector '[] NSSelectionAffinity
selectionAffinitySelector = mkSelector "selectionAffinity"

-- | @Selector@ for @selectionGranularity@
selectionGranularitySelector :: Selector '[] NSSelectionGranularity
selectionGranularitySelector = mkSelector "selectionGranularity"

-- | @Selector@ for @setSelectionGranularity:@
setSelectionGranularitySelector :: Selector '[NSSelectionGranularity] ()
setSelectionGranularitySelector = mkSelector "setSelectionGranularity:"

-- | @Selector@ for @selectedTextAttributes@
selectedTextAttributesSelector :: Selector '[] (Id NSDictionary)
selectedTextAttributesSelector = mkSelector "selectedTextAttributes"

-- | @Selector@ for @setSelectedTextAttributes:@
setSelectedTextAttributesSelector :: Selector '[Id NSDictionary] ()
setSelectedTextAttributesSelector = mkSelector "setSelectedTextAttributes:"

-- | @Selector@ for @insertionPointColor@
insertionPointColorSelector :: Selector '[] (Id NSColor)
insertionPointColorSelector = mkSelector "insertionPointColor"

-- | @Selector@ for @setInsertionPointColor:@
setInsertionPointColorSelector :: Selector '[Id NSColor] ()
setInsertionPointColorSelector = mkSelector "setInsertionPointColor:"

-- | @Selector@ for @markedTextAttributes@
markedTextAttributesSelector :: Selector '[] (Id NSDictionary)
markedTextAttributesSelector = mkSelector "markedTextAttributes"

-- | @Selector@ for @setMarkedTextAttributes:@
setMarkedTextAttributesSelector :: Selector '[Id NSDictionary] ()
setMarkedTextAttributesSelector = mkSelector "setMarkedTextAttributes:"

-- | @Selector@ for @linkTextAttributes@
linkTextAttributesSelector :: Selector '[] (Id NSDictionary)
linkTextAttributesSelector = mkSelector "linkTextAttributes"

-- | @Selector@ for @setLinkTextAttributes:@
setLinkTextAttributesSelector :: Selector '[Id NSDictionary] ()
setLinkTextAttributesSelector = mkSelector "setLinkTextAttributes:"

-- | @Selector@ for @displaysLinkToolTips@
displaysLinkToolTipsSelector :: Selector '[] Bool
displaysLinkToolTipsSelector = mkSelector "displaysLinkToolTips"

-- | @Selector@ for @setDisplaysLinkToolTips:@
setDisplaysLinkToolTipsSelector :: Selector '[Bool] ()
setDisplaysLinkToolTipsSelector = mkSelector "setDisplaysLinkToolTips:"

-- | @Selector@ for @acceptsGlyphInfo@
acceptsGlyphInfoSelector :: Selector '[] Bool
acceptsGlyphInfoSelector = mkSelector "acceptsGlyphInfo"

-- | @Selector@ for @setAcceptsGlyphInfo:@
setAcceptsGlyphInfoSelector :: Selector '[Bool] ()
setAcceptsGlyphInfoSelector = mkSelector "setAcceptsGlyphInfo:"

-- | @Selector@ for @usesRuler@
usesRulerSelector :: Selector '[] Bool
usesRulerSelector = mkSelector "usesRuler"

-- | @Selector@ for @setUsesRuler:@
setUsesRulerSelector :: Selector '[Bool] ()
setUsesRulerSelector = mkSelector "setUsesRuler:"

-- | @Selector@ for @usesInspectorBar@
usesInspectorBarSelector :: Selector '[] Bool
usesInspectorBarSelector = mkSelector "usesInspectorBar"

-- | @Selector@ for @setUsesInspectorBar:@
setUsesInspectorBarSelector :: Selector '[Bool] ()
setUsesInspectorBarSelector = mkSelector "setUsesInspectorBar:"

-- | @Selector@ for @continuousSpellCheckingEnabled@
continuousSpellCheckingEnabledSelector :: Selector '[] Bool
continuousSpellCheckingEnabledSelector = mkSelector "continuousSpellCheckingEnabled"

-- | @Selector@ for @setContinuousSpellCheckingEnabled:@
setContinuousSpellCheckingEnabledSelector :: Selector '[Bool] ()
setContinuousSpellCheckingEnabledSelector = mkSelector "setContinuousSpellCheckingEnabled:"

-- | @Selector@ for @spellCheckerDocumentTag@
spellCheckerDocumentTagSelector :: Selector '[] CLong
spellCheckerDocumentTagSelector = mkSelector "spellCheckerDocumentTag"

-- | @Selector@ for @grammarCheckingEnabled@
grammarCheckingEnabledSelector :: Selector '[] Bool
grammarCheckingEnabledSelector = mkSelector "grammarCheckingEnabled"

-- | @Selector@ for @setGrammarCheckingEnabled:@
setGrammarCheckingEnabledSelector :: Selector '[Bool] ()
setGrammarCheckingEnabledSelector = mkSelector "setGrammarCheckingEnabled:"

-- | @Selector@ for @typingAttributes@
typingAttributesSelector :: Selector '[] (Id NSDictionary)
typingAttributesSelector = mkSelector "typingAttributes"

-- | @Selector@ for @setTypingAttributes:@
setTypingAttributesSelector :: Selector '[Id NSDictionary] ()
setTypingAttributesSelector = mkSelector "setTypingAttributes:"

-- | @Selector@ for @rangesForUserTextChange@
rangesForUserTextChangeSelector :: Selector '[] (Id NSArray)
rangesForUserTextChangeSelector = mkSelector "rangesForUserTextChange"

-- | @Selector@ for @rangesForUserCharacterAttributeChange@
rangesForUserCharacterAttributeChangeSelector :: Selector '[] (Id NSArray)
rangesForUserCharacterAttributeChangeSelector = mkSelector "rangesForUserCharacterAttributeChange"

-- | @Selector@ for @rangesForUserParagraphAttributeChange@
rangesForUserParagraphAttributeChangeSelector :: Selector '[] (Id NSArray)
rangesForUserParagraphAttributeChangeSelector = mkSelector "rangesForUserParagraphAttributeChange"

-- | @Selector@ for @rangeForUserTextChange@
rangeForUserTextChangeSelector :: Selector '[] NSRange
rangeForUserTextChangeSelector = mkSelector "rangeForUserTextChange"

-- | @Selector@ for @rangeForUserCharacterAttributeChange@
rangeForUserCharacterAttributeChangeSelector :: Selector '[] NSRange
rangeForUserCharacterAttributeChangeSelector = mkSelector "rangeForUserCharacterAttributeChange"

-- | @Selector@ for @rangeForUserParagraphAttributeChange@
rangeForUserParagraphAttributeChangeSelector :: Selector '[] NSRange
rangeForUserParagraphAttributeChangeSelector = mkSelector "rangeForUserParagraphAttributeChange"

-- | @Selector@ for @allowsDocumentBackgroundColorChange@
allowsDocumentBackgroundColorChangeSelector :: Selector '[] Bool
allowsDocumentBackgroundColorChangeSelector = mkSelector "allowsDocumentBackgroundColorChange"

-- | @Selector@ for @setAllowsDocumentBackgroundColorChange:@
setAllowsDocumentBackgroundColorChangeSelector :: Selector '[Bool] ()
setAllowsDocumentBackgroundColorChangeSelector = mkSelector "setAllowsDocumentBackgroundColorChange:"

-- | @Selector@ for @defaultParagraphStyle@
defaultParagraphStyleSelector :: Selector '[] (Id NSParagraphStyle)
defaultParagraphStyleSelector = mkSelector "defaultParagraphStyle"

-- | @Selector@ for @setDefaultParagraphStyle:@
setDefaultParagraphStyleSelector :: Selector '[Id NSParagraphStyle] ()
setDefaultParagraphStyleSelector = mkSelector "setDefaultParagraphStyle:"

-- | @Selector@ for @allowsUndo@
allowsUndoSelector :: Selector '[] Bool
allowsUndoSelector = mkSelector "allowsUndo"

-- | @Selector@ for @setAllowsUndo:@
setAllowsUndoSelector :: Selector '[Bool] ()
setAllowsUndoSelector = mkSelector "setAllowsUndo:"

-- | @Selector@ for @coalescingUndo@
coalescingUndoSelector :: Selector '[] Bool
coalescingUndoSelector = mkSelector "coalescingUndo"

-- | @Selector@ for @allowsImageEditing@
allowsImageEditingSelector :: Selector '[] Bool
allowsImageEditingSelector = mkSelector "allowsImageEditing"

-- | @Selector@ for @setAllowsImageEditing:@
setAllowsImageEditingSelector :: Selector '[Bool] ()
setAllowsImageEditingSelector = mkSelector "setAllowsImageEditing:"

-- | @Selector@ for @usesRolloverButtonForSelection@
usesRolloverButtonForSelectionSelector :: Selector '[] Bool
usesRolloverButtonForSelectionSelector = mkSelector "usesRolloverButtonForSelection"

-- | @Selector@ for @setUsesRolloverButtonForSelection:@
setUsesRolloverButtonForSelectionSelector :: Selector '[Bool] ()
setUsesRolloverButtonForSelectionSelector = mkSelector "setUsesRolloverButtonForSelection:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @editable@
editableSelector :: Selector '[] Bool
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector '[Bool] ()
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @selectable@
selectableSelector :: Selector '[] Bool
selectableSelector = mkSelector "selectable"

-- | @Selector@ for @setSelectable:@
setSelectableSelector :: Selector '[Bool] ()
setSelectableSelector = mkSelector "setSelectable:"

-- | @Selector@ for @richText@
richTextSelector :: Selector '[] Bool
richTextSelector = mkSelector "richText"

-- | @Selector@ for @setRichText:@
setRichTextSelector :: Selector '[Bool] ()
setRichTextSelector = mkSelector "setRichText:"

-- | @Selector@ for @importsGraphics@
importsGraphicsSelector :: Selector '[] Bool
importsGraphicsSelector = mkSelector "importsGraphics"

-- | @Selector@ for @setImportsGraphics:@
setImportsGraphicsSelector :: Selector '[Bool] ()
setImportsGraphicsSelector = mkSelector "setImportsGraphics:"

-- | @Selector@ for @drawsBackground@
drawsBackgroundSelector :: Selector '[] Bool
drawsBackgroundSelector = mkSelector "drawsBackground"

-- | @Selector@ for @setDrawsBackground:@
setDrawsBackgroundSelector :: Selector '[Bool] ()
setDrawsBackgroundSelector = mkSelector "setDrawsBackground:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @fieldEditor@
fieldEditorSelector :: Selector '[] Bool
fieldEditorSelector = mkSelector "fieldEditor"

-- | @Selector@ for @setFieldEditor:@
setFieldEditorSelector :: Selector '[Bool] ()
setFieldEditorSelector = mkSelector "setFieldEditor:"

-- | @Selector@ for @usesFontPanel@
usesFontPanelSelector :: Selector '[] Bool
usesFontPanelSelector = mkSelector "usesFontPanel"

-- | @Selector@ for @setUsesFontPanel:@
setUsesFontPanelSelector :: Selector '[Bool] ()
setUsesFontPanelSelector = mkSelector "setUsesFontPanel:"

-- | @Selector@ for @rulerVisible@
rulerVisibleSelector :: Selector '[] Bool
rulerVisibleSelector = mkSelector "rulerVisible"

-- | @Selector@ for @setRulerVisible:@
setRulerVisibleSelector :: Selector '[Bool] ()
setRulerVisibleSelector = mkSelector "setRulerVisible:"

-- | @Selector@ for @allowedInputSourceLocales@
allowedInputSourceLocalesSelector :: Selector '[] (Id NSArray)
allowedInputSourceLocalesSelector = mkSelector "allowedInputSourceLocales"

-- | @Selector@ for @setAllowedInputSourceLocales:@
setAllowedInputSourceLocalesSelector :: Selector '[Id NSArray] ()
setAllowedInputSourceLocalesSelector = mkSelector "setAllowedInputSourceLocales:"

-- | @Selector@ for @writingToolsActive@
writingToolsActiveSelector :: Selector '[] Bool
writingToolsActiveSelector = mkSelector "writingToolsActive"

-- | @Selector@ for @writingToolsBehavior@
writingToolsBehaviorSelector :: Selector '[] NSWritingToolsBehavior
writingToolsBehaviorSelector = mkSelector "writingToolsBehavior"

-- | @Selector@ for @setWritingToolsBehavior:@
setWritingToolsBehaviorSelector :: Selector '[NSWritingToolsBehavior] ()
setWritingToolsBehaviorSelector = mkSelector "setWritingToolsBehavior:"

-- | @Selector@ for @allowedWritingToolsResultOptions@
allowedWritingToolsResultOptionsSelector :: Selector '[] NSWritingToolsResultOptions
allowedWritingToolsResultOptionsSelector = mkSelector "allowedWritingToolsResultOptions"

-- | @Selector@ for @setAllowedWritingToolsResultOptions:@
setAllowedWritingToolsResultOptionsSelector :: Selector '[NSWritingToolsResultOptions] ()
setAllowedWritingToolsResultOptionsSelector = mkSelector "setAllowedWritingToolsResultOptions:"

-- | @Selector@ for @acceptableDragTypes@
acceptableDragTypesSelector :: Selector '[] (Id NSArray)
acceptableDragTypesSelector = mkSelector "acceptableDragTypes"

-- | @Selector@ for @writablePasteboardTypes@
writablePasteboardTypesSelector :: Selector '[] (Id NSArray)
writablePasteboardTypesSelector = mkSelector "writablePasteboardTypes"

-- | @Selector@ for @readablePasteboardTypes@
readablePasteboardTypesSelector :: Selector '[] (Id NSArray)
readablePasteboardTypesSelector = mkSelector "readablePasteboardTypes"

-- | @Selector@ for @rangeForUserCompletion@
rangeForUserCompletionSelector :: Selector '[] NSRange
rangeForUserCompletionSelector = mkSelector "rangeForUserCompletion"

