{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSObject@.
module ObjC.AppKit.NSObject
  ( NSObject
  , IsNSObject(..)
  , application_delegateHandlesKey
  , tableView_writeRows_toPasteboard
  , validateToolbarItem
  , textStorageWillProcessEditing
  , textStorageDidProcessEditing
  , panel_isValidFilename
  , panel_directoryDidChange
  , panel_compareFilename_with_caseSensitive
  , panel_shouldShowFilename
  , awakeFromNib
  , prepareForInterfaceBuilder
  , changeColor
  , validModesForFontPanel
  , changeFont
  , fontManager_willIncludeFont
  , controlTextDidBeginEditing
  , controlTextDidEndEditing
  , controlTextDidChange
  , objectDidBeginEditing
  , objectDidEndEditing
  , discardEditing
  , commitEditing
  , commitEditingWithDelegate_didCommitSelector_contextInfo
  , commitEditingAndReturnError
  , setDefaultPlaceholder_forMarker_withBinding
  , defaultPlaceholderForMarker_withBinding
  , exposeBinding
  , valueClassForBinding
  , unbind
  , validateMenuItem
  , layer_shouldInheritContentsScale_fromWindow
  , draggingSourceOperationMaskForLocal
  , ignoreModifierKeysWhileDragging
  , pasteboard_provideDataForType
  , pasteboardChangedOwner
  , accessibilitySetOverrideValue_forAttribute
  , accessibilityAttributeValue
  , accessibilityIsAttributeSettable
  , accessibilitySetValue_forAttribute
  , accessibilityAttributeValue_forParameter
  , accessibilityActionDescription
  , accessibilityPerformAction
  , accessibilityIsIgnored
  , accessibilityIndexOfChild
  , accessibilityArrayAttributeCount
  , accessibilityArrayAttributeValues_index_maxCount
  , accessibilityFocusedUIElement
  , accessibilityNotifiesWhenDestroyed
  , application_delegateHandlesKeySelector
  , tableView_writeRows_toPasteboardSelector
  , validateToolbarItemSelector
  , textStorageWillProcessEditingSelector
  , textStorageDidProcessEditingSelector
  , panel_isValidFilenameSelector
  , panel_directoryDidChangeSelector
  , panel_compareFilename_with_caseSensitiveSelector
  , panel_shouldShowFilenameSelector
  , awakeFromNibSelector
  , prepareForInterfaceBuilderSelector
  , changeColorSelector
  , validModesForFontPanelSelector
  , changeFontSelector
  , fontManager_willIncludeFontSelector
  , controlTextDidBeginEditingSelector
  , controlTextDidEndEditingSelector
  , controlTextDidChangeSelector
  , objectDidBeginEditingSelector
  , objectDidEndEditingSelector
  , discardEditingSelector
  , commitEditingSelector
  , commitEditingWithDelegate_didCommitSelector_contextInfoSelector
  , commitEditingAndReturnErrorSelector
  , setDefaultPlaceholder_forMarker_withBindingSelector
  , defaultPlaceholderForMarker_withBindingSelector
  , exposeBindingSelector
  , valueClassForBindingSelector
  , unbindSelector
  , validateMenuItemSelector
  , layer_shouldInheritContentsScale_fromWindowSelector
  , draggingSourceOperationMaskForLocalSelector
  , ignoreModifierKeysWhileDraggingSelector
  , pasteboard_provideDataForTypeSelector
  , pasteboardChangedOwnerSelector
  , accessibilitySetOverrideValue_forAttributeSelector
  , accessibilityAttributeValueSelector
  , accessibilityIsAttributeSettableSelector
  , accessibilitySetValue_forAttributeSelector
  , accessibilityAttributeValue_forParameterSelector
  , accessibilityActionDescriptionSelector
  , accessibilityPerformActionSelector
  , accessibilityIsIgnoredSelector
  , accessibilityIndexOfChildSelector
  , accessibilityArrayAttributeCountSelector
  , accessibilityArrayAttributeValues_index_maxCountSelector
  , accessibilityFocusedUIElementSelector
  , accessibilityNotifiesWhenDestroyedSelector

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
  , NSFontPanelModeMask(NSFontPanelModeMask)
  , pattern NSFontPanelModeMaskFace
  , pattern NSFontPanelModeMaskSize
  , pattern NSFontPanelModeMaskCollection
  , pattern NSFontPanelModeMaskUnderlineEffect
  , pattern NSFontPanelModeMaskStrikethroughEffect
  , pattern NSFontPanelModeMaskTextColorEffect
  , pattern NSFontPanelModeMaskDocumentColorEffect
  , pattern NSFontPanelModeMaskShadowEffect
  , pattern NSFontPanelModeMaskAllEffects
  , pattern NSFontPanelModesMaskStandardModes
  , pattern NSFontPanelModesMaskAllModes

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

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums

-- | @- application:delegateHandlesKey:@
application_delegateHandlesKey :: (IsNSObject nsObject, IsNSApplication sender) => nsObject -> sender -> RawId -> IO Bool
application_delegateHandlesKey nsObject  sender key =
  withObjCPtr sender $ \raw_sender ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "application:delegateHandlesKey:") retCULong [argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr (unRawId key) :: Ptr ())]

-- | @- tableView:writeRows:toPasteboard:@
tableView_writeRows_toPasteboard :: (IsNSObject nsObject, IsNSTableView tableView, IsNSPasteboard pboard) => nsObject -> tableView -> RawId -> pboard -> IO Bool
tableView_writeRows_toPasteboard nsObject  tableView rows pboard =
  withObjCPtr tableView $ \raw_tableView ->
    withObjCPtr pboard $ \raw_pboard ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "tableView:writeRows:toPasteboard:") retCULong [argPtr (castPtr raw_tableView :: Ptr ()), argPtr (castPtr (unRawId rows) :: Ptr ()), argPtr (castPtr raw_pboard :: Ptr ())]

-- | @- validateToolbarItem:@
validateToolbarItem :: (IsNSObject nsObject, IsNSToolbarItem item) => nsObject -> item -> IO Bool
validateToolbarItem nsObject  item =
  withObjCPtr item $ \raw_item ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "validateToolbarItem:") retCULong [argPtr (castPtr raw_item :: Ptr ())]

-- | @- textStorageWillProcessEditing:@
textStorageWillProcessEditing :: IsNSObject nsObject => nsObject -> RawId -> IO ()
textStorageWillProcessEditing nsObject  notification =
    sendMsg nsObject (mkSelector "textStorageWillProcessEditing:") retVoid [argPtr (castPtr (unRawId notification) :: Ptr ())]

-- | @- textStorageDidProcessEditing:@
textStorageDidProcessEditing :: IsNSObject nsObject => nsObject -> RawId -> IO ()
textStorageDidProcessEditing nsObject  notification =
    sendMsg nsObject (mkSelector "textStorageDidProcessEditing:") retVoid [argPtr (castPtr (unRawId notification) :: Ptr ())]

-- | @- panel:isValidFilename:@
panel_isValidFilename :: IsNSObject nsObject => nsObject -> RawId -> RawId -> IO Bool
panel_isValidFilename nsObject  sender filename =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "panel:isValidFilename:") retCULong [argPtr (castPtr (unRawId sender) :: Ptr ()), argPtr (castPtr (unRawId filename) :: Ptr ())]

-- | @- panel:directoryDidChange:@
panel_directoryDidChange :: IsNSObject nsObject => nsObject -> RawId -> RawId -> IO ()
panel_directoryDidChange nsObject  sender path =
    sendMsg nsObject (mkSelector "panel:directoryDidChange:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ()), argPtr (castPtr (unRawId path) :: Ptr ())]

-- | @- panel:compareFilename:with:caseSensitive:@
panel_compareFilename_with_caseSensitive :: IsNSObject nsObject => nsObject -> RawId -> RawId -> RawId -> Bool -> IO CInt
panel_compareFilename_with_caseSensitive nsObject  sender name1 name2 caseSensitive =
    sendMsg nsObject (mkSelector "panel:compareFilename:with:caseSensitive:") retCInt [argPtr (castPtr (unRawId sender) :: Ptr ()), argPtr (castPtr (unRawId name1) :: Ptr ()), argPtr (castPtr (unRawId name2) :: Ptr ()), argCULong (if caseSensitive then 1 else 0)]

-- | @- panel:shouldShowFilename:@
panel_shouldShowFilename :: IsNSObject nsObject => nsObject -> RawId -> RawId -> IO Bool
panel_shouldShowFilename nsObject  sender filename =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "panel:shouldShowFilename:") retCULong [argPtr (castPtr (unRawId sender) :: Ptr ()), argPtr (castPtr (unRawId filename) :: Ptr ())]

-- | @- awakeFromNib@
awakeFromNib :: IsNSObject nsObject => nsObject -> IO ()
awakeFromNib nsObject  =
    sendMsg nsObject (mkSelector "awakeFromNib") retVoid []

-- | @- prepareForInterfaceBuilder@
prepareForInterfaceBuilder :: IsNSObject nsObject => nsObject -> IO ()
prepareForInterfaceBuilder nsObject  =
    sendMsg nsObject (mkSelector "prepareForInterfaceBuilder") retVoid []

-- | @- changeColor:@
changeColor :: IsNSObject nsObject => nsObject -> RawId -> IO ()
changeColor nsObject  sender =
    sendMsg nsObject (mkSelector "changeColor:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- validModesForFontPanel:@
validModesForFontPanel :: (IsNSObject nsObject, IsNSFontPanel fontPanel) => nsObject -> fontPanel -> IO NSFontPanelModeMask
validModesForFontPanel nsObject  fontPanel =
  withObjCPtr fontPanel $ \raw_fontPanel ->
      fmap (coerce :: CULong -> NSFontPanelModeMask) $ sendMsg nsObject (mkSelector "validModesForFontPanel:") retCULong [argPtr (castPtr raw_fontPanel :: Ptr ())]

-- | @- changeFont:@
changeFont :: IsNSObject nsObject => nsObject -> RawId -> IO ()
changeFont nsObject  sender =
    sendMsg nsObject (mkSelector "changeFont:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- fontManager:willIncludeFont:@
fontManager_willIncludeFont :: IsNSObject nsObject => nsObject -> RawId -> RawId -> IO Bool
fontManager_willIncludeFont nsObject  sender fontName =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "fontManager:willIncludeFont:") retCULong [argPtr (castPtr (unRawId sender) :: Ptr ()), argPtr (castPtr (unRawId fontName) :: Ptr ())]

-- | @- controlTextDidBeginEditing:@
controlTextDidBeginEditing :: IsNSObject nsObject => nsObject -> RawId -> IO ()
controlTextDidBeginEditing nsObject  obj_ =
    sendMsg nsObject (mkSelector "controlTextDidBeginEditing:") retVoid [argPtr (castPtr (unRawId obj_) :: Ptr ())]

-- | @- controlTextDidEndEditing:@
controlTextDidEndEditing :: IsNSObject nsObject => nsObject -> RawId -> IO ()
controlTextDidEndEditing nsObject  obj_ =
    sendMsg nsObject (mkSelector "controlTextDidEndEditing:") retVoid [argPtr (castPtr (unRawId obj_) :: Ptr ())]

-- | @- controlTextDidChange:@
controlTextDidChange :: IsNSObject nsObject => nsObject -> RawId -> IO ()
controlTextDidChange nsObject  obj_ =
    sendMsg nsObject (mkSelector "controlTextDidChange:") retVoid [argPtr (castPtr (unRawId obj_) :: Ptr ())]

-- | @- objectDidBeginEditing:@
objectDidBeginEditing :: IsNSObject nsObject => nsObject -> RawId -> IO ()
objectDidBeginEditing nsObject  editor =
    sendMsg nsObject (mkSelector "objectDidBeginEditing:") retVoid [argPtr (castPtr (unRawId editor) :: Ptr ())]

-- | @- objectDidEndEditing:@
objectDidEndEditing :: IsNSObject nsObject => nsObject -> RawId -> IO ()
objectDidEndEditing nsObject  editor =
    sendMsg nsObject (mkSelector "objectDidEndEditing:") retVoid [argPtr (castPtr (unRawId editor) :: Ptr ())]

-- | @- discardEditing@
discardEditing :: IsNSObject nsObject => nsObject -> IO ()
discardEditing nsObject  =
    sendMsg nsObject (mkSelector "discardEditing") retVoid []

-- | @- commitEditing@
commitEditing :: IsNSObject nsObject => nsObject -> IO Bool
commitEditing nsObject  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "commitEditing") retCULong []

-- | @- commitEditingWithDelegate:didCommitSelector:contextInfo:@
commitEditingWithDelegate_didCommitSelector_contextInfo :: IsNSObject nsObject => nsObject -> RawId -> Selector -> Ptr () -> IO ()
commitEditingWithDelegate_didCommitSelector_contextInfo nsObject  delegate didCommitSelector contextInfo =
    sendMsg nsObject (mkSelector "commitEditingWithDelegate:didCommitSelector:contextInfo:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didCommitSelector), argPtr contextInfo]

-- | @- commitEditingAndReturnError:@
commitEditingAndReturnError :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
commitEditingAndReturnError nsObject  error_ =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "commitEditingAndReturnError:") retCULong [argPtr (castPtr (unRawId error_) :: Ptr ())]

-- | @+ setDefaultPlaceholder:forMarker:withBinding:@
setDefaultPlaceholder_forMarker_withBinding :: RawId -> RawId -> RawId -> IO ()
setDefaultPlaceholder_forMarker_withBinding placeholder marker binding =
  do
    cls' <- getRequiredClass "NSObject"
    sendClassMsg cls' (mkSelector "setDefaultPlaceholder:forMarker:withBinding:") retVoid [argPtr (castPtr (unRawId placeholder) :: Ptr ()), argPtr (castPtr (unRawId marker) :: Ptr ()), argPtr (castPtr (unRawId binding) :: Ptr ())]

-- | @+ defaultPlaceholderForMarker:withBinding:@
defaultPlaceholderForMarker_withBinding :: RawId -> RawId -> IO RawId
defaultPlaceholderForMarker_withBinding marker binding =
  do
    cls' <- getRequiredClass "NSObject"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "defaultPlaceholderForMarker:withBinding:") (retPtr retVoid) [argPtr (castPtr (unRawId marker) :: Ptr ()), argPtr (castPtr (unRawId binding) :: Ptr ())]

-- | @+ exposeBinding:@
exposeBinding :: RawId -> IO ()
exposeBinding binding =
  do
    cls' <- getRequiredClass "NSObject"
    sendClassMsg cls' (mkSelector "exposeBinding:") retVoid [argPtr (castPtr (unRawId binding) :: Ptr ())]

-- | @- valueClassForBinding:@
valueClassForBinding :: IsNSObject nsObject => nsObject -> RawId -> IO Class
valueClassForBinding nsObject  binding =
    fmap (Class . castPtr) $ sendMsg nsObject (mkSelector "valueClassForBinding:") (retPtr retVoid) [argPtr (castPtr (unRawId binding) :: Ptr ())]

-- | @- unbind:@
unbind :: IsNSObject nsObject => nsObject -> RawId -> IO ()
unbind nsObject  binding =
    sendMsg nsObject (mkSelector "unbind:") retVoid [argPtr (castPtr (unRawId binding) :: Ptr ())]

-- | @- validateMenuItem:@
validateMenuItem :: (IsNSObject nsObject, IsNSMenuItem menuItem) => nsObject -> menuItem -> IO Bool
validateMenuItem nsObject  menuItem =
  withObjCPtr menuItem $ \raw_menuItem ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "validateMenuItem:") retCULong [argPtr (castPtr raw_menuItem :: Ptr ())]

-- | @- layer:shouldInheritContentsScale:fromWindow:@
layer_shouldInheritContentsScale_fromWindow :: (IsNSObject nsObject, IsNSWindow window) => nsObject -> RawId -> CDouble -> window -> IO Bool
layer_shouldInheritContentsScale_fromWindow nsObject  layer newScale window =
  withObjCPtr window $ \raw_window ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "layer:shouldInheritContentsScale:fromWindow:") retCULong [argPtr (castPtr (unRawId layer) :: Ptr ()), argCDouble newScale, argPtr (castPtr raw_window :: Ptr ())]

-- | @- draggingSourceOperationMaskForLocal:@
draggingSourceOperationMaskForLocal :: IsNSObject nsObject => nsObject -> Bool -> IO NSDragOperation
draggingSourceOperationMaskForLocal nsObject  flag =
    fmap (coerce :: CULong -> NSDragOperation) $ sendMsg nsObject (mkSelector "draggingSourceOperationMaskForLocal:") retCULong [argCULong (if flag then 1 else 0)]

-- | @- ignoreModifierKeysWhileDragging@
ignoreModifierKeysWhileDragging :: IsNSObject nsObject => nsObject -> IO Bool
ignoreModifierKeysWhileDragging nsObject  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "ignoreModifierKeysWhileDragging") retCULong []

-- | @- pasteboard:provideDataForType:@
pasteboard_provideDataForType :: (IsNSObject nsObject, IsNSPasteboard sender) => nsObject -> sender -> RawId -> IO ()
pasteboard_provideDataForType nsObject  sender type_ =
  withObjCPtr sender $ \raw_sender ->
      sendMsg nsObject (mkSelector "pasteboard:provideDataForType:") retVoid [argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr (unRawId type_) :: Ptr ())]

-- | @- pasteboardChangedOwner:@
pasteboardChangedOwner :: (IsNSObject nsObject, IsNSPasteboard sender) => nsObject -> sender -> IO ()
pasteboardChangedOwner nsObject  sender =
  withObjCPtr sender $ \raw_sender ->
      sendMsg nsObject (mkSelector "pasteboardChangedOwner:") retVoid [argPtr (castPtr raw_sender :: Ptr ())]

-- | @- accessibilitySetOverrideValue:forAttribute:@
accessibilitySetOverrideValue_forAttribute :: IsNSObject nsObject => nsObject -> RawId -> RawId -> IO Bool
accessibilitySetOverrideValue_forAttribute nsObject  value attribute =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "accessibilitySetOverrideValue:forAttribute:") retCULong [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr (unRawId attribute) :: Ptr ())]

-- | @- accessibilityAttributeValue:@
accessibilityAttributeValue :: IsNSObject nsObject => nsObject -> RawId -> IO RawId
accessibilityAttributeValue nsObject  attribute =
    fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "accessibilityAttributeValue:") (retPtr retVoid) [argPtr (castPtr (unRawId attribute) :: Ptr ())]

-- | @- accessibilityIsAttributeSettable:@
accessibilityIsAttributeSettable :: IsNSObject nsObject => nsObject -> RawId -> IO Bool
accessibilityIsAttributeSettable nsObject  attribute =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "accessibilityIsAttributeSettable:") retCULong [argPtr (castPtr (unRawId attribute) :: Ptr ())]

-- | @- accessibilitySetValue:forAttribute:@
accessibilitySetValue_forAttribute :: IsNSObject nsObject => nsObject -> RawId -> RawId -> IO ()
accessibilitySetValue_forAttribute nsObject  value attribute =
    sendMsg nsObject (mkSelector "accessibilitySetValue:forAttribute:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr (unRawId attribute) :: Ptr ())]

-- | @- accessibilityAttributeValue:forParameter:@
accessibilityAttributeValue_forParameter :: IsNSObject nsObject => nsObject -> RawId -> RawId -> IO RawId
accessibilityAttributeValue_forParameter nsObject  attribute parameter =
    fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "accessibilityAttributeValue:forParameter:") (retPtr retVoid) [argPtr (castPtr (unRawId attribute) :: Ptr ()), argPtr (castPtr (unRawId parameter) :: Ptr ())]

-- | @- accessibilityActionDescription:@
accessibilityActionDescription :: IsNSObject nsObject => nsObject -> RawId -> IO RawId
accessibilityActionDescription nsObject  action =
    fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "accessibilityActionDescription:") (retPtr retVoid) [argPtr (castPtr (unRawId action) :: Ptr ())]

-- | @- accessibilityPerformAction:@
accessibilityPerformAction :: IsNSObject nsObject => nsObject -> RawId -> IO ()
accessibilityPerformAction nsObject  action =
    sendMsg nsObject (mkSelector "accessibilityPerformAction:") retVoid [argPtr (castPtr (unRawId action) :: Ptr ())]

-- | @- accessibilityIsIgnored@
accessibilityIsIgnored :: IsNSObject nsObject => nsObject -> IO Bool
accessibilityIsIgnored nsObject  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "accessibilityIsIgnored") retCULong []

-- | @- accessibilityIndexOfChild:@
accessibilityIndexOfChild :: IsNSObject nsObject => nsObject -> RawId -> IO CULong
accessibilityIndexOfChild nsObject  child =
    sendMsg nsObject (mkSelector "accessibilityIndexOfChild:") retCULong [argPtr (castPtr (unRawId child) :: Ptr ())]

-- | @- accessibilityArrayAttributeCount:@
accessibilityArrayAttributeCount :: IsNSObject nsObject => nsObject -> RawId -> IO CULong
accessibilityArrayAttributeCount nsObject  attribute =
    sendMsg nsObject (mkSelector "accessibilityArrayAttributeCount:") retCULong [argPtr (castPtr (unRawId attribute) :: Ptr ())]

-- | @- accessibilityArrayAttributeValues:index:maxCount:@
accessibilityArrayAttributeValues_index_maxCount :: IsNSObject nsObject => nsObject -> RawId -> CULong -> CULong -> IO RawId
accessibilityArrayAttributeValues_index_maxCount nsObject  attribute index maxCount =
    fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "accessibilityArrayAttributeValues:index:maxCount:") (retPtr retVoid) [argPtr (castPtr (unRawId attribute) :: Ptr ()), argCULong index, argCULong maxCount]

-- | @- accessibilityFocusedUIElement@
accessibilityFocusedUIElement :: IsNSObject nsObject => nsObject -> IO RawId
accessibilityFocusedUIElement nsObject  =
    fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "accessibilityFocusedUIElement") (retPtr retVoid) []

-- | @- accessibilityNotifiesWhenDestroyed@
accessibilityNotifiesWhenDestroyed :: IsNSObject nsObject => nsObject -> IO Bool
accessibilityNotifiesWhenDestroyed nsObject  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "accessibilityNotifiesWhenDestroyed") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @application:delegateHandlesKey:@
application_delegateHandlesKeySelector :: Selector
application_delegateHandlesKeySelector = mkSelector "application:delegateHandlesKey:"

-- | @Selector@ for @tableView:writeRows:toPasteboard:@
tableView_writeRows_toPasteboardSelector :: Selector
tableView_writeRows_toPasteboardSelector = mkSelector "tableView:writeRows:toPasteboard:"

-- | @Selector@ for @validateToolbarItem:@
validateToolbarItemSelector :: Selector
validateToolbarItemSelector = mkSelector "validateToolbarItem:"

-- | @Selector@ for @textStorageWillProcessEditing:@
textStorageWillProcessEditingSelector :: Selector
textStorageWillProcessEditingSelector = mkSelector "textStorageWillProcessEditing:"

-- | @Selector@ for @textStorageDidProcessEditing:@
textStorageDidProcessEditingSelector :: Selector
textStorageDidProcessEditingSelector = mkSelector "textStorageDidProcessEditing:"

-- | @Selector@ for @panel:isValidFilename:@
panel_isValidFilenameSelector :: Selector
panel_isValidFilenameSelector = mkSelector "panel:isValidFilename:"

-- | @Selector@ for @panel:directoryDidChange:@
panel_directoryDidChangeSelector :: Selector
panel_directoryDidChangeSelector = mkSelector "panel:directoryDidChange:"

-- | @Selector@ for @panel:compareFilename:with:caseSensitive:@
panel_compareFilename_with_caseSensitiveSelector :: Selector
panel_compareFilename_with_caseSensitiveSelector = mkSelector "panel:compareFilename:with:caseSensitive:"

-- | @Selector@ for @panel:shouldShowFilename:@
panel_shouldShowFilenameSelector :: Selector
panel_shouldShowFilenameSelector = mkSelector "panel:shouldShowFilename:"

-- | @Selector@ for @awakeFromNib@
awakeFromNibSelector :: Selector
awakeFromNibSelector = mkSelector "awakeFromNib"

-- | @Selector@ for @prepareForInterfaceBuilder@
prepareForInterfaceBuilderSelector :: Selector
prepareForInterfaceBuilderSelector = mkSelector "prepareForInterfaceBuilder"

-- | @Selector@ for @changeColor:@
changeColorSelector :: Selector
changeColorSelector = mkSelector "changeColor:"

-- | @Selector@ for @validModesForFontPanel:@
validModesForFontPanelSelector :: Selector
validModesForFontPanelSelector = mkSelector "validModesForFontPanel:"

-- | @Selector@ for @changeFont:@
changeFontSelector :: Selector
changeFontSelector = mkSelector "changeFont:"

-- | @Selector@ for @fontManager:willIncludeFont:@
fontManager_willIncludeFontSelector :: Selector
fontManager_willIncludeFontSelector = mkSelector "fontManager:willIncludeFont:"

-- | @Selector@ for @controlTextDidBeginEditing:@
controlTextDidBeginEditingSelector :: Selector
controlTextDidBeginEditingSelector = mkSelector "controlTextDidBeginEditing:"

-- | @Selector@ for @controlTextDidEndEditing:@
controlTextDidEndEditingSelector :: Selector
controlTextDidEndEditingSelector = mkSelector "controlTextDidEndEditing:"

-- | @Selector@ for @controlTextDidChange:@
controlTextDidChangeSelector :: Selector
controlTextDidChangeSelector = mkSelector "controlTextDidChange:"

-- | @Selector@ for @objectDidBeginEditing:@
objectDidBeginEditingSelector :: Selector
objectDidBeginEditingSelector = mkSelector "objectDidBeginEditing:"

-- | @Selector@ for @objectDidEndEditing:@
objectDidEndEditingSelector :: Selector
objectDidEndEditingSelector = mkSelector "objectDidEndEditing:"

-- | @Selector@ for @discardEditing@
discardEditingSelector :: Selector
discardEditingSelector = mkSelector "discardEditing"

-- | @Selector@ for @commitEditing@
commitEditingSelector :: Selector
commitEditingSelector = mkSelector "commitEditing"

-- | @Selector@ for @commitEditingWithDelegate:didCommitSelector:contextInfo:@
commitEditingWithDelegate_didCommitSelector_contextInfoSelector :: Selector
commitEditingWithDelegate_didCommitSelector_contextInfoSelector = mkSelector "commitEditingWithDelegate:didCommitSelector:contextInfo:"

-- | @Selector@ for @commitEditingAndReturnError:@
commitEditingAndReturnErrorSelector :: Selector
commitEditingAndReturnErrorSelector = mkSelector "commitEditingAndReturnError:"

-- | @Selector@ for @setDefaultPlaceholder:forMarker:withBinding:@
setDefaultPlaceholder_forMarker_withBindingSelector :: Selector
setDefaultPlaceholder_forMarker_withBindingSelector = mkSelector "setDefaultPlaceholder:forMarker:withBinding:"

-- | @Selector@ for @defaultPlaceholderForMarker:withBinding:@
defaultPlaceholderForMarker_withBindingSelector :: Selector
defaultPlaceholderForMarker_withBindingSelector = mkSelector "defaultPlaceholderForMarker:withBinding:"

-- | @Selector@ for @exposeBinding:@
exposeBindingSelector :: Selector
exposeBindingSelector = mkSelector "exposeBinding:"

-- | @Selector@ for @valueClassForBinding:@
valueClassForBindingSelector :: Selector
valueClassForBindingSelector = mkSelector "valueClassForBinding:"

-- | @Selector@ for @unbind:@
unbindSelector :: Selector
unbindSelector = mkSelector "unbind:"

-- | @Selector@ for @validateMenuItem:@
validateMenuItemSelector :: Selector
validateMenuItemSelector = mkSelector "validateMenuItem:"

-- | @Selector@ for @layer:shouldInheritContentsScale:fromWindow:@
layer_shouldInheritContentsScale_fromWindowSelector :: Selector
layer_shouldInheritContentsScale_fromWindowSelector = mkSelector "layer:shouldInheritContentsScale:fromWindow:"

-- | @Selector@ for @draggingSourceOperationMaskForLocal:@
draggingSourceOperationMaskForLocalSelector :: Selector
draggingSourceOperationMaskForLocalSelector = mkSelector "draggingSourceOperationMaskForLocal:"

-- | @Selector@ for @ignoreModifierKeysWhileDragging@
ignoreModifierKeysWhileDraggingSelector :: Selector
ignoreModifierKeysWhileDraggingSelector = mkSelector "ignoreModifierKeysWhileDragging"

-- | @Selector@ for @pasteboard:provideDataForType:@
pasteboard_provideDataForTypeSelector :: Selector
pasteboard_provideDataForTypeSelector = mkSelector "pasteboard:provideDataForType:"

-- | @Selector@ for @pasteboardChangedOwner:@
pasteboardChangedOwnerSelector :: Selector
pasteboardChangedOwnerSelector = mkSelector "pasteboardChangedOwner:"

-- | @Selector@ for @accessibilitySetOverrideValue:forAttribute:@
accessibilitySetOverrideValue_forAttributeSelector :: Selector
accessibilitySetOverrideValue_forAttributeSelector = mkSelector "accessibilitySetOverrideValue:forAttribute:"

-- | @Selector@ for @accessibilityAttributeValue:@
accessibilityAttributeValueSelector :: Selector
accessibilityAttributeValueSelector = mkSelector "accessibilityAttributeValue:"

-- | @Selector@ for @accessibilityIsAttributeSettable:@
accessibilityIsAttributeSettableSelector :: Selector
accessibilityIsAttributeSettableSelector = mkSelector "accessibilityIsAttributeSettable:"

-- | @Selector@ for @accessibilitySetValue:forAttribute:@
accessibilitySetValue_forAttributeSelector :: Selector
accessibilitySetValue_forAttributeSelector = mkSelector "accessibilitySetValue:forAttribute:"

-- | @Selector@ for @accessibilityAttributeValue:forParameter:@
accessibilityAttributeValue_forParameterSelector :: Selector
accessibilityAttributeValue_forParameterSelector = mkSelector "accessibilityAttributeValue:forParameter:"

-- | @Selector@ for @accessibilityActionDescription:@
accessibilityActionDescriptionSelector :: Selector
accessibilityActionDescriptionSelector = mkSelector "accessibilityActionDescription:"

-- | @Selector@ for @accessibilityPerformAction:@
accessibilityPerformActionSelector :: Selector
accessibilityPerformActionSelector = mkSelector "accessibilityPerformAction:"

-- | @Selector@ for @accessibilityIsIgnored@
accessibilityIsIgnoredSelector :: Selector
accessibilityIsIgnoredSelector = mkSelector "accessibilityIsIgnored"

-- | @Selector@ for @accessibilityIndexOfChild:@
accessibilityIndexOfChildSelector :: Selector
accessibilityIndexOfChildSelector = mkSelector "accessibilityIndexOfChild:"

-- | @Selector@ for @accessibilityArrayAttributeCount:@
accessibilityArrayAttributeCountSelector :: Selector
accessibilityArrayAttributeCountSelector = mkSelector "accessibilityArrayAttributeCount:"

-- | @Selector@ for @accessibilityArrayAttributeValues:index:maxCount:@
accessibilityArrayAttributeValues_index_maxCountSelector :: Selector
accessibilityArrayAttributeValues_index_maxCountSelector = mkSelector "accessibilityArrayAttributeValues:index:maxCount:"

-- | @Selector@ for @accessibilityFocusedUIElement@
accessibilityFocusedUIElementSelector :: Selector
accessibilityFocusedUIElementSelector = mkSelector "accessibilityFocusedUIElement"

-- | @Selector@ for @accessibilityNotifiesWhenDestroyed@
accessibilityNotifiesWhenDestroyedSelector :: Selector
accessibilityNotifiesWhenDestroyedSelector = mkSelector "accessibilityNotifiesWhenDestroyed"

