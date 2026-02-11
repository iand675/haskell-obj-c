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
  , bind_toObject_withKeyPath_options
  , unbind
  , infoForBinding
  , optionDescriptionsForBinding
  , validateMenuItem
  , view_stringForToolTip_point_userData
  , layer_shouldInheritContentsScale_fromWindow
  , namesOfPromisedFilesDroppedAtDestination
  , draggingSourceOperationMaskForLocal
  , draggedImage_beganAt
  , draggedImage_endedAt_operation
  , draggedImage_movedTo
  , ignoreModifierKeysWhileDragging
  , draggedImage_endedAt_deposited
  , pasteboard_provideDataForType
  , pasteboardChangedOwner
  , accessibilitySetOverrideValue_forAttribute
  , accessibilityAttributeNames
  , accessibilityAttributeValue
  , accessibilityIsAttributeSettable
  , accessibilitySetValue_forAttribute
  , accessibilityParameterizedAttributeNames
  , accessibilityAttributeValue_forParameter
  , accessibilityActionNames
  , accessibilityActionDescription
  , accessibilityPerformAction
  , accessibilityIsIgnored
  , accessibilityHitTest
  , accessibilityIndexOfChild
  , accessibilityArrayAttributeCount
  , accessibilityArrayAttributeValues_index_maxCount
  , exposedBindings
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
  , bind_toObject_withKeyPath_optionsSelector
  , unbindSelector
  , infoForBindingSelector
  , optionDescriptionsForBindingSelector
  , validateMenuItemSelector
  , view_stringForToolTip_point_userDataSelector
  , layer_shouldInheritContentsScale_fromWindowSelector
  , namesOfPromisedFilesDroppedAtDestinationSelector
  , draggingSourceOperationMaskForLocalSelector
  , draggedImage_beganAtSelector
  , draggedImage_endedAt_operationSelector
  , draggedImage_movedToSelector
  , ignoreModifierKeysWhileDraggingSelector
  , draggedImage_endedAt_depositedSelector
  , pasteboard_provideDataForTypeSelector
  , pasteboardChangedOwnerSelector
  , accessibilitySetOverrideValue_forAttributeSelector
  , accessibilityAttributeNamesSelector
  , accessibilityAttributeValueSelector
  , accessibilityIsAttributeSettableSelector
  , accessibilitySetValue_forAttributeSelector
  , accessibilityParameterizedAttributeNamesSelector
  , accessibilityAttributeValue_forParameterSelector
  , accessibilityActionNamesSelector
  , accessibilityActionDescriptionSelector
  , accessibilityPerformActionSelector
  , accessibilityIsIgnoredSelector
  , accessibilityHitTestSelector
  , accessibilityIndexOfChildSelector
  , accessibilityArrayAttributeCountSelector
  , accessibilityArrayAttributeValues_index_maxCountSelector
  , exposedBindingsSelector
  , accessibilityFocusedUIElementSelector
  , accessibilityNotifiesWhenDestroyedSelector

  -- * Enum types
  , NSComparisonResult(NSComparisonResult)
  , pattern NSOrderedAscending
  , pattern NSOrderedSame
  , pattern NSOrderedDescending
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
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- application:delegateHandlesKey:@
application_delegateHandlesKey :: (IsNSObject nsObject, IsNSApplication sender, IsNSString key) => nsObject -> sender -> key -> IO Bool
application_delegateHandlesKey nsObject  sender key =
  withObjCPtr sender $ \raw_sender ->
    withObjCPtr key $ \raw_key ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "application:delegateHandlesKey:") retCULong [argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- tableView:writeRows:toPasteboard:@
tableView_writeRows_toPasteboard :: (IsNSObject nsObject, IsNSTableView tableView, IsNSArray rows, IsNSPasteboard pboard) => nsObject -> tableView -> rows -> pboard -> IO Bool
tableView_writeRows_toPasteboard nsObject  tableView rows pboard =
  withObjCPtr tableView $ \raw_tableView ->
    withObjCPtr rows $ \raw_rows ->
      withObjCPtr pboard $ \raw_pboard ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "tableView:writeRows:toPasteboard:") retCULong [argPtr (castPtr raw_tableView :: Ptr ()), argPtr (castPtr raw_rows :: Ptr ()), argPtr (castPtr raw_pboard :: Ptr ())]

-- | @- validateToolbarItem:@
validateToolbarItem :: (IsNSObject nsObject, IsNSToolbarItem item) => nsObject -> item -> IO Bool
validateToolbarItem nsObject  item =
  withObjCPtr item $ \raw_item ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "validateToolbarItem:") retCULong [argPtr (castPtr raw_item :: Ptr ())]

-- | @- textStorageWillProcessEditing:@
textStorageWillProcessEditing :: (IsNSObject nsObject, IsNSNotification notification) => nsObject -> notification -> IO ()
textStorageWillProcessEditing nsObject  notification =
  withObjCPtr notification $ \raw_notification ->
      sendMsg nsObject (mkSelector "textStorageWillProcessEditing:") retVoid [argPtr (castPtr raw_notification :: Ptr ())]

-- | @- textStorageDidProcessEditing:@
textStorageDidProcessEditing :: (IsNSObject nsObject, IsNSNotification notification) => nsObject -> notification -> IO ()
textStorageDidProcessEditing nsObject  notification =
  withObjCPtr notification $ \raw_notification ->
      sendMsg nsObject (mkSelector "textStorageDidProcessEditing:") retVoid [argPtr (castPtr raw_notification :: Ptr ())]

-- | @- panel:isValidFilename:@
panel_isValidFilename :: (IsNSObject nsObject, IsNSString filename) => nsObject -> RawId -> filename -> IO Bool
panel_isValidFilename nsObject  sender filename =
  withObjCPtr filename $ \raw_filename ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "panel:isValidFilename:") retCULong [argPtr (castPtr (unRawId sender) :: Ptr ()), argPtr (castPtr raw_filename :: Ptr ())]

-- | @- panel:directoryDidChange:@
panel_directoryDidChange :: (IsNSObject nsObject, IsNSString path) => nsObject -> RawId -> path -> IO ()
panel_directoryDidChange nsObject  sender path =
  withObjCPtr path $ \raw_path ->
      sendMsg nsObject (mkSelector "panel:directoryDidChange:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ()), argPtr (castPtr raw_path :: Ptr ())]

-- | @- panel:compareFilename:with:caseSensitive:@
panel_compareFilename_with_caseSensitive :: (IsNSObject nsObject, IsNSString name1, IsNSString name2) => nsObject -> RawId -> name1 -> name2 -> Bool -> IO NSComparisonResult
panel_compareFilename_with_caseSensitive nsObject  sender name1 name2 caseSensitive =
  withObjCPtr name1 $ \raw_name1 ->
    withObjCPtr name2 $ \raw_name2 ->
        fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg nsObject (mkSelector "panel:compareFilename:with:caseSensitive:") retCLong [argPtr (castPtr (unRawId sender) :: Ptr ()), argPtr (castPtr raw_name1 :: Ptr ()), argPtr (castPtr raw_name2 :: Ptr ()), argCULong (if caseSensitive then 1 else 0)]

-- | @- panel:shouldShowFilename:@
panel_shouldShowFilename :: (IsNSObject nsObject, IsNSString filename) => nsObject -> RawId -> filename -> IO Bool
panel_shouldShowFilename nsObject  sender filename =
  withObjCPtr filename $ \raw_filename ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "panel:shouldShowFilename:") retCULong [argPtr (castPtr (unRawId sender) :: Ptr ()), argPtr (castPtr raw_filename :: Ptr ())]

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
fontManager_willIncludeFont :: (IsNSObject nsObject, IsNSString fontName) => nsObject -> RawId -> fontName -> IO Bool
fontManager_willIncludeFont nsObject  sender fontName =
  withObjCPtr fontName $ \raw_fontName ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "fontManager:willIncludeFont:") retCULong [argPtr (castPtr (unRawId sender) :: Ptr ()), argPtr (castPtr raw_fontName :: Ptr ())]

-- | @- controlTextDidBeginEditing:@
controlTextDidBeginEditing :: (IsNSObject nsObject, IsNSNotification obj_) => nsObject -> obj_ -> IO ()
controlTextDidBeginEditing nsObject  obj_ =
  withObjCPtr obj_ $ \raw_obj_ ->
      sendMsg nsObject (mkSelector "controlTextDidBeginEditing:") retVoid [argPtr (castPtr raw_obj_ :: Ptr ())]

-- | @- controlTextDidEndEditing:@
controlTextDidEndEditing :: (IsNSObject nsObject, IsNSNotification obj_) => nsObject -> obj_ -> IO ()
controlTextDidEndEditing nsObject  obj_ =
  withObjCPtr obj_ $ \raw_obj_ ->
      sendMsg nsObject (mkSelector "controlTextDidEndEditing:") retVoid [argPtr (castPtr raw_obj_ :: Ptr ())]

-- | @- controlTextDidChange:@
controlTextDidChange :: (IsNSObject nsObject, IsNSNotification obj_) => nsObject -> obj_ -> IO ()
controlTextDidChange nsObject  obj_ =
  withObjCPtr obj_ $ \raw_obj_ ->
      sendMsg nsObject (mkSelector "controlTextDidChange:") retVoid [argPtr (castPtr raw_obj_ :: Ptr ())]

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
commitEditingAndReturnError :: (IsNSObject nsObject, IsNSError error_) => nsObject -> error_ -> IO Bool
commitEditingAndReturnError nsObject  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "commitEditingAndReturnError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ setDefaultPlaceholder:forMarker:withBinding:@
setDefaultPlaceholder_forMarker_withBinding :: IsNSString binding => RawId -> RawId -> binding -> IO ()
setDefaultPlaceholder_forMarker_withBinding placeholder marker binding =
  do
    cls' <- getRequiredClass "NSObject"
    withObjCPtr binding $ \raw_binding ->
      sendClassMsg cls' (mkSelector "setDefaultPlaceholder:forMarker:withBinding:") retVoid [argPtr (castPtr (unRawId placeholder) :: Ptr ()), argPtr (castPtr (unRawId marker) :: Ptr ()), argPtr (castPtr raw_binding :: Ptr ())]

-- | @+ defaultPlaceholderForMarker:withBinding:@
defaultPlaceholderForMarker_withBinding :: IsNSString binding => RawId -> binding -> IO RawId
defaultPlaceholderForMarker_withBinding marker binding =
  do
    cls' <- getRequiredClass "NSObject"
    withObjCPtr binding $ \raw_binding ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "defaultPlaceholderForMarker:withBinding:") (retPtr retVoid) [argPtr (castPtr (unRawId marker) :: Ptr ()), argPtr (castPtr raw_binding :: Ptr ())]

-- | @+ exposeBinding:@
exposeBinding :: IsNSString binding => binding -> IO ()
exposeBinding binding =
  do
    cls' <- getRequiredClass "NSObject"
    withObjCPtr binding $ \raw_binding ->
      sendClassMsg cls' (mkSelector "exposeBinding:") retVoid [argPtr (castPtr raw_binding :: Ptr ())]

-- | @- valueClassForBinding:@
valueClassForBinding :: (IsNSObject nsObject, IsNSString binding) => nsObject -> binding -> IO Class
valueClassForBinding nsObject  binding =
  withObjCPtr binding $ \raw_binding ->
      fmap (Class . castPtr) $ sendMsg nsObject (mkSelector "valueClassForBinding:") (retPtr retVoid) [argPtr (castPtr raw_binding :: Ptr ())]

-- | @- bind:toObject:withKeyPath:options:@
bind_toObject_withKeyPath_options :: (IsNSObject nsObject, IsNSString binding, IsNSString keyPath, IsNSDictionary options) => nsObject -> binding -> RawId -> keyPath -> options -> IO ()
bind_toObject_withKeyPath_options nsObject  binding observable keyPath options =
  withObjCPtr binding $ \raw_binding ->
    withObjCPtr keyPath $ \raw_keyPath ->
      withObjCPtr options $ \raw_options ->
          sendMsg nsObject (mkSelector "bind:toObject:withKeyPath:options:") retVoid [argPtr (castPtr raw_binding :: Ptr ()), argPtr (castPtr (unRawId observable) :: Ptr ()), argPtr (castPtr raw_keyPath :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())]

-- | @- unbind:@
unbind :: (IsNSObject nsObject, IsNSString binding) => nsObject -> binding -> IO ()
unbind nsObject  binding =
  withObjCPtr binding $ \raw_binding ->
      sendMsg nsObject (mkSelector "unbind:") retVoid [argPtr (castPtr raw_binding :: Ptr ())]

-- | @- infoForBinding:@
infoForBinding :: (IsNSObject nsObject, IsNSString binding) => nsObject -> binding -> IO (Id NSDictionary)
infoForBinding nsObject  binding =
  withObjCPtr binding $ \raw_binding ->
      sendMsg nsObject (mkSelector "infoForBinding:") (retPtr retVoid) [argPtr (castPtr raw_binding :: Ptr ())] >>= retainedObject . castPtr

-- | @- optionDescriptionsForBinding:@
optionDescriptionsForBinding :: (IsNSObject nsObject, IsNSString binding) => nsObject -> binding -> IO (Id NSArray)
optionDescriptionsForBinding nsObject  binding =
  withObjCPtr binding $ \raw_binding ->
      sendMsg nsObject (mkSelector "optionDescriptionsForBinding:") (retPtr retVoid) [argPtr (castPtr raw_binding :: Ptr ())] >>= retainedObject . castPtr

-- | @- validateMenuItem:@
validateMenuItem :: (IsNSObject nsObject, IsNSMenuItem menuItem) => nsObject -> menuItem -> IO Bool
validateMenuItem nsObject  menuItem =
  withObjCPtr menuItem $ \raw_menuItem ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "validateMenuItem:") retCULong [argPtr (castPtr raw_menuItem :: Ptr ())]

-- | @- view:stringForToolTip:point:userData:@
view_stringForToolTip_point_userData :: (IsNSObject nsObject, IsNSView view) => nsObject -> view -> CLong -> NSPoint -> Ptr () -> IO (Id NSString)
view_stringForToolTip_point_userData nsObject  view tag point data_ =
  withObjCPtr view $ \raw_view ->
      sendMsg nsObject (mkSelector "view:stringForToolTip:point:userData:") (retPtr retVoid) [argPtr (castPtr raw_view :: Ptr ()), argCLong tag, argNSPoint point, argPtr data_] >>= retainedObject . castPtr

-- | @- layer:shouldInheritContentsScale:fromWindow:@
layer_shouldInheritContentsScale_fromWindow :: (IsNSObject nsObject, IsNSWindow window) => nsObject -> RawId -> CDouble -> window -> IO Bool
layer_shouldInheritContentsScale_fromWindow nsObject  layer newScale window =
  withObjCPtr window $ \raw_window ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "layer:shouldInheritContentsScale:fromWindow:") retCULong [argPtr (castPtr (unRawId layer) :: Ptr ()), argCDouble newScale, argPtr (castPtr raw_window :: Ptr ())]

-- | @- namesOfPromisedFilesDroppedAtDestination:@
namesOfPromisedFilesDroppedAtDestination :: (IsNSObject nsObject, IsNSURL dropDestination) => nsObject -> dropDestination -> IO (Id NSArray)
namesOfPromisedFilesDroppedAtDestination nsObject  dropDestination =
  withObjCPtr dropDestination $ \raw_dropDestination ->
      sendMsg nsObject (mkSelector "namesOfPromisedFilesDroppedAtDestination:") (retPtr retVoid) [argPtr (castPtr raw_dropDestination :: Ptr ())] >>= retainedObject . castPtr

-- | @- draggingSourceOperationMaskForLocal:@
draggingSourceOperationMaskForLocal :: IsNSObject nsObject => nsObject -> Bool -> IO NSDragOperation
draggingSourceOperationMaskForLocal nsObject  flag =
    fmap (coerce :: CULong -> NSDragOperation) $ sendMsg nsObject (mkSelector "draggingSourceOperationMaskForLocal:") retCULong [argCULong (if flag then 1 else 0)]

-- | @- draggedImage:beganAt:@
draggedImage_beganAt :: (IsNSObject nsObject, IsNSImage image) => nsObject -> image -> NSPoint -> IO ()
draggedImage_beganAt nsObject  image screenPoint =
  withObjCPtr image $ \raw_image ->
      sendMsg nsObject (mkSelector "draggedImage:beganAt:") retVoid [argPtr (castPtr raw_image :: Ptr ()), argNSPoint screenPoint]

-- | @- draggedImage:endedAt:operation:@
draggedImage_endedAt_operation :: (IsNSObject nsObject, IsNSImage image) => nsObject -> image -> NSPoint -> NSDragOperation -> IO ()
draggedImage_endedAt_operation nsObject  image screenPoint operation =
  withObjCPtr image $ \raw_image ->
      sendMsg nsObject (mkSelector "draggedImage:endedAt:operation:") retVoid [argPtr (castPtr raw_image :: Ptr ()), argNSPoint screenPoint, argCULong (coerce operation)]

-- | @- draggedImage:movedTo:@
draggedImage_movedTo :: (IsNSObject nsObject, IsNSImage image) => nsObject -> image -> NSPoint -> IO ()
draggedImage_movedTo nsObject  image screenPoint =
  withObjCPtr image $ \raw_image ->
      sendMsg nsObject (mkSelector "draggedImage:movedTo:") retVoid [argPtr (castPtr raw_image :: Ptr ()), argNSPoint screenPoint]

-- | @- ignoreModifierKeysWhileDragging@
ignoreModifierKeysWhileDragging :: IsNSObject nsObject => nsObject -> IO Bool
ignoreModifierKeysWhileDragging nsObject  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "ignoreModifierKeysWhileDragging") retCULong []

-- | @- draggedImage:endedAt:deposited:@
draggedImage_endedAt_deposited :: (IsNSObject nsObject, IsNSImage image) => nsObject -> image -> NSPoint -> Bool -> IO ()
draggedImage_endedAt_deposited nsObject  image screenPoint flag =
  withObjCPtr image $ \raw_image ->
      sendMsg nsObject (mkSelector "draggedImage:endedAt:deposited:") retVoid [argPtr (castPtr raw_image :: Ptr ()), argNSPoint screenPoint, argCULong (if flag then 1 else 0)]

-- | @- pasteboard:provideDataForType:@
pasteboard_provideDataForType :: (IsNSObject nsObject, IsNSPasteboard sender, IsNSString type_) => nsObject -> sender -> type_ -> IO ()
pasteboard_provideDataForType nsObject  sender type_ =
  withObjCPtr sender $ \raw_sender ->
    withObjCPtr type_ $ \raw_type_ ->
        sendMsg nsObject (mkSelector "pasteboard:provideDataForType:") retVoid [argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- pasteboardChangedOwner:@
pasteboardChangedOwner :: (IsNSObject nsObject, IsNSPasteboard sender) => nsObject -> sender -> IO ()
pasteboardChangedOwner nsObject  sender =
  withObjCPtr sender $ \raw_sender ->
      sendMsg nsObject (mkSelector "pasteboardChangedOwner:") retVoid [argPtr (castPtr raw_sender :: Ptr ())]

-- | @- accessibilitySetOverrideValue:forAttribute:@
accessibilitySetOverrideValue_forAttribute :: (IsNSObject nsObject, IsNSString attribute) => nsObject -> RawId -> attribute -> IO Bool
accessibilitySetOverrideValue_forAttribute nsObject  value attribute =
  withObjCPtr attribute $ \raw_attribute ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "accessibilitySetOverrideValue:forAttribute:") retCULong [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_attribute :: Ptr ())]

-- | @- accessibilityAttributeNames@
accessibilityAttributeNames :: IsNSObject nsObject => nsObject -> IO (Id NSArray)
accessibilityAttributeNames nsObject  =
    sendMsg nsObject (mkSelector "accessibilityAttributeNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- accessibilityAttributeValue:@
accessibilityAttributeValue :: (IsNSObject nsObject, IsNSString attribute) => nsObject -> attribute -> IO RawId
accessibilityAttributeValue nsObject  attribute =
  withObjCPtr attribute $ \raw_attribute ->
      fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "accessibilityAttributeValue:") (retPtr retVoid) [argPtr (castPtr raw_attribute :: Ptr ())]

-- | @- accessibilityIsAttributeSettable:@
accessibilityIsAttributeSettable :: (IsNSObject nsObject, IsNSString attribute) => nsObject -> attribute -> IO Bool
accessibilityIsAttributeSettable nsObject  attribute =
  withObjCPtr attribute $ \raw_attribute ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "accessibilityIsAttributeSettable:") retCULong [argPtr (castPtr raw_attribute :: Ptr ())]

-- | @- accessibilitySetValue:forAttribute:@
accessibilitySetValue_forAttribute :: (IsNSObject nsObject, IsNSString attribute) => nsObject -> RawId -> attribute -> IO ()
accessibilitySetValue_forAttribute nsObject  value attribute =
  withObjCPtr attribute $ \raw_attribute ->
      sendMsg nsObject (mkSelector "accessibilitySetValue:forAttribute:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_attribute :: Ptr ())]

-- | @- accessibilityParameterizedAttributeNames@
accessibilityParameterizedAttributeNames :: IsNSObject nsObject => nsObject -> IO (Id NSArray)
accessibilityParameterizedAttributeNames nsObject  =
    sendMsg nsObject (mkSelector "accessibilityParameterizedAttributeNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- accessibilityAttributeValue:forParameter:@
accessibilityAttributeValue_forParameter :: (IsNSObject nsObject, IsNSString attribute) => nsObject -> attribute -> RawId -> IO RawId
accessibilityAttributeValue_forParameter nsObject  attribute parameter =
  withObjCPtr attribute $ \raw_attribute ->
      fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "accessibilityAttributeValue:forParameter:") (retPtr retVoid) [argPtr (castPtr raw_attribute :: Ptr ()), argPtr (castPtr (unRawId parameter) :: Ptr ())]

-- | @- accessibilityActionNames@
accessibilityActionNames :: IsNSObject nsObject => nsObject -> IO (Id NSArray)
accessibilityActionNames nsObject  =
    sendMsg nsObject (mkSelector "accessibilityActionNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- accessibilityActionDescription:@
accessibilityActionDescription :: (IsNSObject nsObject, IsNSString action) => nsObject -> action -> IO (Id NSString)
accessibilityActionDescription nsObject  action =
  withObjCPtr action $ \raw_action ->
      sendMsg nsObject (mkSelector "accessibilityActionDescription:") (retPtr retVoid) [argPtr (castPtr raw_action :: Ptr ())] >>= retainedObject . castPtr

-- | @- accessibilityPerformAction:@
accessibilityPerformAction :: (IsNSObject nsObject, IsNSString action) => nsObject -> action -> IO ()
accessibilityPerformAction nsObject  action =
  withObjCPtr action $ \raw_action ->
      sendMsg nsObject (mkSelector "accessibilityPerformAction:") retVoid [argPtr (castPtr raw_action :: Ptr ())]

-- | @- accessibilityIsIgnored@
accessibilityIsIgnored :: IsNSObject nsObject => nsObject -> IO Bool
accessibilityIsIgnored nsObject  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "accessibilityIsIgnored") retCULong []

-- | @- accessibilityHitTest:@
accessibilityHitTest :: IsNSObject nsObject => nsObject -> NSPoint -> IO RawId
accessibilityHitTest nsObject  point =
    fmap (RawId . castPtr) $ sendMsg nsObject (mkSelector "accessibilityHitTest:") (retPtr retVoid) [argNSPoint point]

-- | @- accessibilityIndexOfChild:@
accessibilityIndexOfChild :: IsNSObject nsObject => nsObject -> RawId -> IO CULong
accessibilityIndexOfChild nsObject  child =
    sendMsg nsObject (mkSelector "accessibilityIndexOfChild:") retCULong [argPtr (castPtr (unRawId child) :: Ptr ())]

-- | @- accessibilityArrayAttributeCount:@
accessibilityArrayAttributeCount :: (IsNSObject nsObject, IsNSString attribute) => nsObject -> attribute -> IO CULong
accessibilityArrayAttributeCount nsObject  attribute =
  withObjCPtr attribute $ \raw_attribute ->
      sendMsg nsObject (mkSelector "accessibilityArrayAttributeCount:") retCULong [argPtr (castPtr raw_attribute :: Ptr ())]

-- | @- accessibilityArrayAttributeValues:index:maxCount:@
accessibilityArrayAttributeValues_index_maxCount :: (IsNSObject nsObject, IsNSString attribute) => nsObject -> attribute -> CULong -> CULong -> IO (Id NSArray)
accessibilityArrayAttributeValues_index_maxCount nsObject  attribute index maxCount =
  withObjCPtr attribute $ \raw_attribute ->
      sendMsg nsObject (mkSelector "accessibilityArrayAttributeValues:index:maxCount:") (retPtr retVoid) [argPtr (castPtr raw_attribute :: Ptr ()), argCULong index, argCULong maxCount] >>= retainedObject . castPtr

-- | @- exposedBindings@
exposedBindings :: IsNSObject nsObject => nsObject -> IO (Id NSArray)
exposedBindings nsObject  =
    sendMsg nsObject (mkSelector "exposedBindings") (retPtr retVoid) [] >>= retainedObject . castPtr

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

-- | @Selector@ for @bind:toObject:withKeyPath:options:@
bind_toObject_withKeyPath_optionsSelector :: Selector
bind_toObject_withKeyPath_optionsSelector = mkSelector "bind:toObject:withKeyPath:options:"

-- | @Selector@ for @unbind:@
unbindSelector :: Selector
unbindSelector = mkSelector "unbind:"

-- | @Selector@ for @infoForBinding:@
infoForBindingSelector :: Selector
infoForBindingSelector = mkSelector "infoForBinding:"

-- | @Selector@ for @optionDescriptionsForBinding:@
optionDescriptionsForBindingSelector :: Selector
optionDescriptionsForBindingSelector = mkSelector "optionDescriptionsForBinding:"

-- | @Selector@ for @validateMenuItem:@
validateMenuItemSelector :: Selector
validateMenuItemSelector = mkSelector "validateMenuItem:"

-- | @Selector@ for @view:stringForToolTip:point:userData:@
view_stringForToolTip_point_userDataSelector :: Selector
view_stringForToolTip_point_userDataSelector = mkSelector "view:stringForToolTip:point:userData:"

-- | @Selector@ for @layer:shouldInheritContentsScale:fromWindow:@
layer_shouldInheritContentsScale_fromWindowSelector :: Selector
layer_shouldInheritContentsScale_fromWindowSelector = mkSelector "layer:shouldInheritContentsScale:fromWindow:"

-- | @Selector@ for @namesOfPromisedFilesDroppedAtDestination:@
namesOfPromisedFilesDroppedAtDestinationSelector :: Selector
namesOfPromisedFilesDroppedAtDestinationSelector = mkSelector "namesOfPromisedFilesDroppedAtDestination:"

-- | @Selector@ for @draggingSourceOperationMaskForLocal:@
draggingSourceOperationMaskForLocalSelector :: Selector
draggingSourceOperationMaskForLocalSelector = mkSelector "draggingSourceOperationMaskForLocal:"

-- | @Selector@ for @draggedImage:beganAt:@
draggedImage_beganAtSelector :: Selector
draggedImage_beganAtSelector = mkSelector "draggedImage:beganAt:"

-- | @Selector@ for @draggedImage:endedAt:operation:@
draggedImage_endedAt_operationSelector :: Selector
draggedImage_endedAt_operationSelector = mkSelector "draggedImage:endedAt:operation:"

-- | @Selector@ for @draggedImage:movedTo:@
draggedImage_movedToSelector :: Selector
draggedImage_movedToSelector = mkSelector "draggedImage:movedTo:"

-- | @Selector@ for @ignoreModifierKeysWhileDragging@
ignoreModifierKeysWhileDraggingSelector :: Selector
ignoreModifierKeysWhileDraggingSelector = mkSelector "ignoreModifierKeysWhileDragging"

-- | @Selector@ for @draggedImage:endedAt:deposited:@
draggedImage_endedAt_depositedSelector :: Selector
draggedImage_endedAt_depositedSelector = mkSelector "draggedImage:endedAt:deposited:"

-- | @Selector@ for @pasteboard:provideDataForType:@
pasteboard_provideDataForTypeSelector :: Selector
pasteboard_provideDataForTypeSelector = mkSelector "pasteboard:provideDataForType:"

-- | @Selector@ for @pasteboardChangedOwner:@
pasteboardChangedOwnerSelector :: Selector
pasteboardChangedOwnerSelector = mkSelector "pasteboardChangedOwner:"

-- | @Selector@ for @accessibilitySetOverrideValue:forAttribute:@
accessibilitySetOverrideValue_forAttributeSelector :: Selector
accessibilitySetOverrideValue_forAttributeSelector = mkSelector "accessibilitySetOverrideValue:forAttribute:"

-- | @Selector@ for @accessibilityAttributeNames@
accessibilityAttributeNamesSelector :: Selector
accessibilityAttributeNamesSelector = mkSelector "accessibilityAttributeNames"

-- | @Selector@ for @accessibilityAttributeValue:@
accessibilityAttributeValueSelector :: Selector
accessibilityAttributeValueSelector = mkSelector "accessibilityAttributeValue:"

-- | @Selector@ for @accessibilityIsAttributeSettable:@
accessibilityIsAttributeSettableSelector :: Selector
accessibilityIsAttributeSettableSelector = mkSelector "accessibilityIsAttributeSettable:"

-- | @Selector@ for @accessibilitySetValue:forAttribute:@
accessibilitySetValue_forAttributeSelector :: Selector
accessibilitySetValue_forAttributeSelector = mkSelector "accessibilitySetValue:forAttribute:"

-- | @Selector@ for @accessibilityParameterizedAttributeNames@
accessibilityParameterizedAttributeNamesSelector :: Selector
accessibilityParameterizedAttributeNamesSelector = mkSelector "accessibilityParameterizedAttributeNames"

-- | @Selector@ for @accessibilityAttributeValue:forParameter:@
accessibilityAttributeValue_forParameterSelector :: Selector
accessibilityAttributeValue_forParameterSelector = mkSelector "accessibilityAttributeValue:forParameter:"

-- | @Selector@ for @accessibilityActionNames@
accessibilityActionNamesSelector :: Selector
accessibilityActionNamesSelector = mkSelector "accessibilityActionNames"

-- | @Selector@ for @accessibilityActionDescription:@
accessibilityActionDescriptionSelector :: Selector
accessibilityActionDescriptionSelector = mkSelector "accessibilityActionDescription:"

-- | @Selector@ for @accessibilityPerformAction:@
accessibilityPerformActionSelector :: Selector
accessibilityPerformActionSelector = mkSelector "accessibilityPerformAction:"

-- | @Selector@ for @accessibilityIsIgnored@
accessibilityIsIgnoredSelector :: Selector
accessibilityIsIgnoredSelector = mkSelector "accessibilityIsIgnored"

-- | @Selector@ for @accessibilityHitTest:@
accessibilityHitTestSelector :: Selector
accessibilityHitTestSelector = mkSelector "accessibilityHitTest:"

-- | @Selector@ for @accessibilityIndexOfChild:@
accessibilityIndexOfChildSelector :: Selector
accessibilityIndexOfChildSelector = mkSelector "accessibilityIndexOfChild:"

-- | @Selector@ for @accessibilityArrayAttributeCount:@
accessibilityArrayAttributeCountSelector :: Selector
accessibilityArrayAttributeCountSelector = mkSelector "accessibilityArrayAttributeCount:"

-- | @Selector@ for @accessibilityArrayAttributeValues:index:maxCount:@
accessibilityArrayAttributeValues_index_maxCountSelector :: Selector
accessibilityArrayAttributeValues_index_maxCountSelector = mkSelector "accessibilityArrayAttributeValues:index:maxCount:"

-- | @Selector@ for @exposedBindings@
exposedBindingsSelector :: Selector
exposedBindingsSelector = mkSelector "exposedBindings"

-- | @Selector@ for @accessibilityFocusedUIElement@
accessibilityFocusedUIElementSelector :: Selector
accessibilityFocusedUIElementSelector = mkSelector "accessibilityFocusedUIElement"

-- | @Selector@ for @accessibilityNotifiesWhenDestroyed@
accessibilityNotifiesWhenDestroyedSelector :: Selector
accessibilityNotifiesWhenDestroyedSelector = mkSelector "accessibilityNotifiesWhenDestroyed"

