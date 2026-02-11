{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDocument@.
module ObjC.AppKit.NSDocument
  ( NSDocument
  , IsNSDocument(..)
  , init_
  , initWithType_error
  , canConcurrentlyReadDocumentsOfType
  , initWithContentsOfURL_ofType_error
  , initForURL_withContentsOfURL_ofType_error
  , performActivityWithSynchronousWaiting_usingBlock
  , continueActivityUsingBlock
  , continueAsynchronousWorkOnMainThreadUsingBlock
  , performSynchronousFileAccessUsingBlock
  , performAsynchronousFileAccessUsingBlock
  , revertDocumentToSaved
  , revertToContentsOfURL_ofType_error
  , readFromURL_ofType_error
  , readFromFileWrapper_ofType_error
  , readFromData_ofType_error
  , writeToURL_ofType_error
  , fileWrapperOfType_error
  , dataOfType_error
  , unblockUserInteraction
  , writeSafelyToURL_ofType_forSaveOperation_error
  , writeToURL_ofType_forSaveOperation_originalContentsURL_error
  , fileAttributesToWriteToURL_ofType_forSaveOperation_originalContentsURL_error
  , saveDocument
  , saveDocumentAs
  , saveDocumentTo
  , saveDocumentWithDelegate_didSaveSelector_contextInfo
  , runModalSavePanelForSaveOperation_delegate_didSaveSelector_contextInfo
  , prepareSavePanel
  , saveToURL_ofType_forSaveOperation_delegate_didSaveSelector_contextInfo
  , saveToURL_ofType_forSaveOperation_completionHandler
  , canAsynchronouslyWriteToURL_ofType_forSaveOperation
  , checkAutosavingSafetyAndReturnError
  , scheduleAutosaving
  , autosaveDocumentWithDelegate_didAutosaveSelector_contextInfo
  , autosaveWithImplicitCancellability_completionHandler
  , browseDocumentVersions
  , stopBrowsingVersionsWithCompletionHandler
  , canCloseDocumentWithDelegate_shouldCloseSelector_contextInfo
  , close
  , duplicateDocument
  , duplicateDocumentWithDelegate_didDuplicateSelector_contextInfo
  , duplicateAndReturnError
  , renameDocument
  , moveDocumentToUbiquityContainer
  , moveDocument
  , moveDocumentWithCompletionHandler
  , moveToURL_completionHandler
  , lockDocument
  , unlockDocument
  , lockDocumentWithCompletionHandler
  , lockWithCompletionHandler
  , unlockDocumentWithCompletionHandler
  , unlockWithCompletionHandler
  , runPageLayout
  , runModalPageLayoutWithPrintInfo_delegate_didRunSelector_contextInfo
  , preparePageLayout
  , shouldChangePrintInfo
  , printDocument
  , printDocumentWithSettings_showPrintPanel_delegate_didPrintSelector_contextInfo
  , printOperationWithSettings_error
  , runModalPrintOperation_delegate_didRunSelector_contextInfo
  , saveDocumentToPDF
  , shareDocumentWithSharingService_completionHandler
  , prepareSharingServicePicker
  , updateChangeCount
  , changeCountTokenForSaveOperation
  , updateChangeCountWithToken_forSaveOperation
  , presentError_modalForWindow_delegate_didPresentSelector_contextInfo
  , presentError
  , willPresentError
  , willNotPresentError
  , makeWindowControllers
  , windowControllerWillLoadNib
  , windowControllerDidLoadNib
  , setWindow
  , addWindowController
  , removeWindowController
  , showWindows
  , shouldCloseWindowController_delegate_shouldCloseSelector_contextInfo
  , setDisplayName
  , defaultDraftName
  , isNativeType
  , writableTypesForSaveOperation
  , fileNameExtensionForType_saveOperation
  , validateUserInterfaceItem
  , relinquishPresentedItemToReader
  , relinquishPresentedItemToWriter
  , savePresentedItemChangesWithCompletionHandler
  , accommodatePresentedItemDeletionWithCompletionHandler
  , presentedItemDidMoveToURL
  , presentedItemDidChange
  , presentedItemDidChangeUbiquityAttributes
  , presentedItemDidGainVersion
  , presentedItemDidLoseVersion
  , presentedItemDidResolveConflictVersion
  , restoreDocumentWindowWithIdentifier_state_completionHandler
  , encodeRestorableStateWithCoder
  , encodeRestorableStateWithCoder_backgroundQueue
  , restoreStateWithCoder
  , invalidateRestorableState
  , allowedClassesForRestorableStateKeyPath
  , handleSaveScriptCommand
  , handleCloseScriptCommand
  , handlePrintScriptCommand
  , updateUserActivityState
  , saveToURL_ofType_forSaveOperation_error
  , dataRepresentationOfType
  , fileAttributesToWriteToFile_ofType_saveOperation
  , fileName
  , fileWrapperRepresentationOfType
  , initWithContentsOfFile_ofType
  , initWithContentsOfURL_ofType
  , loadDataRepresentation_ofType
  , loadFileWrapperRepresentation_ofType
  , printShowingPrintPanel
  , readFromFile_ofType
  , readFromURL_ofType
  , revertToSavedFromFile_ofType
  , revertToSavedFromURL_ofType
  , runModalPageLayoutWithPrintInfo
  , saveToFile_saveOperation_delegate_didSaveSelector_contextInfo
  , setFileName
  , writeToFile_ofType
  , writeToFile_ofType_originalFile_saveOperation
  , writeToURL_ofType
  , writeWithBackupToFile_ofType_saveOperation
  , fileType
  , setFileType
  , fileURL
  , setFileURL
  , fileModificationDate
  , setFileModificationDate
  , draft
  , setDraft
  , entireFileLoaded
  , autosavingIsImplicitlyCancellable
  , keepBackupFile
  , savePanelShowsFileFormatsControl
  , fileNameExtensionWasHiddenInLastRunSavePanel
  , fileTypeFromLastRunSavePanel
  , hasUnautosavedChanges
  , autosavesInPlace
  , preservesVersions
  , browsingVersions
  , autosavesDrafts
  , autosavingFileType
  , autosavedContentsFileURL
  , setAutosavedContentsFileURL
  , locked
  , printInfo
  , setPrintInfo
  , allowsDocumentSharing
  , documentEdited
  , inViewingMode
  , undoManager
  , setUndoManager
  , hasUndoManager
  , setHasUndoManager
  , windowNibName
  , windowControllers
  , displayName
  , windowForSheet
  , readableTypes
  , writableTypes
  , usesUbiquitousStorage
  , lastComponentOfFileName
  , setLastComponentOfFileName
  , objectSpecifier
  , shouldRunSavePanelWithAccessoryView
  , initSelector
  , initWithType_errorSelector
  , canConcurrentlyReadDocumentsOfTypeSelector
  , initWithContentsOfURL_ofType_errorSelector
  , initForURL_withContentsOfURL_ofType_errorSelector
  , performActivityWithSynchronousWaiting_usingBlockSelector
  , continueActivityUsingBlockSelector
  , continueAsynchronousWorkOnMainThreadUsingBlockSelector
  , performSynchronousFileAccessUsingBlockSelector
  , performAsynchronousFileAccessUsingBlockSelector
  , revertDocumentToSavedSelector
  , revertToContentsOfURL_ofType_errorSelector
  , readFromURL_ofType_errorSelector
  , readFromFileWrapper_ofType_errorSelector
  , readFromData_ofType_errorSelector
  , writeToURL_ofType_errorSelector
  , fileWrapperOfType_errorSelector
  , dataOfType_errorSelector
  , unblockUserInteractionSelector
  , writeSafelyToURL_ofType_forSaveOperation_errorSelector
  , writeToURL_ofType_forSaveOperation_originalContentsURL_errorSelector
  , fileAttributesToWriteToURL_ofType_forSaveOperation_originalContentsURL_errorSelector
  , saveDocumentSelector
  , saveDocumentAsSelector
  , saveDocumentToSelector
  , saveDocumentWithDelegate_didSaveSelector_contextInfoSelector
  , runModalSavePanelForSaveOperation_delegate_didSaveSelector_contextInfoSelector
  , prepareSavePanelSelector
  , saveToURL_ofType_forSaveOperation_delegate_didSaveSelector_contextInfoSelector
  , saveToURL_ofType_forSaveOperation_completionHandlerSelector
  , canAsynchronouslyWriteToURL_ofType_forSaveOperationSelector
  , checkAutosavingSafetyAndReturnErrorSelector
  , scheduleAutosavingSelector
  , autosaveDocumentWithDelegate_didAutosaveSelector_contextInfoSelector
  , autosaveWithImplicitCancellability_completionHandlerSelector
  , browseDocumentVersionsSelector
  , stopBrowsingVersionsWithCompletionHandlerSelector
  , canCloseDocumentWithDelegate_shouldCloseSelector_contextInfoSelector
  , closeSelector
  , duplicateDocumentSelector
  , duplicateDocumentWithDelegate_didDuplicateSelector_contextInfoSelector
  , duplicateAndReturnErrorSelector
  , renameDocumentSelector
  , moveDocumentToUbiquityContainerSelector
  , moveDocumentSelector
  , moveDocumentWithCompletionHandlerSelector
  , moveToURL_completionHandlerSelector
  , lockDocumentSelector
  , unlockDocumentSelector
  , lockDocumentWithCompletionHandlerSelector
  , lockWithCompletionHandlerSelector
  , unlockDocumentWithCompletionHandlerSelector
  , unlockWithCompletionHandlerSelector
  , runPageLayoutSelector
  , runModalPageLayoutWithPrintInfo_delegate_didRunSelector_contextInfoSelector
  , preparePageLayoutSelector
  , shouldChangePrintInfoSelector
  , printDocumentSelector
  , printDocumentWithSettings_showPrintPanel_delegate_didPrintSelector_contextInfoSelector
  , printOperationWithSettings_errorSelector
  , runModalPrintOperation_delegate_didRunSelector_contextInfoSelector
  , saveDocumentToPDFSelector
  , shareDocumentWithSharingService_completionHandlerSelector
  , prepareSharingServicePickerSelector
  , updateChangeCountSelector
  , changeCountTokenForSaveOperationSelector
  , updateChangeCountWithToken_forSaveOperationSelector
  , presentError_modalForWindow_delegate_didPresentSelector_contextInfoSelector
  , presentErrorSelector
  , willPresentErrorSelector
  , willNotPresentErrorSelector
  , makeWindowControllersSelector
  , windowControllerWillLoadNibSelector
  , windowControllerDidLoadNibSelector
  , setWindowSelector
  , addWindowControllerSelector
  , removeWindowControllerSelector
  , showWindowsSelector
  , shouldCloseWindowController_delegate_shouldCloseSelector_contextInfoSelector
  , setDisplayNameSelector
  , defaultDraftNameSelector
  , isNativeTypeSelector
  , writableTypesForSaveOperationSelector
  , fileNameExtensionForType_saveOperationSelector
  , validateUserInterfaceItemSelector
  , relinquishPresentedItemToReaderSelector
  , relinquishPresentedItemToWriterSelector
  , savePresentedItemChangesWithCompletionHandlerSelector
  , accommodatePresentedItemDeletionWithCompletionHandlerSelector
  , presentedItemDidMoveToURLSelector
  , presentedItemDidChangeSelector
  , presentedItemDidChangeUbiquityAttributesSelector
  , presentedItemDidGainVersionSelector
  , presentedItemDidLoseVersionSelector
  , presentedItemDidResolveConflictVersionSelector
  , restoreDocumentWindowWithIdentifier_state_completionHandlerSelector
  , encodeRestorableStateWithCoderSelector
  , encodeRestorableStateWithCoder_backgroundQueueSelector
  , restoreStateWithCoderSelector
  , invalidateRestorableStateSelector
  , allowedClassesForRestorableStateKeyPathSelector
  , handleSaveScriptCommandSelector
  , handleCloseScriptCommandSelector
  , handlePrintScriptCommandSelector
  , updateUserActivityStateSelector
  , saveToURL_ofType_forSaveOperation_errorSelector
  , dataRepresentationOfTypeSelector
  , fileAttributesToWriteToFile_ofType_saveOperationSelector
  , fileNameSelector
  , fileWrapperRepresentationOfTypeSelector
  , initWithContentsOfFile_ofTypeSelector
  , initWithContentsOfURL_ofTypeSelector
  , loadDataRepresentation_ofTypeSelector
  , loadFileWrapperRepresentation_ofTypeSelector
  , printShowingPrintPanelSelector
  , readFromFile_ofTypeSelector
  , readFromURL_ofTypeSelector
  , revertToSavedFromFile_ofTypeSelector
  , revertToSavedFromURL_ofTypeSelector
  , runModalPageLayoutWithPrintInfoSelector
  , saveToFile_saveOperation_delegate_didSaveSelector_contextInfoSelector
  , setFileNameSelector
  , writeToFile_ofTypeSelector
  , writeToFile_ofType_originalFile_saveOperationSelector
  , writeToURL_ofTypeSelector
  , writeWithBackupToFile_ofType_saveOperationSelector
  , fileTypeSelector
  , setFileTypeSelector
  , fileURLSelector
  , setFileURLSelector
  , fileModificationDateSelector
  , setFileModificationDateSelector
  , draftSelector
  , setDraftSelector
  , entireFileLoadedSelector
  , autosavingIsImplicitlyCancellableSelector
  , keepBackupFileSelector
  , savePanelShowsFileFormatsControlSelector
  , fileNameExtensionWasHiddenInLastRunSavePanelSelector
  , fileTypeFromLastRunSavePanelSelector
  , hasUnautosavedChangesSelector
  , autosavesInPlaceSelector
  , preservesVersionsSelector
  , browsingVersionsSelector
  , autosavesDraftsSelector
  , autosavingFileTypeSelector
  , autosavedContentsFileURLSelector
  , setAutosavedContentsFileURLSelector
  , lockedSelector
  , printInfoSelector
  , setPrintInfoSelector
  , allowsDocumentSharingSelector
  , documentEditedSelector
  , inViewingModeSelector
  , undoManagerSelector
  , setUndoManagerSelector
  , hasUndoManagerSelector
  , setHasUndoManagerSelector
  , windowNibNameSelector
  , windowControllersSelector
  , displayNameSelector
  , windowForSheetSelector
  , readableTypesSelector
  , writableTypesSelector
  , usesUbiquitousStorageSelector
  , lastComponentOfFileNameSelector
  , setLastComponentOfFileNameSelector
  , objectSpecifierSelector
  , shouldRunSavePanelWithAccessoryViewSelector

  -- * Enum types
  , NSDocumentChangeType(NSDocumentChangeType)
  , pattern NSChangeDone
  , pattern NSChangeUndone
  , pattern NSChangeRedone
  , pattern NSChangeCleared
  , pattern NSChangeReadOtherContents
  , pattern NSChangeAutosaved
  , pattern NSChangeDiscardable
  , NSSaveOperationType(NSSaveOperationType)
  , pattern NSSaveOperation
  , pattern NSSaveAsOperation
  , pattern NSSaveToOperation
  , pattern NSAutosaveInPlaceOperation
  , pattern NSAutosaveElsewhereOperation
  , pattern NSAutosaveAsOperation
  , pattern NSAutosaveOperation

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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSDocument nsDocument => nsDocument -> IO (Id NSDocument)
init_ nsDocument  =
  sendMsg nsDocument (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithType:error:@
initWithType_error :: (IsNSDocument nsDocument, IsNSString typeName, IsNSError outError) => nsDocument -> typeName -> outError -> IO (Id NSDocument)
initWithType_error nsDocument  typeName outError =
withObjCPtr typeName $ \raw_typeName ->
  withObjCPtr outError $ \raw_outError ->
      sendMsg nsDocument (mkSelector "initWithType:error:") (retPtr retVoid) [argPtr (castPtr raw_typeName :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | @+ canConcurrentlyReadDocumentsOfType:@
canConcurrentlyReadDocumentsOfType :: IsNSString typeName => typeName -> IO Bool
canConcurrentlyReadDocumentsOfType typeName =
  do
    cls' <- getRequiredClass "NSDocument"
    withObjCPtr typeName $ \raw_typeName ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "canConcurrentlyReadDocumentsOfType:") retCULong [argPtr (castPtr raw_typeName :: Ptr ())]

-- | @- initWithContentsOfURL:ofType:error:@
initWithContentsOfURL_ofType_error :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName, IsNSError outError) => nsDocument -> url -> typeName -> outError -> IO (Id NSDocument)
initWithContentsOfURL_ofType_error nsDocument  url typeName outError =
withObjCPtr url $ \raw_url ->
  withObjCPtr typeName $ \raw_typeName ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg nsDocument (mkSelector "initWithContentsOfURL:ofType:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_typeName :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | @- initForURL:withContentsOfURL:ofType:error:@
initForURL_withContentsOfURL_ofType_error :: (IsNSDocument nsDocument, IsNSURL urlOrNil, IsNSURL contentsURL, IsNSString typeName, IsNSError outError) => nsDocument -> urlOrNil -> contentsURL -> typeName -> outError -> IO (Id NSDocument)
initForURL_withContentsOfURL_ofType_error nsDocument  urlOrNil contentsURL typeName outError =
withObjCPtr urlOrNil $ \raw_urlOrNil ->
  withObjCPtr contentsURL $ \raw_contentsURL ->
    withObjCPtr typeName $ \raw_typeName ->
      withObjCPtr outError $ \raw_outError ->
          sendMsg nsDocument (mkSelector "initForURL:withContentsOfURL:ofType:error:") (retPtr retVoid) [argPtr (castPtr raw_urlOrNil :: Ptr ()), argPtr (castPtr raw_contentsURL :: Ptr ()), argPtr (castPtr raw_typeName :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | @- performActivityWithSynchronousWaiting:usingBlock:@
performActivityWithSynchronousWaiting_usingBlock :: IsNSDocument nsDocument => nsDocument -> Bool -> Ptr () -> IO ()
performActivityWithSynchronousWaiting_usingBlock nsDocument  waitSynchronously block =
  sendMsg nsDocument (mkSelector "performActivityWithSynchronousWaiting:usingBlock:") retVoid [argCULong (if waitSynchronously then 1 else 0), argPtr (castPtr block :: Ptr ())]

-- | @- continueActivityUsingBlock:@
continueActivityUsingBlock :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
continueActivityUsingBlock nsDocument  block =
  sendMsg nsDocument (mkSelector "continueActivityUsingBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @- continueAsynchronousWorkOnMainThreadUsingBlock:@
continueAsynchronousWorkOnMainThreadUsingBlock :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
continueAsynchronousWorkOnMainThreadUsingBlock nsDocument  block =
  sendMsg nsDocument (mkSelector "continueAsynchronousWorkOnMainThreadUsingBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @- performSynchronousFileAccessUsingBlock:@
performSynchronousFileAccessUsingBlock :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
performSynchronousFileAccessUsingBlock nsDocument  block =
  sendMsg nsDocument (mkSelector "performSynchronousFileAccessUsingBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @- performAsynchronousFileAccessUsingBlock:@
performAsynchronousFileAccessUsingBlock :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
performAsynchronousFileAccessUsingBlock nsDocument  block =
  sendMsg nsDocument (mkSelector "performAsynchronousFileAccessUsingBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @- revertDocumentToSaved:@
revertDocumentToSaved :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
revertDocumentToSaved nsDocument  sender =
  sendMsg nsDocument (mkSelector "revertDocumentToSaved:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- revertToContentsOfURL:ofType:error:@
revertToContentsOfURL_ofType_error :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName, IsNSError outError) => nsDocument -> url -> typeName -> outError -> IO Bool
revertToContentsOfURL_ofType_error nsDocument  url typeName outError =
withObjCPtr url $ \raw_url ->
  withObjCPtr typeName $ \raw_typeName ->
    withObjCPtr outError $ \raw_outError ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "revertToContentsOfURL:ofType:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_typeName :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- readFromURL:ofType:error:@
readFromURL_ofType_error :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName, IsNSError outError) => nsDocument -> url -> typeName -> outError -> IO Bool
readFromURL_ofType_error nsDocument  url typeName outError =
withObjCPtr url $ \raw_url ->
  withObjCPtr typeName $ \raw_typeName ->
    withObjCPtr outError $ \raw_outError ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "readFromURL:ofType:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_typeName :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- readFromFileWrapper:ofType:error:@
readFromFileWrapper_ofType_error :: (IsNSDocument nsDocument, IsNSFileWrapper fileWrapper, IsNSString typeName, IsNSError outError) => nsDocument -> fileWrapper -> typeName -> outError -> IO Bool
readFromFileWrapper_ofType_error nsDocument  fileWrapper typeName outError =
withObjCPtr fileWrapper $ \raw_fileWrapper ->
  withObjCPtr typeName $ \raw_typeName ->
    withObjCPtr outError $ \raw_outError ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "readFromFileWrapper:ofType:error:") retCULong [argPtr (castPtr raw_fileWrapper :: Ptr ()), argPtr (castPtr raw_typeName :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- readFromData:ofType:error:@
readFromData_ofType_error :: (IsNSDocument nsDocument, IsNSData data_, IsNSString typeName, IsNSError outError) => nsDocument -> data_ -> typeName -> outError -> IO Bool
readFromData_ofType_error nsDocument  data_ typeName outError =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr typeName $ \raw_typeName ->
    withObjCPtr outError $ \raw_outError ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "readFromData:ofType:error:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_typeName :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- writeToURL:ofType:error:@
writeToURL_ofType_error :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName, IsNSError outError) => nsDocument -> url -> typeName -> outError -> IO Bool
writeToURL_ofType_error nsDocument  url typeName outError =
withObjCPtr url $ \raw_url ->
  withObjCPtr typeName $ \raw_typeName ->
    withObjCPtr outError $ \raw_outError ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "writeToURL:ofType:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_typeName :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- fileWrapperOfType:error:@
fileWrapperOfType_error :: (IsNSDocument nsDocument, IsNSString typeName, IsNSError outError) => nsDocument -> typeName -> outError -> IO (Id NSFileWrapper)
fileWrapperOfType_error nsDocument  typeName outError =
withObjCPtr typeName $ \raw_typeName ->
  withObjCPtr outError $ \raw_outError ->
      sendMsg nsDocument (mkSelector "fileWrapperOfType:error:") (retPtr retVoid) [argPtr (castPtr raw_typeName :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | @- dataOfType:error:@
dataOfType_error :: (IsNSDocument nsDocument, IsNSString typeName, IsNSError outError) => nsDocument -> typeName -> outError -> IO (Id NSData)
dataOfType_error nsDocument  typeName outError =
withObjCPtr typeName $ \raw_typeName ->
  withObjCPtr outError $ \raw_outError ->
      sendMsg nsDocument (mkSelector "dataOfType:error:") (retPtr retVoid) [argPtr (castPtr raw_typeName :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | @- unblockUserInteraction@
unblockUserInteraction :: IsNSDocument nsDocument => nsDocument -> IO ()
unblockUserInteraction nsDocument  =
  sendMsg nsDocument (mkSelector "unblockUserInteraction") retVoid []

-- | @- writeSafelyToURL:ofType:forSaveOperation:error:@
writeSafelyToURL_ofType_forSaveOperation_error :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName, IsNSError outError) => nsDocument -> url -> typeName -> NSSaveOperationType -> outError -> IO Bool
writeSafelyToURL_ofType_forSaveOperation_error nsDocument  url typeName saveOperation outError =
withObjCPtr url $ \raw_url ->
  withObjCPtr typeName $ \raw_typeName ->
    withObjCPtr outError $ \raw_outError ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "writeSafelyToURL:ofType:forSaveOperation:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_typeName :: Ptr ()), argCULong (coerce saveOperation), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- writeToURL:ofType:forSaveOperation:originalContentsURL:error:@
writeToURL_ofType_forSaveOperation_originalContentsURL_error :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName, IsNSURL absoluteOriginalContentsURL, IsNSError outError) => nsDocument -> url -> typeName -> NSSaveOperationType -> absoluteOriginalContentsURL -> outError -> IO Bool
writeToURL_ofType_forSaveOperation_originalContentsURL_error nsDocument  url typeName saveOperation absoluteOriginalContentsURL outError =
withObjCPtr url $ \raw_url ->
  withObjCPtr typeName $ \raw_typeName ->
    withObjCPtr absoluteOriginalContentsURL $ \raw_absoluteOriginalContentsURL ->
      withObjCPtr outError $ \raw_outError ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "writeToURL:ofType:forSaveOperation:originalContentsURL:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_typeName :: Ptr ()), argCULong (coerce saveOperation), argPtr (castPtr raw_absoluteOriginalContentsURL :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- fileAttributesToWriteToURL:ofType:forSaveOperation:originalContentsURL:error:@
fileAttributesToWriteToURL_ofType_forSaveOperation_originalContentsURL_error :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName, IsNSURL absoluteOriginalContentsURL, IsNSError outError) => nsDocument -> url -> typeName -> NSSaveOperationType -> absoluteOriginalContentsURL -> outError -> IO (Id NSDictionary)
fileAttributesToWriteToURL_ofType_forSaveOperation_originalContentsURL_error nsDocument  url typeName saveOperation absoluteOriginalContentsURL outError =
withObjCPtr url $ \raw_url ->
  withObjCPtr typeName $ \raw_typeName ->
    withObjCPtr absoluteOriginalContentsURL $ \raw_absoluteOriginalContentsURL ->
      withObjCPtr outError $ \raw_outError ->
          sendMsg nsDocument (mkSelector "fileAttributesToWriteToURL:ofType:forSaveOperation:originalContentsURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_typeName :: Ptr ()), argCULong (coerce saveOperation), argPtr (castPtr raw_absoluteOriginalContentsURL :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | @- saveDocument:@
saveDocument :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
saveDocument nsDocument  sender =
  sendMsg nsDocument (mkSelector "saveDocument:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- saveDocumentAs:@
saveDocumentAs :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
saveDocumentAs nsDocument  sender =
  sendMsg nsDocument (mkSelector "saveDocumentAs:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- saveDocumentTo:@
saveDocumentTo :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
saveDocumentTo nsDocument  sender =
  sendMsg nsDocument (mkSelector "saveDocumentTo:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- saveDocumentWithDelegate:didSaveSelector:contextInfo:@
saveDocumentWithDelegate_didSaveSelector_contextInfo :: IsNSDocument nsDocument => nsDocument -> RawId -> Selector -> Ptr () -> IO ()
saveDocumentWithDelegate_didSaveSelector_contextInfo nsDocument  delegate didSaveSelector contextInfo =
  sendMsg nsDocument (mkSelector "saveDocumentWithDelegate:didSaveSelector:contextInfo:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didSaveSelector), argPtr contextInfo]

-- | @- runModalSavePanelForSaveOperation:delegate:didSaveSelector:contextInfo:@
runModalSavePanelForSaveOperation_delegate_didSaveSelector_contextInfo :: IsNSDocument nsDocument => nsDocument -> NSSaveOperationType -> RawId -> Selector -> Ptr () -> IO ()
runModalSavePanelForSaveOperation_delegate_didSaveSelector_contextInfo nsDocument  saveOperation delegate didSaveSelector contextInfo =
  sendMsg nsDocument (mkSelector "runModalSavePanelForSaveOperation:delegate:didSaveSelector:contextInfo:") retVoid [argCULong (coerce saveOperation), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didSaveSelector), argPtr contextInfo]

-- | @- prepareSavePanel:@
prepareSavePanel :: (IsNSDocument nsDocument, IsNSSavePanel savePanel) => nsDocument -> savePanel -> IO Bool
prepareSavePanel nsDocument  savePanel =
withObjCPtr savePanel $ \raw_savePanel ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "prepareSavePanel:") retCULong [argPtr (castPtr raw_savePanel :: Ptr ())]

-- | @- saveToURL:ofType:forSaveOperation:delegate:didSaveSelector:contextInfo:@
saveToURL_ofType_forSaveOperation_delegate_didSaveSelector_contextInfo :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName) => nsDocument -> url -> typeName -> NSSaveOperationType -> RawId -> Selector -> Ptr () -> IO ()
saveToURL_ofType_forSaveOperation_delegate_didSaveSelector_contextInfo nsDocument  url typeName saveOperation delegate didSaveSelector contextInfo =
withObjCPtr url $ \raw_url ->
  withObjCPtr typeName $ \raw_typeName ->
      sendMsg nsDocument (mkSelector "saveToURL:ofType:forSaveOperation:delegate:didSaveSelector:contextInfo:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_typeName :: Ptr ()), argCULong (coerce saveOperation), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didSaveSelector), argPtr contextInfo]

-- | @- saveToURL:ofType:forSaveOperation:completionHandler:@
saveToURL_ofType_forSaveOperation_completionHandler :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName) => nsDocument -> url -> typeName -> NSSaveOperationType -> Ptr () -> IO ()
saveToURL_ofType_forSaveOperation_completionHandler nsDocument  url typeName saveOperation completionHandler =
withObjCPtr url $ \raw_url ->
  withObjCPtr typeName $ \raw_typeName ->
      sendMsg nsDocument (mkSelector "saveToURL:ofType:forSaveOperation:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_typeName :: Ptr ()), argCULong (coerce saveOperation), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- canAsynchronouslyWriteToURL:ofType:forSaveOperation:@
canAsynchronouslyWriteToURL_ofType_forSaveOperation :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName) => nsDocument -> url -> typeName -> NSSaveOperationType -> IO Bool
canAsynchronouslyWriteToURL_ofType_forSaveOperation nsDocument  url typeName saveOperation =
withObjCPtr url $ \raw_url ->
  withObjCPtr typeName $ \raw_typeName ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "canAsynchronouslyWriteToURL:ofType:forSaveOperation:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_typeName :: Ptr ()), argCULong (coerce saveOperation)]

-- | @- checkAutosavingSafetyAndReturnError:@
checkAutosavingSafetyAndReturnError :: (IsNSDocument nsDocument, IsNSError outError) => nsDocument -> outError -> IO Bool
checkAutosavingSafetyAndReturnError nsDocument  outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "checkAutosavingSafetyAndReturnError:") retCULong [argPtr (castPtr raw_outError :: Ptr ())]

-- | @- scheduleAutosaving@
scheduleAutosaving :: IsNSDocument nsDocument => nsDocument -> IO ()
scheduleAutosaving nsDocument  =
  sendMsg nsDocument (mkSelector "scheduleAutosaving") retVoid []

-- | @- autosaveDocumentWithDelegate:didAutosaveSelector:contextInfo:@
autosaveDocumentWithDelegate_didAutosaveSelector_contextInfo :: IsNSDocument nsDocument => nsDocument -> RawId -> Selector -> Ptr () -> IO ()
autosaveDocumentWithDelegate_didAutosaveSelector_contextInfo nsDocument  delegate didAutosaveSelector contextInfo =
  sendMsg nsDocument (mkSelector "autosaveDocumentWithDelegate:didAutosaveSelector:contextInfo:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didAutosaveSelector), argPtr contextInfo]

-- | @- autosaveWithImplicitCancellability:completionHandler:@
autosaveWithImplicitCancellability_completionHandler :: IsNSDocument nsDocument => nsDocument -> Bool -> Ptr () -> IO ()
autosaveWithImplicitCancellability_completionHandler nsDocument  autosavingIsImplicitlyCancellable completionHandler =
  sendMsg nsDocument (mkSelector "autosaveWithImplicitCancellability:completionHandler:") retVoid [argCULong (if autosavingIsImplicitlyCancellable then 1 else 0), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- browseDocumentVersions:@
browseDocumentVersions :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
browseDocumentVersions nsDocument  sender =
  sendMsg nsDocument (mkSelector "browseDocumentVersions:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- stopBrowsingVersionsWithCompletionHandler:@
stopBrowsingVersionsWithCompletionHandler :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
stopBrowsingVersionsWithCompletionHandler nsDocument  completionHandler =
  sendMsg nsDocument (mkSelector "stopBrowsingVersionsWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- canCloseDocumentWithDelegate:shouldCloseSelector:contextInfo:@
canCloseDocumentWithDelegate_shouldCloseSelector_contextInfo :: IsNSDocument nsDocument => nsDocument -> RawId -> Selector -> Ptr () -> IO ()
canCloseDocumentWithDelegate_shouldCloseSelector_contextInfo nsDocument  delegate shouldCloseSelector contextInfo =
  sendMsg nsDocument (mkSelector "canCloseDocumentWithDelegate:shouldCloseSelector:contextInfo:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector shouldCloseSelector), argPtr contextInfo]

-- | @- close@
close :: IsNSDocument nsDocument => nsDocument -> IO ()
close nsDocument  =
  sendMsg nsDocument (mkSelector "close") retVoid []

-- | @- duplicateDocument:@
duplicateDocument :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
duplicateDocument nsDocument  sender =
  sendMsg nsDocument (mkSelector "duplicateDocument:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- duplicateDocumentWithDelegate:didDuplicateSelector:contextInfo:@
duplicateDocumentWithDelegate_didDuplicateSelector_contextInfo :: IsNSDocument nsDocument => nsDocument -> RawId -> Selector -> Ptr () -> IO ()
duplicateDocumentWithDelegate_didDuplicateSelector_contextInfo nsDocument  delegate didDuplicateSelector contextInfo =
  sendMsg nsDocument (mkSelector "duplicateDocumentWithDelegate:didDuplicateSelector:contextInfo:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didDuplicateSelector), argPtr contextInfo]

-- | @- duplicateAndReturnError:@
duplicateAndReturnError :: (IsNSDocument nsDocument, IsNSError outError) => nsDocument -> outError -> IO (Id NSDocument)
duplicateAndReturnError nsDocument  outError =
withObjCPtr outError $ \raw_outError ->
    sendMsg nsDocument (mkSelector "duplicateAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | @- renameDocument:@
renameDocument :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
renameDocument nsDocument  sender =
  sendMsg nsDocument (mkSelector "renameDocument:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- moveDocumentToUbiquityContainer:@
moveDocumentToUbiquityContainer :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
moveDocumentToUbiquityContainer nsDocument  sender =
  sendMsg nsDocument (mkSelector "moveDocumentToUbiquityContainer:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- moveDocument:@
moveDocument :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
moveDocument nsDocument  sender =
  sendMsg nsDocument (mkSelector "moveDocument:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- moveDocumentWithCompletionHandler:@
moveDocumentWithCompletionHandler :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
moveDocumentWithCompletionHandler nsDocument  completionHandler =
  sendMsg nsDocument (mkSelector "moveDocumentWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- moveToURL:completionHandler:@
moveToURL_completionHandler :: (IsNSDocument nsDocument, IsNSURL url) => nsDocument -> url -> Ptr () -> IO ()
moveToURL_completionHandler nsDocument  url completionHandler =
withObjCPtr url $ \raw_url ->
    sendMsg nsDocument (mkSelector "moveToURL:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- lockDocument:@
lockDocument :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
lockDocument nsDocument  sender =
  sendMsg nsDocument (mkSelector "lockDocument:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- unlockDocument:@
unlockDocument :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
unlockDocument nsDocument  sender =
  sendMsg nsDocument (mkSelector "unlockDocument:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- lockDocumentWithCompletionHandler:@
lockDocumentWithCompletionHandler :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
lockDocumentWithCompletionHandler nsDocument  completionHandler =
  sendMsg nsDocument (mkSelector "lockDocumentWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- lockWithCompletionHandler:@
lockWithCompletionHandler :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
lockWithCompletionHandler nsDocument  completionHandler =
  sendMsg nsDocument (mkSelector "lockWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- unlockDocumentWithCompletionHandler:@
unlockDocumentWithCompletionHandler :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
unlockDocumentWithCompletionHandler nsDocument  completionHandler =
  sendMsg nsDocument (mkSelector "unlockDocumentWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- unlockWithCompletionHandler:@
unlockWithCompletionHandler :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
unlockWithCompletionHandler nsDocument  completionHandler =
  sendMsg nsDocument (mkSelector "unlockWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- runPageLayout:@
runPageLayout :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
runPageLayout nsDocument  sender =
  sendMsg nsDocument (mkSelector "runPageLayout:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- runModalPageLayoutWithPrintInfo:delegate:didRunSelector:contextInfo:@
runModalPageLayoutWithPrintInfo_delegate_didRunSelector_contextInfo :: (IsNSDocument nsDocument, IsNSPrintInfo printInfo) => nsDocument -> printInfo -> RawId -> Selector -> Ptr () -> IO ()
runModalPageLayoutWithPrintInfo_delegate_didRunSelector_contextInfo nsDocument  printInfo delegate didRunSelector contextInfo =
withObjCPtr printInfo $ \raw_printInfo ->
    sendMsg nsDocument (mkSelector "runModalPageLayoutWithPrintInfo:delegate:didRunSelector:contextInfo:") retVoid [argPtr (castPtr raw_printInfo :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didRunSelector), argPtr contextInfo]

-- | @- preparePageLayout:@
preparePageLayout :: (IsNSDocument nsDocument, IsNSPageLayout pageLayout) => nsDocument -> pageLayout -> IO Bool
preparePageLayout nsDocument  pageLayout =
withObjCPtr pageLayout $ \raw_pageLayout ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "preparePageLayout:") retCULong [argPtr (castPtr raw_pageLayout :: Ptr ())]

-- | @- shouldChangePrintInfo:@
shouldChangePrintInfo :: (IsNSDocument nsDocument, IsNSPrintInfo newPrintInfo) => nsDocument -> newPrintInfo -> IO Bool
shouldChangePrintInfo nsDocument  newPrintInfo =
withObjCPtr newPrintInfo $ \raw_newPrintInfo ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "shouldChangePrintInfo:") retCULong [argPtr (castPtr raw_newPrintInfo :: Ptr ())]

-- | @- printDocument:@
printDocument :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
printDocument nsDocument  sender =
  sendMsg nsDocument (mkSelector "printDocument:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- printDocumentWithSettings:showPrintPanel:delegate:didPrintSelector:contextInfo:@
printDocumentWithSettings_showPrintPanel_delegate_didPrintSelector_contextInfo :: (IsNSDocument nsDocument, IsNSDictionary printSettings) => nsDocument -> printSettings -> Bool -> RawId -> Selector -> Ptr () -> IO ()
printDocumentWithSettings_showPrintPanel_delegate_didPrintSelector_contextInfo nsDocument  printSettings showPrintPanel delegate didPrintSelector contextInfo =
withObjCPtr printSettings $ \raw_printSettings ->
    sendMsg nsDocument (mkSelector "printDocumentWithSettings:showPrintPanel:delegate:didPrintSelector:contextInfo:") retVoid [argPtr (castPtr raw_printSettings :: Ptr ()), argCULong (if showPrintPanel then 1 else 0), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didPrintSelector), argPtr contextInfo]

-- | @- printOperationWithSettings:error:@
printOperationWithSettings_error :: (IsNSDocument nsDocument, IsNSDictionary printSettings, IsNSError outError) => nsDocument -> printSettings -> outError -> IO (Id NSPrintOperation)
printOperationWithSettings_error nsDocument  printSettings outError =
withObjCPtr printSettings $ \raw_printSettings ->
  withObjCPtr outError $ \raw_outError ->
      sendMsg nsDocument (mkSelector "printOperationWithSettings:error:") (retPtr retVoid) [argPtr (castPtr raw_printSettings :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | @- runModalPrintOperation:delegate:didRunSelector:contextInfo:@
runModalPrintOperation_delegate_didRunSelector_contextInfo :: (IsNSDocument nsDocument, IsNSPrintOperation printOperation) => nsDocument -> printOperation -> RawId -> Selector -> Ptr () -> IO ()
runModalPrintOperation_delegate_didRunSelector_contextInfo nsDocument  printOperation delegate didRunSelector contextInfo =
withObjCPtr printOperation $ \raw_printOperation ->
    sendMsg nsDocument (mkSelector "runModalPrintOperation:delegate:didRunSelector:contextInfo:") retVoid [argPtr (castPtr raw_printOperation :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didRunSelector), argPtr contextInfo]

-- | @- saveDocumentToPDF:@
saveDocumentToPDF :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
saveDocumentToPDF nsDocument  sender =
  sendMsg nsDocument (mkSelector "saveDocumentToPDF:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- shareDocumentWithSharingService:completionHandler:@
shareDocumentWithSharingService_completionHandler :: (IsNSDocument nsDocument, IsNSSharingService sharingService) => nsDocument -> sharingService -> Ptr () -> IO ()
shareDocumentWithSharingService_completionHandler nsDocument  sharingService completionHandler =
withObjCPtr sharingService $ \raw_sharingService ->
    sendMsg nsDocument (mkSelector "shareDocumentWithSharingService:completionHandler:") retVoid [argPtr (castPtr raw_sharingService :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- prepareSharingServicePicker:@
prepareSharingServicePicker :: (IsNSDocument nsDocument, IsNSSharingServicePicker sharingServicePicker) => nsDocument -> sharingServicePicker -> IO ()
prepareSharingServicePicker nsDocument  sharingServicePicker =
withObjCPtr sharingServicePicker $ \raw_sharingServicePicker ->
    sendMsg nsDocument (mkSelector "prepareSharingServicePicker:") retVoid [argPtr (castPtr raw_sharingServicePicker :: Ptr ())]

-- | @- updateChangeCount:@
updateChangeCount :: IsNSDocument nsDocument => nsDocument -> NSDocumentChangeType -> IO ()
updateChangeCount nsDocument  change =
  sendMsg nsDocument (mkSelector "updateChangeCount:") retVoid [argCULong (coerce change)]

-- | @- changeCountTokenForSaveOperation:@
changeCountTokenForSaveOperation :: IsNSDocument nsDocument => nsDocument -> NSSaveOperationType -> IO RawId
changeCountTokenForSaveOperation nsDocument  saveOperation =
  fmap (RawId . castPtr) $ sendMsg nsDocument (mkSelector "changeCountTokenForSaveOperation:") (retPtr retVoid) [argCULong (coerce saveOperation)]

-- | @- updateChangeCountWithToken:forSaveOperation:@
updateChangeCountWithToken_forSaveOperation :: IsNSDocument nsDocument => nsDocument -> RawId -> NSSaveOperationType -> IO ()
updateChangeCountWithToken_forSaveOperation nsDocument  changeCountToken saveOperation =
  sendMsg nsDocument (mkSelector "updateChangeCountWithToken:forSaveOperation:") retVoid [argPtr (castPtr (unRawId changeCountToken) :: Ptr ()), argCULong (coerce saveOperation)]

-- | @- presentError:modalForWindow:delegate:didPresentSelector:contextInfo:@
presentError_modalForWindow_delegate_didPresentSelector_contextInfo :: (IsNSDocument nsDocument, IsNSError error_, IsNSWindow window) => nsDocument -> error_ -> window -> RawId -> Selector -> Ptr () -> IO ()
presentError_modalForWindow_delegate_didPresentSelector_contextInfo nsDocument  error_ window delegate didPresentSelector contextInfo =
withObjCPtr error_ $ \raw_error_ ->
  withObjCPtr window $ \raw_window ->
      sendMsg nsDocument (mkSelector "presentError:modalForWindow:delegate:didPresentSelector:contextInfo:") retVoid [argPtr (castPtr raw_error_ :: Ptr ()), argPtr (castPtr raw_window :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didPresentSelector), argPtr contextInfo]

-- | @- presentError:@
presentError :: (IsNSDocument nsDocument, IsNSError error_) => nsDocument -> error_ -> IO Bool
presentError nsDocument  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "presentError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- willPresentError:@
willPresentError :: (IsNSDocument nsDocument, IsNSError error_) => nsDocument -> error_ -> IO (Id NSError)
willPresentError nsDocument  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg nsDocument (mkSelector "willPresentError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- willNotPresentError:@
willNotPresentError :: (IsNSDocument nsDocument, IsNSError error_) => nsDocument -> error_ -> IO ()
willNotPresentError nsDocument  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg nsDocument (mkSelector "willNotPresentError:") retVoid [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- makeWindowControllers@
makeWindowControllers :: IsNSDocument nsDocument => nsDocument -> IO ()
makeWindowControllers nsDocument  =
  sendMsg nsDocument (mkSelector "makeWindowControllers") retVoid []

-- | @- windowControllerWillLoadNib:@
windowControllerWillLoadNib :: (IsNSDocument nsDocument, IsNSWindowController windowController) => nsDocument -> windowController -> IO ()
windowControllerWillLoadNib nsDocument  windowController =
withObjCPtr windowController $ \raw_windowController ->
    sendMsg nsDocument (mkSelector "windowControllerWillLoadNib:") retVoid [argPtr (castPtr raw_windowController :: Ptr ())]

-- | @- windowControllerDidLoadNib:@
windowControllerDidLoadNib :: (IsNSDocument nsDocument, IsNSWindowController windowController) => nsDocument -> windowController -> IO ()
windowControllerDidLoadNib nsDocument  windowController =
withObjCPtr windowController $ \raw_windowController ->
    sendMsg nsDocument (mkSelector "windowControllerDidLoadNib:") retVoid [argPtr (castPtr raw_windowController :: Ptr ())]

-- | @- setWindow:@
setWindow :: (IsNSDocument nsDocument, IsNSWindow window) => nsDocument -> window -> IO ()
setWindow nsDocument  window =
withObjCPtr window $ \raw_window ->
    sendMsg nsDocument (mkSelector "setWindow:") retVoid [argPtr (castPtr raw_window :: Ptr ())]

-- | @- addWindowController:@
addWindowController :: (IsNSDocument nsDocument, IsNSWindowController windowController) => nsDocument -> windowController -> IO ()
addWindowController nsDocument  windowController =
withObjCPtr windowController $ \raw_windowController ->
    sendMsg nsDocument (mkSelector "addWindowController:") retVoid [argPtr (castPtr raw_windowController :: Ptr ())]

-- | @- removeWindowController:@
removeWindowController :: (IsNSDocument nsDocument, IsNSWindowController windowController) => nsDocument -> windowController -> IO ()
removeWindowController nsDocument  windowController =
withObjCPtr windowController $ \raw_windowController ->
    sendMsg nsDocument (mkSelector "removeWindowController:") retVoid [argPtr (castPtr raw_windowController :: Ptr ())]

-- | @- showWindows@
showWindows :: IsNSDocument nsDocument => nsDocument -> IO ()
showWindows nsDocument  =
  sendMsg nsDocument (mkSelector "showWindows") retVoid []

-- | @- shouldCloseWindowController:delegate:shouldCloseSelector:contextInfo:@
shouldCloseWindowController_delegate_shouldCloseSelector_contextInfo :: (IsNSDocument nsDocument, IsNSWindowController windowController) => nsDocument -> windowController -> RawId -> Selector -> Ptr () -> IO ()
shouldCloseWindowController_delegate_shouldCloseSelector_contextInfo nsDocument  windowController delegate shouldCloseSelector contextInfo =
withObjCPtr windowController $ \raw_windowController ->
    sendMsg nsDocument (mkSelector "shouldCloseWindowController:delegate:shouldCloseSelector:contextInfo:") retVoid [argPtr (castPtr raw_windowController :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector shouldCloseSelector), argPtr contextInfo]

-- | @- setDisplayName:@
setDisplayName :: (IsNSDocument nsDocument, IsNSString displayNameOrNil) => nsDocument -> displayNameOrNil -> IO ()
setDisplayName nsDocument  displayNameOrNil =
withObjCPtr displayNameOrNil $ \raw_displayNameOrNil ->
    sendMsg nsDocument (mkSelector "setDisplayName:") retVoid [argPtr (castPtr raw_displayNameOrNil :: Ptr ())]

-- | @- defaultDraftName@
defaultDraftName :: IsNSDocument nsDocument => nsDocument -> IO (Id NSString)
defaultDraftName nsDocument  =
  sendMsg nsDocument (mkSelector "defaultDraftName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ isNativeType:@
isNativeType :: IsNSString type_ => type_ -> IO Bool
isNativeType type_ =
  do
    cls' <- getRequiredClass "NSDocument"
    withObjCPtr type_ $ \raw_type_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isNativeType:") retCULong [argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- writableTypesForSaveOperation:@
writableTypesForSaveOperation :: IsNSDocument nsDocument => nsDocument -> NSSaveOperationType -> IO (Id NSArray)
writableTypesForSaveOperation nsDocument  saveOperation =
  sendMsg nsDocument (mkSelector "writableTypesForSaveOperation:") (retPtr retVoid) [argCULong (coerce saveOperation)] >>= retainedObject . castPtr

-- | @- fileNameExtensionForType:saveOperation:@
fileNameExtensionForType_saveOperation :: (IsNSDocument nsDocument, IsNSString typeName) => nsDocument -> typeName -> NSSaveOperationType -> IO (Id NSString)
fileNameExtensionForType_saveOperation nsDocument  typeName saveOperation =
withObjCPtr typeName $ \raw_typeName ->
    sendMsg nsDocument (mkSelector "fileNameExtensionForType:saveOperation:") (retPtr retVoid) [argPtr (castPtr raw_typeName :: Ptr ()), argCULong (coerce saveOperation)] >>= retainedObject . castPtr

-- | @- validateUserInterfaceItem:@
validateUserInterfaceItem :: IsNSDocument nsDocument => nsDocument -> RawId -> IO Bool
validateUserInterfaceItem nsDocument  item =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "validateUserInterfaceItem:") retCULong [argPtr (castPtr (unRawId item) :: Ptr ())]

-- | @- relinquishPresentedItemToReader:@
relinquishPresentedItemToReader :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
relinquishPresentedItemToReader nsDocument  reader =
  sendMsg nsDocument (mkSelector "relinquishPresentedItemToReader:") retVoid [argPtr (castPtr reader :: Ptr ())]

-- | @- relinquishPresentedItemToWriter:@
relinquishPresentedItemToWriter :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
relinquishPresentedItemToWriter nsDocument  writer =
  sendMsg nsDocument (mkSelector "relinquishPresentedItemToWriter:") retVoid [argPtr (castPtr writer :: Ptr ())]

-- | @- savePresentedItemChangesWithCompletionHandler:@
savePresentedItemChangesWithCompletionHandler :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
savePresentedItemChangesWithCompletionHandler nsDocument  completionHandler =
  sendMsg nsDocument (mkSelector "savePresentedItemChangesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- accommodatePresentedItemDeletionWithCompletionHandler:@
accommodatePresentedItemDeletionWithCompletionHandler :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
accommodatePresentedItemDeletionWithCompletionHandler nsDocument  completionHandler =
  sendMsg nsDocument (mkSelector "accommodatePresentedItemDeletionWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- presentedItemDidMoveToURL:@
presentedItemDidMoveToURL :: (IsNSDocument nsDocument, IsNSURL newURL) => nsDocument -> newURL -> IO ()
presentedItemDidMoveToURL nsDocument  newURL =
withObjCPtr newURL $ \raw_newURL ->
    sendMsg nsDocument (mkSelector "presentedItemDidMoveToURL:") retVoid [argPtr (castPtr raw_newURL :: Ptr ())]

-- | @- presentedItemDidChange@
presentedItemDidChange :: IsNSDocument nsDocument => nsDocument -> IO ()
presentedItemDidChange nsDocument  =
  sendMsg nsDocument (mkSelector "presentedItemDidChange") retVoid []

-- | @- presentedItemDidChangeUbiquityAttributes:@
presentedItemDidChangeUbiquityAttributes :: (IsNSDocument nsDocument, IsNSSet attributes) => nsDocument -> attributes -> IO ()
presentedItemDidChangeUbiquityAttributes nsDocument  attributes =
withObjCPtr attributes $ \raw_attributes ->
    sendMsg nsDocument (mkSelector "presentedItemDidChangeUbiquityAttributes:") retVoid [argPtr (castPtr raw_attributes :: Ptr ())]

-- | @- presentedItemDidGainVersion:@
presentedItemDidGainVersion :: (IsNSDocument nsDocument, IsNSFileVersion version) => nsDocument -> version -> IO ()
presentedItemDidGainVersion nsDocument  version =
withObjCPtr version $ \raw_version ->
    sendMsg nsDocument (mkSelector "presentedItemDidGainVersion:") retVoid [argPtr (castPtr raw_version :: Ptr ())]

-- | @- presentedItemDidLoseVersion:@
presentedItemDidLoseVersion :: (IsNSDocument nsDocument, IsNSFileVersion version) => nsDocument -> version -> IO ()
presentedItemDidLoseVersion nsDocument  version =
withObjCPtr version $ \raw_version ->
    sendMsg nsDocument (mkSelector "presentedItemDidLoseVersion:") retVoid [argPtr (castPtr raw_version :: Ptr ())]

-- | @- presentedItemDidResolveConflictVersion:@
presentedItemDidResolveConflictVersion :: (IsNSDocument nsDocument, IsNSFileVersion version) => nsDocument -> version -> IO ()
presentedItemDidResolveConflictVersion nsDocument  version =
withObjCPtr version $ \raw_version ->
    sendMsg nsDocument (mkSelector "presentedItemDidResolveConflictVersion:") retVoid [argPtr (castPtr raw_version :: Ptr ())]

-- | @- restoreDocumentWindowWithIdentifier:state:completionHandler:@
restoreDocumentWindowWithIdentifier_state_completionHandler :: (IsNSDocument nsDocument, IsNSString identifier, IsNSCoder state) => nsDocument -> identifier -> state -> Ptr () -> IO ()
restoreDocumentWindowWithIdentifier_state_completionHandler nsDocument  identifier state completionHandler =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr state $ \raw_state ->
      sendMsg nsDocument (mkSelector "restoreDocumentWindowWithIdentifier:state:completionHandler:") retVoid [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_state :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- encodeRestorableStateWithCoder:@
encodeRestorableStateWithCoder :: (IsNSDocument nsDocument, IsNSCoder coder) => nsDocument -> coder -> IO ()
encodeRestorableStateWithCoder nsDocument  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsDocument (mkSelector "encodeRestorableStateWithCoder:") retVoid [argPtr (castPtr raw_coder :: Ptr ())]

-- | @- encodeRestorableStateWithCoder:backgroundQueue:@
encodeRestorableStateWithCoder_backgroundQueue :: (IsNSDocument nsDocument, IsNSCoder coder, IsNSOperationQueue queue) => nsDocument -> coder -> queue -> IO ()
encodeRestorableStateWithCoder_backgroundQueue nsDocument  coder queue =
withObjCPtr coder $ \raw_coder ->
  withObjCPtr queue $ \raw_queue ->
      sendMsg nsDocument (mkSelector "encodeRestorableStateWithCoder:backgroundQueue:") retVoid [argPtr (castPtr raw_coder :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())]

-- | @- restoreStateWithCoder:@
restoreStateWithCoder :: (IsNSDocument nsDocument, IsNSCoder coder) => nsDocument -> coder -> IO ()
restoreStateWithCoder nsDocument  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsDocument (mkSelector "restoreStateWithCoder:") retVoid [argPtr (castPtr raw_coder :: Ptr ())]

-- | @- invalidateRestorableState@
invalidateRestorableState :: IsNSDocument nsDocument => nsDocument -> IO ()
invalidateRestorableState nsDocument  =
  sendMsg nsDocument (mkSelector "invalidateRestorableState") retVoid []

-- | @+ allowedClassesForRestorableStateKeyPath:@
allowedClassesForRestorableStateKeyPath :: IsNSString keyPath => keyPath -> IO (Id NSArray)
allowedClassesForRestorableStateKeyPath keyPath =
  do
    cls' <- getRequiredClass "NSDocument"
    withObjCPtr keyPath $ \raw_keyPath ->
      sendClassMsg cls' (mkSelector "allowedClassesForRestorableStateKeyPath:") (retPtr retVoid) [argPtr (castPtr raw_keyPath :: Ptr ())] >>= retainedObject . castPtr

-- | @- handleSaveScriptCommand:@
handleSaveScriptCommand :: (IsNSDocument nsDocument, IsNSScriptCommand command) => nsDocument -> command -> IO RawId
handleSaveScriptCommand nsDocument  command =
withObjCPtr command $ \raw_command ->
    fmap (RawId . castPtr) $ sendMsg nsDocument (mkSelector "handleSaveScriptCommand:") (retPtr retVoid) [argPtr (castPtr raw_command :: Ptr ())]

-- | @- handleCloseScriptCommand:@
handleCloseScriptCommand :: (IsNSDocument nsDocument, IsNSCloseCommand command) => nsDocument -> command -> IO RawId
handleCloseScriptCommand nsDocument  command =
withObjCPtr command $ \raw_command ->
    fmap (RawId . castPtr) $ sendMsg nsDocument (mkSelector "handleCloseScriptCommand:") (retPtr retVoid) [argPtr (castPtr raw_command :: Ptr ())]

-- | @- handlePrintScriptCommand:@
handlePrintScriptCommand :: (IsNSDocument nsDocument, IsNSScriptCommand command) => nsDocument -> command -> IO RawId
handlePrintScriptCommand nsDocument  command =
withObjCPtr command $ \raw_command ->
    fmap (RawId . castPtr) $ sendMsg nsDocument (mkSelector "handlePrintScriptCommand:") (retPtr retVoid) [argPtr (castPtr raw_command :: Ptr ())]

-- | @- updateUserActivityState:@
updateUserActivityState :: (IsNSDocument nsDocument, IsNSUserActivity activity) => nsDocument -> activity -> IO ()
updateUserActivityState nsDocument  activity =
withObjCPtr activity $ \raw_activity ->
    sendMsg nsDocument (mkSelector "updateUserActivityState:") retVoid [argPtr (castPtr raw_activity :: Ptr ())]

-- | @- saveToURL:ofType:forSaveOperation:error:@
saveToURL_ofType_forSaveOperation_error :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName, IsNSError outError) => nsDocument -> url -> typeName -> NSSaveOperationType -> outError -> IO Bool
saveToURL_ofType_forSaveOperation_error nsDocument  url typeName saveOperation outError =
withObjCPtr url $ \raw_url ->
  withObjCPtr typeName $ \raw_typeName ->
    withObjCPtr outError $ \raw_outError ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "saveToURL:ofType:forSaveOperation:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_typeName :: Ptr ()), argCULong (coerce saveOperation), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- dataRepresentationOfType:@
dataRepresentationOfType :: (IsNSDocument nsDocument, IsNSString type_) => nsDocument -> type_ -> IO (Id NSData)
dataRepresentationOfType nsDocument  type_ =
withObjCPtr type_ $ \raw_type_ ->
    sendMsg nsDocument (mkSelector "dataRepresentationOfType:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- fileAttributesToWriteToFile:ofType:saveOperation:@
fileAttributesToWriteToFile_ofType_saveOperation :: (IsNSDocument nsDocument, IsNSString fullDocumentPath, IsNSString documentTypeName) => nsDocument -> fullDocumentPath -> documentTypeName -> NSSaveOperationType -> IO (Id NSDictionary)
fileAttributesToWriteToFile_ofType_saveOperation nsDocument  fullDocumentPath documentTypeName saveOperationType =
withObjCPtr fullDocumentPath $ \raw_fullDocumentPath ->
  withObjCPtr documentTypeName $ \raw_documentTypeName ->
      sendMsg nsDocument (mkSelector "fileAttributesToWriteToFile:ofType:saveOperation:") (retPtr retVoid) [argPtr (castPtr raw_fullDocumentPath :: Ptr ()), argPtr (castPtr raw_documentTypeName :: Ptr ()), argCULong (coerce saveOperationType)] >>= retainedObject . castPtr

-- | @- fileName@
fileName :: IsNSDocument nsDocument => nsDocument -> IO (Id NSString)
fileName nsDocument  =
  sendMsg nsDocument (mkSelector "fileName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fileWrapperRepresentationOfType:@
fileWrapperRepresentationOfType :: (IsNSDocument nsDocument, IsNSString type_) => nsDocument -> type_ -> IO (Id NSFileWrapper)
fileWrapperRepresentationOfType nsDocument  type_ =
withObjCPtr type_ $ \raw_type_ ->
    sendMsg nsDocument (mkSelector "fileWrapperRepresentationOfType:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithContentsOfFile:ofType:@
initWithContentsOfFile_ofType :: (IsNSDocument nsDocument, IsNSString absolutePath, IsNSString typeName) => nsDocument -> absolutePath -> typeName -> IO RawId
initWithContentsOfFile_ofType nsDocument  absolutePath typeName =
withObjCPtr absolutePath $ \raw_absolutePath ->
  withObjCPtr typeName $ \raw_typeName ->
      fmap (RawId . castPtr) $ sendMsg nsDocument (mkSelector "initWithContentsOfFile:ofType:") (retPtr retVoid) [argPtr (castPtr raw_absolutePath :: Ptr ()), argPtr (castPtr raw_typeName :: Ptr ())]

-- | @- initWithContentsOfURL:ofType:@
initWithContentsOfURL_ofType :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName) => nsDocument -> url -> typeName -> IO RawId
initWithContentsOfURL_ofType nsDocument  url typeName =
withObjCPtr url $ \raw_url ->
  withObjCPtr typeName $ \raw_typeName ->
      fmap (RawId . castPtr) $ sendMsg nsDocument (mkSelector "initWithContentsOfURL:ofType:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_typeName :: Ptr ())]

-- | @- loadDataRepresentation:ofType:@
loadDataRepresentation_ofType :: (IsNSDocument nsDocument, IsNSData data_, IsNSString type_) => nsDocument -> data_ -> type_ -> IO Bool
loadDataRepresentation_ofType nsDocument  data_ type_ =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr type_ $ \raw_type_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "loadDataRepresentation:ofType:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- loadFileWrapperRepresentation:ofType:@
loadFileWrapperRepresentation_ofType :: (IsNSDocument nsDocument, IsNSFileWrapper wrapper, IsNSString type_) => nsDocument -> wrapper -> type_ -> IO Bool
loadFileWrapperRepresentation_ofType nsDocument  wrapper type_ =
withObjCPtr wrapper $ \raw_wrapper ->
  withObjCPtr type_ $ \raw_type_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "loadFileWrapperRepresentation:ofType:") retCULong [argPtr (castPtr raw_wrapper :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- printShowingPrintPanel:@
printShowingPrintPanel :: IsNSDocument nsDocument => nsDocument -> Bool -> IO ()
printShowingPrintPanel nsDocument  flag =
  sendMsg nsDocument (mkSelector "printShowingPrintPanel:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- readFromFile:ofType:@
readFromFile_ofType :: (IsNSDocument nsDocument, IsNSString fileName, IsNSString type_) => nsDocument -> fileName -> type_ -> IO Bool
readFromFile_ofType nsDocument  fileName type_ =
withObjCPtr fileName $ \raw_fileName ->
  withObjCPtr type_ $ \raw_type_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "readFromFile:ofType:") retCULong [argPtr (castPtr raw_fileName :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- readFromURL:ofType:@
readFromURL_ofType :: (IsNSDocument nsDocument, IsNSURL url, IsNSString type_) => nsDocument -> url -> type_ -> IO Bool
readFromURL_ofType nsDocument  url type_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr type_ $ \raw_type_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "readFromURL:ofType:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- revertToSavedFromFile:ofType:@
revertToSavedFromFile_ofType :: (IsNSDocument nsDocument, IsNSString fileName, IsNSString type_) => nsDocument -> fileName -> type_ -> IO Bool
revertToSavedFromFile_ofType nsDocument  fileName type_ =
withObjCPtr fileName $ \raw_fileName ->
  withObjCPtr type_ $ \raw_type_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "revertToSavedFromFile:ofType:") retCULong [argPtr (castPtr raw_fileName :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- revertToSavedFromURL:ofType:@
revertToSavedFromURL_ofType :: (IsNSDocument nsDocument, IsNSURL url, IsNSString type_) => nsDocument -> url -> type_ -> IO Bool
revertToSavedFromURL_ofType nsDocument  url type_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr type_ $ \raw_type_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "revertToSavedFromURL:ofType:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- runModalPageLayoutWithPrintInfo:@
runModalPageLayoutWithPrintInfo :: (IsNSDocument nsDocument, IsNSPrintInfo printInfo) => nsDocument -> printInfo -> IO CLong
runModalPageLayoutWithPrintInfo nsDocument  printInfo =
withObjCPtr printInfo $ \raw_printInfo ->
    sendMsg nsDocument (mkSelector "runModalPageLayoutWithPrintInfo:") retCLong [argPtr (castPtr raw_printInfo :: Ptr ())]

-- | @- saveToFile:saveOperation:delegate:didSaveSelector:contextInfo:@
saveToFile_saveOperation_delegate_didSaveSelector_contextInfo :: (IsNSDocument nsDocument, IsNSString fileName) => nsDocument -> fileName -> NSSaveOperationType -> RawId -> Selector -> Ptr () -> IO ()
saveToFile_saveOperation_delegate_didSaveSelector_contextInfo nsDocument  fileName saveOperation delegate didSaveSelector contextInfo =
withObjCPtr fileName $ \raw_fileName ->
    sendMsg nsDocument (mkSelector "saveToFile:saveOperation:delegate:didSaveSelector:contextInfo:") retVoid [argPtr (castPtr raw_fileName :: Ptr ()), argCULong (coerce saveOperation), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didSaveSelector), argPtr contextInfo]

-- | @- setFileName:@
setFileName :: (IsNSDocument nsDocument, IsNSString fileName) => nsDocument -> fileName -> IO ()
setFileName nsDocument  fileName =
withObjCPtr fileName $ \raw_fileName ->
    sendMsg nsDocument (mkSelector "setFileName:") retVoid [argPtr (castPtr raw_fileName :: Ptr ())]

-- | @- writeToFile:ofType:@
writeToFile_ofType :: (IsNSDocument nsDocument, IsNSString fileName, IsNSString type_) => nsDocument -> fileName -> type_ -> IO Bool
writeToFile_ofType nsDocument  fileName type_ =
withObjCPtr fileName $ \raw_fileName ->
  withObjCPtr type_ $ \raw_type_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "writeToFile:ofType:") retCULong [argPtr (castPtr raw_fileName :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- writeToFile:ofType:originalFile:saveOperation:@
writeToFile_ofType_originalFile_saveOperation :: (IsNSDocument nsDocument, IsNSString fullDocumentPath, IsNSString documentTypeName, IsNSString fullOriginalDocumentPath) => nsDocument -> fullDocumentPath -> documentTypeName -> fullOriginalDocumentPath -> NSSaveOperationType -> IO Bool
writeToFile_ofType_originalFile_saveOperation nsDocument  fullDocumentPath documentTypeName fullOriginalDocumentPath saveOperationType =
withObjCPtr fullDocumentPath $ \raw_fullDocumentPath ->
  withObjCPtr documentTypeName $ \raw_documentTypeName ->
    withObjCPtr fullOriginalDocumentPath $ \raw_fullOriginalDocumentPath ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "writeToFile:ofType:originalFile:saveOperation:") retCULong [argPtr (castPtr raw_fullDocumentPath :: Ptr ()), argPtr (castPtr raw_documentTypeName :: Ptr ()), argPtr (castPtr raw_fullOriginalDocumentPath :: Ptr ()), argCULong (coerce saveOperationType)]

-- | @- writeToURL:ofType:@
writeToURL_ofType :: (IsNSDocument nsDocument, IsNSURL url, IsNSString type_) => nsDocument -> url -> type_ -> IO Bool
writeToURL_ofType nsDocument  url type_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr type_ $ \raw_type_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "writeToURL:ofType:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- writeWithBackupToFile:ofType:saveOperation:@
writeWithBackupToFile_ofType_saveOperation :: (IsNSDocument nsDocument, IsNSString fullDocumentPath, IsNSString documentTypeName) => nsDocument -> fullDocumentPath -> documentTypeName -> NSSaveOperationType -> IO Bool
writeWithBackupToFile_ofType_saveOperation nsDocument  fullDocumentPath documentTypeName saveOperationType =
withObjCPtr fullDocumentPath $ \raw_fullDocumentPath ->
  withObjCPtr documentTypeName $ \raw_documentTypeName ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "writeWithBackupToFile:ofType:saveOperation:") retCULong [argPtr (castPtr raw_fullDocumentPath :: Ptr ()), argPtr (castPtr raw_documentTypeName :: Ptr ()), argCULong (coerce saveOperationType)]

-- | @- fileType@
fileType :: IsNSDocument nsDocument => nsDocument -> IO (Id NSString)
fileType nsDocument  =
  sendMsg nsDocument (mkSelector "fileType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFileType:@
setFileType :: (IsNSDocument nsDocument, IsNSString value) => nsDocument -> value -> IO ()
setFileType nsDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDocument (mkSelector "setFileType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fileURL@
fileURL :: IsNSDocument nsDocument => nsDocument -> IO (Id NSURL)
fileURL nsDocument  =
  sendMsg nsDocument (mkSelector "fileURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFileURL:@
setFileURL :: (IsNSDocument nsDocument, IsNSURL value) => nsDocument -> value -> IO ()
setFileURL nsDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDocument (mkSelector "setFileURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fileModificationDate@
fileModificationDate :: IsNSDocument nsDocument => nsDocument -> IO (Id NSDate)
fileModificationDate nsDocument  =
  sendMsg nsDocument (mkSelector "fileModificationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFileModificationDate:@
setFileModificationDate :: (IsNSDocument nsDocument, IsNSDate value) => nsDocument -> value -> IO ()
setFileModificationDate nsDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDocument (mkSelector "setFileModificationDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- draft@
draft :: IsNSDocument nsDocument => nsDocument -> IO Bool
draft nsDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "draft") retCULong []

-- | @- setDraft:@
setDraft :: IsNSDocument nsDocument => nsDocument -> Bool -> IO ()
setDraft nsDocument  value =
  sendMsg nsDocument (mkSelector "setDraft:") retVoid [argCULong (if value then 1 else 0)]

-- | @- entireFileLoaded@
entireFileLoaded :: IsNSDocument nsDocument => nsDocument -> IO Bool
entireFileLoaded nsDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "entireFileLoaded") retCULong []

-- | @- autosavingIsImplicitlyCancellable@
autosavingIsImplicitlyCancellable :: IsNSDocument nsDocument => nsDocument -> IO Bool
autosavingIsImplicitlyCancellable nsDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "autosavingIsImplicitlyCancellable") retCULong []

-- | @- keepBackupFile@
keepBackupFile :: IsNSDocument nsDocument => nsDocument -> IO Bool
keepBackupFile nsDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "keepBackupFile") retCULong []

-- | @- savePanelShowsFileFormatsControl@
savePanelShowsFileFormatsControl :: IsNSDocument nsDocument => nsDocument -> IO Bool
savePanelShowsFileFormatsControl nsDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "savePanelShowsFileFormatsControl") retCULong []

-- | @- fileNameExtensionWasHiddenInLastRunSavePanel@
fileNameExtensionWasHiddenInLastRunSavePanel :: IsNSDocument nsDocument => nsDocument -> IO Bool
fileNameExtensionWasHiddenInLastRunSavePanel nsDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "fileNameExtensionWasHiddenInLastRunSavePanel") retCULong []

-- | @- fileTypeFromLastRunSavePanel@
fileTypeFromLastRunSavePanel :: IsNSDocument nsDocument => nsDocument -> IO (Id NSString)
fileTypeFromLastRunSavePanel nsDocument  =
  sendMsg nsDocument (mkSelector "fileTypeFromLastRunSavePanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- hasUnautosavedChanges@
hasUnautosavedChanges :: IsNSDocument nsDocument => nsDocument -> IO Bool
hasUnautosavedChanges nsDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "hasUnautosavedChanges") retCULong []

-- | @+ autosavesInPlace@
autosavesInPlace :: IO Bool
autosavesInPlace  =
  do
    cls' <- getRequiredClass "NSDocument"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "autosavesInPlace") retCULong []

-- | @+ preservesVersions@
preservesVersions :: IO Bool
preservesVersions  =
  do
    cls' <- getRequiredClass "NSDocument"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "preservesVersions") retCULong []

-- | @- browsingVersions@
browsingVersions :: IsNSDocument nsDocument => nsDocument -> IO Bool
browsingVersions nsDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "browsingVersions") retCULong []

-- | @+ autosavesDrafts@
autosavesDrafts :: IO Bool
autosavesDrafts  =
  do
    cls' <- getRequiredClass "NSDocument"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "autosavesDrafts") retCULong []

-- | @- autosavingFileType@
autosavingFileType :: IsNSDocument nsDocument => nsDocument -> IO (Id NSString)
autosavingFileType nsDocument  =
  sendMsg nsDocument (mkSelector "autosavingFileType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- autosavedContentsFileURL@
autosavedContentsFileURL :: IsNSDocument nsDocument => nsDocument -> IO (Id NSURL)
autosavedContentsFileURL nsDocument  =
  sendMsg nsDocument (mkSelector "autosavedContentsFileURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAutosavedContentsFileURL:@
setAutosavedContentsFileURL :: (IsNSDocument nsDocument, IsNSURL value) => nsDocument -> value -> IO ()
setAutosavedContentsFileURL nsDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDocument (mkSelector "setAutosavedContentsFileURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- locked@
locked :: IsNSDocument nsDocument => nsDocument -> IO Bool
locked nsDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "locked") retCULong []

-- | @- printInfo@
printInfo :: IsNSDocument nsDocument => nsDocument -> IO (Id NSPrintInfo)
printInfo nsDocument  =
  sendMsg nsDocument (mkSelector "printInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrintInfo:@
setPrintInfo :: (IsNSDocument nsDocument, IsNSPrintInfo value) => nsDocument -> value -> IO ()
setPrintInfo nsDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDocument (mkSelector "setPrintInfo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- allowsDocumentSharing@
allowsDocumentSharing :: IsNSDocument nsDocument => nsDocument -> IO Bool
allowsDocumentSharing nsDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "allowsDocumentSharing") retCULong []

-- | @- documentEdited@
documentEdited :: IsNSDocument nsDocument => nsDocument -> IO Bool
documentEdited nsDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "documentEdited") retCULong []

-- | @- inViewingMode@
inViewingMode :: IsNSDocument nsDocument => nsDocument -> IO Bool
inViewingMode nsDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "inViewingMode") retCULong []

-- | @- undoManager@
undoManager :: IsNSDocument nsDocument => nsDocument -> IO (Id NSUndoManager)
undoManager nsDocument  =
  sendMsg nsDocument (mkSelector "undoManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUndoManager:@
setUndoManager :: (IsNSDocument nsDocument, IsNSUndoManager value) => nsDocument -> value -> IO ()
setUndoManager nsDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDocument (mkSelector "setUndoManager:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- hasUndoManager@
hasUndoManager :: IsNSDocument nsDocument => nsDocument -> IO Bool
hasUndoManager nsDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "hasUndoManager") retCULong []

-- | @- setHasUndoManager:@
setHasUndoManager :: IsNSDocument nsDocument => nsDocument -> Bool -> IO ()
setHasUndoManager nsDocument  value =
  sendMsg nsDocument (mkSelector "setHasUndoManager:") retVoid [argCULong (if value then 1 else 0)]

-- | @- windowNibName@
windowNibName :: IsNSDocument nsDocument => nsDocument -> IO (Id NSString)
windowNibName nsDocument  =
  sendMsg nsDocument (mkSelector "windowNibName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- windowControllers@
windowControllers :: IsNSDocument nsDocument => nsDocument -> IO (Id NSArray)
windowControllers nsDocument  =
  sendMsg nsDocument (mkSelector "windowControllers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- displayName@
displayName :: IsNSDocument nsDocument => nsDocument -> IO (Id NSString)
displayName nsDocument  =
  sendMsg nsDocument (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- windowForSheet@
windowForSheet :: IsNSDocument nsDocument => nsDocument -> IO (Id NSWindow)
windowForSheet nsDocument  =
  sendMsg nsDocument (mkSelector "windowForSheet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ readableTypes@
readableTypes :: IO (Id NSArray)
readableTypes  =
  do
    cls' <- getRequiredClass "NSDocument"
    sendClassMsg cls' (mkSelector "readableTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ writableTypes@
writableTypes :: IO (Id NSArray)
writableTypes  =
  do
    cls' <- getRequiredClass "NSDocument"
    sendClassMsg cls' (mkSelector "writableTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ usesUbiquitousStorage@
usesUbiquitousStorage :: IO Bool
usesUbiquitousStorage  =
  do
    cls' <- getRequiredClass "NSDocument"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "usesUbiquitousStorage") retCULong []

-- | @- lastComponentOfFileName@
lastComponentOfFileName :: IsNSDocument nsDocument => nsDocument -> IO (Id NSString)
lastComponentOfFileName nsDocument  =
  sendMsg nsDocument (mkSelector "lastComponentOfFileName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLastComponentOfFileName:@
setLastComponentOfFileName :: (IsNSDocument nsDocument, IsNSString value) => nsDocument -> value -> IO ()
setLastComponentOfFileName nsDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDocument (mkSelector "setLastComponentOfFileName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- objectSpecifier@
objectSpecifier :: IsNSDocument nsDocument => nsDocument -> IO (Id NSScriptObjectSpecifier)
objectSpecifier nsDocument  =
  sendMsg nsDocument (mkSelector "objectSpecifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- shouldRunSavePanelWithAccessoryView@
shouldRunSavePanelWithAccessoryView :: IsNSDocument nsDocument => nsDocument -> IO Bool
shouldRunSavePanelWithAccessoryView nsDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocument (mkSelector "shouldRunSavePanelWithAccessoryView") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithType:error:@
initWithType_errorSelector :: Selector
initWithType_errorSelector = mkSelector "initWithType:error:"

-- | @Selector@ for @canConcurrentlyReadDocumentsOfType:@
canConcurrentlyReadDocumentsOfTypeSelector :: Selector
canConcurrentlyReadDocumentsOfTypeSelector = mkSelector "canConcurrentlyReadDocumentsOfType:"

-- | @Selector@ for @initWithContentsOfURL:ofType:error:@
initWithContentsOfURL_ofType_errorSelector :: Selector
initWithContentsOfURL_ofType_errorSelector = mkSelector "initWithContentsOfURL:ofType:error:"

-- | @Selector@ for @initForURL:withContentsOfURL:ofType:error:@
initForURL_withContentsOfURL_ofType_errorSelector :: Selector
initForURL_withContentsOfURL_ofType_errorSelector = mkSelector "initForURL:withContentsOfURL:ofType:error:"

-- | @Selector@ for @performActivityWithSynchronousWaiting:usingBlock:@
performActivityWithSynchronousWaiting_usingBlockSelector :: Selector
performActivityWithSynchronousWaiting_usingBlockSelector = mkSelector "performActivityWithSynchronousWaiting:usingBlock:"

-- | @Selector@ for @continueActivityUsingBlock:@
continueActivityUsingBlockSelector :: Selector
continueActivityUsingBlockSelector = mkSelector "continueActivityUsingBlock:"

-- | @Selector@ for @continueAsynchronousWorkOnMainThreadUsingBlock:@
continueAsynchronousWorkOnMainThreadUsingBlockSelector :: Selector
continueAsynchronousWorkOnMainThreadUsingBlockSelector = mkSelector "continueAsynchronousWorkOnMainThreadUsingBlock:"

-- | @Selector@ for @performSynchronousFileAccessUsingBlock:@
performSynchronousFileAccessUsingBlockSelector :: Selector
performSynchronousFileAccessUsingBlockSelector = mkSelector "performSynchronousFileAccessUsingBlock:"

-- | @Selector@ for @performAsynchronousFileAccessUsingBlock:@
performAsynchronousFileAccessUsingBlockSelector :: Selector
performAsynchronousFileAccessUsingBlockSelector = mkSelector "performAsynchronousFileAccessUsingBlock:"

-- | @Selector@ for @revertDocumentToSaved:@
revertDocumentToSavedSelector :: Selector
revertDocumentToSavedSelector = mkSelector "revertDocumentToSaved:"

-- | @Selector@ for @revertToContentsOfURL:ofType:error:@
revertToContentsOfURL_ofType_errorSelector :: Selector
revertToContentsOfURL_ofType_errorSelector = mkSelector "revertToContentsOfURL:ofType:error:"

-- | @Selector@ for @readFromURL:ofType:error:@
readFromURL_ofType_errorSelector :: Selector
readFromURL_ofType_errorSelector = mkSelector "readFromURL:ofType:error:"

-- | @Selector@ for @readFromFileWrapper:ofType:error:@
readFromFileWrapper_ofType_errorSelector :: Selector
readFromFileWrapper_ofType_errorSelector = mkSelector "readFromFileWrapper:ofType:error:"

-- | @Selector@ for @readFromData:ofType:error:@
readFromData_ofType_errorSelector :: Selector
readFromData_ofType_errorSelector = mkSelector "readFromData:ofType:error:"

-- | @Selector@ for @writeToURL:ofType:error:@
writeToURL_ofType_errorSelector :: Selector
writeToURL_ofType_errorSelector = mkSelector "writeToURL:ofType:error:"

-- | @Selector@ for @fileWrapperOfType:error:@
fileWrapperOfType_errorSelector :: Selector
fileWrapperOfType_errorSelector = mkSelector "fileWrapperOfType:error:"

-- | @Selector@ for @dataOfType:error:@
dataOfType_errorSelector :: Selector
dataOfType_errorSelector = mkSelector "dataOfType:error:"

-- | @Selector@ for @unblockUserInteraction@
unblockUserInteractionSelector :: Selector
unblockUserInteractionSelector = mkSelector "unblockUserInteraction"

-- | @Selector@ for @writeSafelyToURL:ofType:forSaveOperation:error:@
writeSafelyToURL_ofType_forSaveOperation_errorSelector :: Selector
writeSafelyToURL_ofType_forSaveOperation_errorSelector = mkSelector "writeSafelyToURL:ofType:forSaveOperation:error:"

-- | @Selector@ for @writeToURL:ofType:forSaveOperation:originalContentsURL:error:@
writeToURL_ofType_forSaveOperation_originalContentsURL_errorSelector :: Selector
writeToURL_ofType_forSaveOperation_originalContentsURL_errorSelector = mkSelector "writeToURL:ofType:forSaveOperation:originalContentsURL:error:"

-- | @Selector@ for @fileAttributesToWriteToURL:ofType:forSaveOperation:originalContentsURL:error:@
fileAttributesToWriteToURL_ofType_forSaveOperation_originalContentsURL_errorSelector :: Selector
fileAttributesToWriteToURL_ofType_forSaveOperation_originalContentsURL_errorSelector = mkSelector "fileAttributesToWriteToURL:ofType:forSaveOperation:originalContentsURL:error:"

-- | @Selector@ for @saveDocument:@
saveDocumentSelector :: Selector
saveDocumentSelector = mkSelector "saveDocument:"

-- | @Selector@ for @saveDocumentAs:@
saveDocumentAsSelector :: Selector
saveDocumentAsSelector = mkSelector "saveDocumentAs:"

-- | @Selector@ for @saveDocumentTo:@
saveDocumentToSelector :: Selector
saveDocumentToSelector = mkSelector "saveDocumentTo:"

-- | @Selector@ for @saveDocumentWithDelegate:didSaveSelector:contextInfo:@
saveDocumentWithDelegate_didSaveSelector_contextInfoSelector :: Selector
saveDocumentWithDelegate_didSaveSelector_contextInfoSelector = mkSelector "saveDocumentWithDelegate:didSaveSelector:contextInfo:"

-- | @Selector@ for @runModalSavePanelForSaveOperation:delegate:didSaveSelector:contextInfo:@
runModalSavePanelForSaveOperation_delegate_didSaveSelector_contextInfoSelector :: Selector
runModalSavePanelForSaveOperation_delegate_didSaveSelector_contextInfoSelector = mkSelector "runModalSavePanelForSaveOperation:delegate:didSaveSelector:contextInfo:"

-- | @Selector@ for @prepareSavePanel:@
prepareSavePanelSelector :: Selector
prepareSavePanelSelector = mkSelector "prepareSavePanel:"

-- | @Selector@ for @saveToURL:ofType:forSaveOperation:delegate:didSaveSelector:contextInfo:@
saveToURL_ofType_forSaveOperation_delegate_didSaveSelector_contextInfoSelector :: Selector
saveToURL_ofType_forSaveOperation_delegate_didSaveSelector_contextInfoSelector = mkSelector "saveToURL:ofType:forSaveOperation:delegate:didSaveSelector:contextInfo:"

-- | @Selector@ for @saveToURL:ofType:forSaveOperation:completionHandler:@
saveToURL_ofType_forSaveOperation_completionHandlerSelector :: Selector
saveToURL_ofType_forSaveOperation_completionHandlerSelector = mkSelector "saveToURL:ofType:forSaveOperation:completionHandler:"

-- | @Selector@ for @canAsynchronouslyWriteToURL:ofType:forSaveOperation:@
canAsynchronouslyWriteToURL_ofType_forSaveOperationSelector :: Selector
canAsynchronouslyWriteToURL_ofType_forSaveOperationSelector = mkSelector "canAsynchronouslyWriteToURL:ofType:forSaveOperation:"

-- | @Selector@ for @checkAutosavingSafetyAndReturnError:@
checkAutosavingSafetyAndReturnErrorSelector :: Selector
checkAutosavingSafetyAndReturnErrorSelector = mkSelector "checkAutosavingSafetyAndReturnError:"

-- | @Selector@ for @scheduleAutosaving@
scheduleAutosavingSelector :: Selector
scheduleAutosavingSelector = mkSelector "scheduleAutosaving"

-- | @Selector@ for @autosaveDocumentWithDelegate:didAutosaveSelector:contextInfo:@
autosaveDocumentWithDelegate_didAutosaveSelector_contextInfoSelector :: Selector
autosaveDocumentWithDelegate_didAutosaveSelector_contextInfoSelector = mkSelector "autosaveDocumentWithDelegate:didAutosaveSelector:contextInfo:"

-- | @Selector@ for @autosaveWithImplicitCancellability:completionHandler:@
autosaveWithImplicitCancellability_completionHandlerSelector :: Selector
autosaveWithImplicitCancellability_completionHandlerSelector = mkSelector "autosaveWithImplicitCancellability:completionHandler:"

-- | @Selector@ for @browseDocumentVersions:@
browseDocumentVersionsSelector :: Selector
browseDocumentVersionsSelector = mkSelector "browseDocumentVersions:"

-- | @Selector@ for @stopBrowsingVersionsWithCompletionHandler:@
stopBrowsingVersionsWithCompletionHandlerSelector :: Selector
stopBrowsingVersionsWithCompletionHandlerSelector = mkSelector "stopBrowsingVersionsWithCompletionHandler:"

-- | @Selector@ for @canCloseDocumentWithDelegate:shouldCloseSelector:contextInfo:@
canCloseDocumentWithDelegate_shouldCloseSelector_contextInfoSelector :: Selector
canCloseDocumentWithDelegate_shouldCloseSelector_contextInfoSelector = mkSelector "canCloseDocumentWithDelegate:shouldCloseSelector:contextInfo:"

-- | @Selector@ for @close@
closeSelector :: Selector
closeSelector = mkSelector "close"

-- | @Selector@ for @duplicateDocument:@
duplicateDocumentSelector :: Selector
duplicateDocumentSelector = mkSelector "duplicateDocument:"

-- | @Selector@ for @duplicateDocumentWithDelegate:didDuplicateSelector:contextInfo:@
duplicateDocumentWithDelegate_didDuplicateSelector_contextInfoSelector :: Selector
duplicateDocumentWithDelegate_didDuplicateSelector_contextInfoSelector = mkSelector "duplicateDocumentWithDelegate:didDuplicateSelector:contextInfo:"

-- | @Selector@ for @duplicateAndReturnError:@
duplicateAndReturnErrorSelector :: Selector
duplicateAndReturnErrorSelector = mkSelector "duplicateAndReturnError:"

-- | @Selector@ for @renameDocument:@
renameDocumentSelector :: Selector
renameDocumentSelector = mkSelector "renameDocument:"

-- | @Selector@ for @moveDocumentToUbiquityContainer:@
moveDocumentToUbiquityContainerSelector :: Selector
moveDocumentToUbiquityContainerSelector = mkSelector "moveDocumentToUbiquityContainer:"

-- | @Selector@ for @moveDocument:@
moveDocumentSelector :: Selector
moveDocumentSelector = mkSelector "moveDocument:"

-- | @Selector@ for @moveDocumentWithCompletionHandler:@
moveDocumentWithCompletionHandlerSelector :: Selector
moveDocumentWithCompletionHandlerSelector = mkSelector "moveDocumentWithCompletionHandler:"

-- | @Selector@ for @moveToURL:completionHandler:@
moveToURL_completionHandlerSelector :: Selector
moveToURL_completionHandlerSelector = mkSelector "moveToURL:completionHandler:"

-- | @Selector@ for @lockDocument:@
lockDocumentSelector :: Selector
lockDocumentSelector = mkSelector "lockDocument:"

-- | @Selector@ for @unlockDocument:@
unlockDocumentSelector :: Selector
unlockDocumentSelector = mkSelector "unlockDocument:"

-- | @Selector@ for @lockDocumentWithCompletionHandler:@
lockDocumentWithCompletionHandlerSelector :: Selector
lockDocumentWithCompletionHandlerSelector = mkSelector "lockDocumentWithCompletionHandler:"

-- | @Selector@ for @lockWithCompletionHandler:@
lockWithCompletionHandlerSelector :: Selector
lockWithCompletionHandlerSelector = mkSelector "lockWithCompletionHandler:"

-- | @Selector@ for @unlockDocumentWithCompletionHandler:@
unlockDocumentWithCompletionHandlerSelector :: Selector
unlockDocumentWithCompletionHandlerSelector = mkSelector "unlockDocumentWithCompletionHandler:"

-- | @Selector@ for @unlockWithCompletionHandler:@
unlockWithCompletionHandlerSelector :: Selector
unlockWithCompletionHandlerSelector = mkSelector "unlockWithCompletionHandler:"

-- | @Selector@ for @runPageLayout:@
runPageLayoutSelector :: Selector
runPageLayoutSelector = mkSelector "runPageLayout:"

-- | @Selector@ for @runModalPageLayoutWithPrintInfo:delegate:didRunSelector:contextInfo:@
runModalPageLayoutWithPrintInfo_delegate_didRunSelector_contextInfoSelector :: Selector
runModalPageLayoutWithPrintInfo_delegate_didRunSelector_contextInfoSelector = mkSelector "runModalPageLayoutWithPrintInfo:delegate:didRunSelector:contextInfo:"

-- | @Selector@ for @preparePageLayout:@
preparePageLayoutSelector :: Selector
preparePageLayoutSelector = mkSelector "preparePageLayout:"

-- | @Selector@ for @shouldChangePrintInfo:@
shouldChangePrintInfoSelector :: Selector
shouldChangePrintInfoSelector = mkSelector "shouldChangePrintInfo:"

-- | @Selector@ for @printDocument:@
printDocumentSelector :: Selector
printDocumentSelector = mkSelector "printDocument:"

-- | @Selector@ for @printDocumentWithSettings:showPrintPanel:delegate:didPrintSelector:contextInfo:@
printDocumentWithSettings_showPrintPanel_delegate_didPrintSelector_contextInfoSelector :: Selector
printDocumentWithSettings_showPrintPanel_delegate_didPrintSelector_contextInfoSelector = mkSelector "printDocumentWithSettings:showPrintPanel:delegate:didPrintSelector:contextInfo:"

-- | @Selector@ for @printOperationWithSettings:error:@
printOperationWithSettings_errorSelector :: Selector
printOperationWithSettings_errorSelector = mkSelector "printOperationWithSettings:error:"

-- | @Selector@ for @runModalPrintOperation:delegate:didRunSelector:contextInfo:@
runModalPrintOperation_delegate_didRunSelector_contextInfoSelector :: Selector
runModalPrintOperation_delegate_didRunSelector_contextInfoSelector = mkSelector "runModalPrintOperation:delegate:didRunSelector:contextInfo:"

-- | @Selector@ for @saveDocumentToPDF:@
saveDocumentToPDFSelector :: Selector
saveDocumentToPDFSelector = mkSelector "saveDocumentToPDF:"

-- | @Selector@ for @shareDocumentWithSharingService:completionHandler:@
shareDocumentWithSharingService_completionHandlerSelector :: Selector
shareDocumentWithSharingService_completionHandlerSelector = mkSelector "shareDocumentWithSharingService:completionHandler:"

-- | @Selector@ for @prepareSharingServicePicker:@
prepareSharingServicePickerSelector :: Selector
prepareSharingServicePickerSelector = mkSelector "prepareSharingServicePicker:"

-- | @Selector@ for @updateChangeCount:@
updateChangeCountSelector :: Selector
updateChangeCountSelector = mkSelector "updateChangeCount:"

-- | @Selector@ for @changeCountTokenForSaveOperation:@
changeCountTokenForSaveOperationSelector :: Selector
changeCountTokenForSaveOperationSelector = mkSelector "changeCountTokenForSaveOperation:"

-- | @Selector@ for @updateChangeCountWithToken:forSaveOperation:@
updateChangeCountWithToken_forSaveOperationSelector :: Selector
updateChangeCountWithToken_forSaveOperationSelector = mkSelector "updateChangeCountWithToken:forSaveOperation:"

-- | @Selector@ for @presentError:modalForWindow:delegate:didPresentSelector:contextInfo:@
presentError_modalForWindow_delegate_didPresentSelector_contextInfoSelector :: Selector
presentError_modalForWindow_delegate_didPresentSelector_contextInfoSelector = mkSelector "presentError:modalForWindow:delegate:didPresentSelector:contextInfo:"

-- | @Selector@ for @presentError:@
presentErrorSelector :: Selector
presentErrorSelector = mkSelector "presentError:"

-- | @Selector@ for @willPresentError:@
willPresentErrorSelector :: Selector
willPresentErrorSelector = mkSelector "willPresentError:"

-- | @Selector@ for @willNotPresentError:@
willNotPresentErrorSelector :: Selector
willNotPresentErrorSelector = mkSelector "willNotPresentError:"

-- | @Selector@ for @makeWindowControllers@
makeWindowControllersSelector :: Selector
makeWindowControllersSelector = mkSelector "makeWindowControllers"

-- | @Selector@ for @windowControllerWillLoadNib:@
windowControllerWillLoadNibSelector :: Selector
windowControllerWillLoadNibSelector = mkSelector "windowControllerWillLoadNib:"

-- | @Selector@ for @windowControllerDidLoadNib:@
windowControllerDidLoadNibSelector :: Selector
windowControllerDidLoadNibSelector = mkSelector "windowControllerDidLoadNib:"

-- | @Selector@ for @setWindow:@
setWindowSelector :: Selector
setWindowSelector = mkSelector "setWindow:"

-- | @Selector@ for @addWindowController:@
addWindowControllerSelector :: Selector
addWindowControllerSelector = mkSelector "addWindowController:"

-- | @Selector@ for @removeWindowController:@
removeWindowControllerSelector :: Selector
removeWindowControllerSelector = mkSelector "removeWindowController:"

-- | @Selector@ for @showWindows@
showWindowsSelector :: Selector
showWindowsSelector = mkSelector "showWindows"

-- | @Selector@ for @shouldCloseWindowController:delegate:shouldCloseSelector:contextInfo:@
shouldCloseWindowController_delegate_shouldCloseSelector_contextInfoSelector :: Selector
shouldCloseWindowController_delegate_shouldCloseSelector_contextInfoSelector = mkSelector "shouldCloseWindowController:delegate:shouldCloseSelector:contextInfo:"

-- | @Selector@ for @setDisplayName:@
setDisplayNameSelector :: Selector
setDisplayNameSelector = mkSelector "setDisplayName:"

-- | @Selector@ for @defaultDraftName@
defaultDraftNameSelector :: Selector
defaultDraftNameSelector = mkSelector "defaultDraftName"

-- | @Selector@ for @isNativeType:@
isNativeTypeSelector :: Selector
isNativeTypeSelector = mkSelector "isNativeType:"

-- | @Selector@ for @writableTypesForSaveOperation:@
writableTypesForSaveOperationSelector :: Selector
writableTypesForSaveOperationSelector = mkSelector "writableTypesForSaveOperation:"

-- | @Selector@ for @fileNameExtensionForType:saveOperation:@
fileNameExtensionForType_saveOperationSelector :: Selector
fileNameExtensionForType_saveOperationSelector = mkSelector "fileNameExtensionForType:saveOperation:"

-- | @Selector@ for @validateUserInterfaceItem:@
validateUserInterfaceItemSelector :: Selector
validateUserInterfaceItemSelector = mkSelector "validateUserInterfaceItem:"

-- | @Selector@ for @relinquishPresentedItemToReader:@
relinquishPresentedItemToReaderSelector :: Selector
relinquishPresentedItemToReaderSelector = mkSelector "relinquishPresentedItemToReader:"

-- | @Selector@ for @relinquishPresentedItemToWriter:@
relinquishPresentedItemToWriterSelector :: Selector
relinquishPresentedItemToWriterSelector = mkSelector "relinquishPresentedItemToWriter:"

-- | @Selector@ for @savePresentedItemChangesWithCompletionHandler:@
savePresentedItemChangesWithCompletionHandlerSelector :: Selector
savePresentedItemChangesWithCompletionHandlerSelector = mkSelector "savePresentedItemChangesWithCompletionHandler:"

-- | @Selector@ for @accommodatePresentedItemDeletionWithCompletionHandler:@
accommodatePresentedItemDeletionWithCompletionHandlerSelector :: Selector
accommodatePresentedItemDeletionWithCompletionHandlerSelector = mkSelector "accommodatePresentedItemDeletionWithCompletionHandler:"

-- | @Selector@ for @presentedItemDidMoveToURL:@
presentedItemDidMoveToURLSelector :: Selector
presentedItemDidMoveToURLSelector = mkSelector "presentedItemDidMoveToURL:"

-- | @Selector@ for @presentedItemDidChange@
presentedItemDidChangeSelector :: Selector
presentedItemDidChangeSelector = mkSelector "presentedItemDidChange"

-- | @Selector@ for @presentedItemDidChangeUbiquityAttributes:@
presentedItemDidChangeUbiquityAttributesSelector :: Selector
presentedItemDidChangeUbiquityAttributesSelector = mkSelector "presentedItemDidChangeUbiquityAttributes:"

-- | @Selector@ for @presentedItemDidGainVersion:@
presentedItemDidGainVersionSelector :: Selector
presentedItemDidGainVersionSelector = mkSelector "presentedItemDidGainVersion:"

-- | @Selector@ for @presentedItemDidLoseVersion:@
presentedItemDidLoseVersionSelector :: Selector
presentedItemDidLoseVersionSelector = mkSelector "presentedItemDidLoseVersion:"

-- | @Selector@ for @presentedItemDidResolveConflictVersion:@
presentedItemDidResolveConflictVersionSelector :: Selector
presentedItemDidResolveConflictVersionSelector = mkSelector "presentedItemDidResolveConflictVersion:"

-- | @Selector@ for @restoreDocumentWindowWithIdentifier:state:completionHandler:@
restoreDocumentWindowWithIdentifier_state_completionHandlerSelector :: Selector
restoreDocumentWindowWithIdentifier_state_completionHandlerSelector = mkSelector "restoreDocumentWindowWithIdentifier:state:completionHandler:"

-- | @Selector@ for @encodeRestorableStateWithCoder:@
encodeRestorableStateWithCoderSelector :: Selector
encodeRestorableStateWithCoderSelector = mkSelector "encodeRestorableStateWithCoder:"

-- | @Selector@ for @encodeRestorableStateWithCoder:backgroundQueue:@
encodeRestorableStateWithCoder_backgroundQueueSelector :: Selector
encodeRestorableStateWithCoder_backgroundQueueSelector = mkSelector "encodeRestorableStateWithCoder:backgroundQueue:"

-- | @Selector@ for @restoreStateWithCoder:@
restoreStateWithCoderSelector :: Selector
restoreStateWithCoderSelector = mkSelector "restoreStateWithCoder:"

-- | @Selector@ for @invalidateRestorableState@
invalidateRestorableStateSelector :: Selector
invalidateRestorableStateSelector = mkSelector "invalidateRestorableState"

-- | @Selector@ for @allowedClassesForRestorableStateKeyPath:@
allowedClassesForRestorableStateKeyPathSelector :: Selector
allowedClassesForRestorableStateKeyPathSelector = mkSelector "allowedClassesForRestorableStateKeyPath:"

-- | @Selector@ for @handleSaveScriptCommand:@
handleSaveScriptCommandSelector :: Selector
handleSaveScriptCommandSelector = mkSelector "handleSaveScriptCommand:"

-- | @Selector@ for @handleCloseScriptCommand:@
handleCloseScriptCommandSelector :: Selector
handleCloseScriptCommandSelector = mkSelector "handleCloseScriptCommand:"

-- | @Selector@ for @handlePrintScriptCommand:@
handlePrintScriptCommandSelector :: Selector
handlePrintScriptCommandSelector = mkSelector "handlePrintScriptCommand:"

-- | @Selector@ for @updateUserActivityState:@
updateUserActivityStateSelector :: Selector
updateUserActivityStateSelector = mkSelector "updateUserActivityState:"

-- | @Selector@ for @saveToURL:ofType:forSaveOperation:error:@
saveToURL_ofType_forSaveOperation_errorSelector :: Selector
saveToURL_ofType_forSaveOperation_errorSelector = mkSelector "saveToURL:ofType:forSaveOperation:error:"

-- | @Selector@ for @dataRepresentationOfType:@
dataRepresentationOfTypeSelector :: Selector
dataRepresentationOfTypeSelector = mkSelector "dataRepresentationOfType:"

-- | @Selector@ for @fileAttributesToWriteToFile:ofType:saveOperation:@
fileAttributesToWriteToFile_ofType_saveOperationSelector :: Selector
fileAttributesToWriteToFile_ofType_saveOperationSelector = mkSelector "fileAttributesToWriteToFile:ofType:saveOperation:"

-- | @Selector@ for @fileName@
fileNameSelector :: Selector
fileNameSelector = mkSelector "fileName"

-- | @Selector@ for @fileWrapperRepresentationOfType:@
fileWrapperRepresentationOfTypeSelector :: Selector
fileWrapperRepresentationOfTypeSelector = mkSelector "fileWrapperRepresentationOfType:"

-- | @Selector@ for @initWithContentsOfFile:ofType:@
initWithContentsOfFile_ofTypeSelector :: Selector
initWithContentsOfFile_ofTypeSelector = mkSelector "initWithContentsOfFile:ofType:"

-- | @Selector@ for @initWithContentsOfURL:ofType:@
initWithContentsOfURL_ofTypeSelector :: Selector
initWithContentsOfURL_ofTypeSelector = mkSelector "initWithContentsOfURL:ofType:"

-- | @Selector@ for @loadDataRepresentation:ofType:@
loadDataRepresentation_ofTypeSelector :: Selector
loadDataRepresentation_ofTypeSelector = mkSelector "loadDataRepresentation:ofType:"

-- | @Selector@ for @loadFileWrapperRepresentation:ofType:@
loadFileWrapperRepresentation_ofTypeSelector :: Selector
loadFileWrapperRepresentation_ofTypeSelector = mkSelector "loadFileWrapperRepresentation:ofType:"

-- | @Selector@ for @printShowingPrintPanel:@
printShowingPrintPanelSelector :: Selector
printShowingPrintPanelSelector = mkSelector "printShowingPrintPanel:"

-- | @Selector@ for @readFromFile:ofType:@
readFromFile_ofTypeSelector :: Selector
readFromFile_ofTypeSelector = mkSelector "readFromFile:ofType:"

-- | @Selector@ for @readFromURL:ofType:@
readFromURL_ofTypeSelector :: Selector
readFromURL_ofTypeSelector = mkSelector "readFromURL:ofType:"

-- | @Selector@ for @revertToSavedFromFile:ofType:@
revertToSavedFromFile_ofTypeSelector :: Selector
revertToSavedFromFile_ofTypeSelector = mkSelector "revertToSavedFromFile:ofType:"

-- | @Selector@ for @revertToSavedFromURL:ofType:@
revertToSavedFromURL_ofTypeSelector :: Selector
revertToSavedFromURL_ofTypeSelector = mkSelector "revertToSavedFromURL:ofType:"

-- | @Selector@ for @runModalPageLayoutWithPrintInfo:@
runModalPageLayoutWithPrintInfoSelector :: Selector
runModalPageLayoutWithPrintInfoSelector = mkSelector "runModalPageLayoutWithPrintInfo:"

-- | @Selector@ for @saveToFile:saveOperation:delegate:didSaveSelector:contextInfo:@
saveToFile_saveOperation_delegate_didSaveSelector_contextInfoSelector :: Selector
saveToFile_saveOperation_delegate_didSaveSelector_contextInfoSelector = mkSelector "saveToFile:saveOperation:delegate:didSaveSelector:contextInfo:"

-- | @Selector@ for @setFileName:@
setFileNameSelector :: Selector
setFileNameSelector = mkSelector "setFileName:"

-- | @Selector@ for @writeToFile:ofType:@
writeToFile_ofTypeSelector :: Selector
writeToFile_ofTypeSelector = mkSelector "writeToFile:ofType:"

-- | @Selector@ for @writeToFile:ofType:originalFile:saveOperation:@
writeToFile_ofType_originalFile_saveOperationSelector :: Selector
writeToFile_ofType_originalFile_saveOperationSelector = mkSelector "writeToFile:ofType:originalFile:saveOperation:"

-- | @Selector@ for @writeToURL:ofType:@
writeToURL_ofTypeSelector :: Selector
writeToURL_ofTypeSelector = mkSelector "writeToURL:ofType:"

-- | @Selector@ for @writeWithBackupToFile:ofType:saveOperation:@
writeWithBackupToFile_ofType_saveOperationSelector :: Selector
writeWithBackupToFile_ofType_saveOperationSelector = mkSelector "writeWithBackupToFile:ofType:saveOperation:"

-- | @Selector@ for @fileType@
fileTypeSelector :: Selector
fileTypeSelector = mkSelector "fileType"

-- | @Selector@ for @setFileType:@
setFileTypeSelector :: Selector
setFileTypeSelector = mkSelector "setFileType:"

-- | @Selector@ for @fileURL@
fileURLSelector :: Selector
fileURLSelector = mkSelector "fileURL"

-- | @Selector@ for @setFileURL:@
setFileURLSelector :: Selector
setFileURLSelector = mkSelector "setFileURL:"

-- | @Selector@ for @fileModificationDate@
fileModificationDateSelector :: Selector
fileModificationDateSelector = mkSelector "fileModificationDate"

-- | @Selector@ for @setFileModificationDate:@
setFileModificationDateSelector :: Selector
setFileModificationDateSelector = mkSelector "setFileModificationDate:"

-- | @Selector@ for @draft@
draftSelector :: Selector
draftSelector = mkSelector "draft"

-- | @Selector@ for @setDraft:@
setDraftSelector :: Selector
setDraftSelector = mkSelector "setDraft:"

-- | @Selector@ for @entireFileLoaded@
entireFileLoadedSelector :: Selector
entireFileLoadedSelector = mkSelector "entireFileLoaded"

-- | @Selector@ for @autosavingIsImplicitlyCancellable@
autosavingIsImplicitlyCancellableSelector :: Selector
autosavingIsImplicitlyCancellableSelector = mkSelector "autosavingIsImplicitlyCancellable"

-- | @Selector@ for @keepBackupFile@
keepBackupFileSelector :: Selector
keepBackupFileSelector = mkSelector "keepBackupFile"

-- | @Selector@ for @savePanelShowsFileFormatsControl@
savePanelShowsFileFormatsControlSelector :: Selector
savePanelShowsFileFormatsControlSelector = mkSelector "savePanelShowsFileFormatsControl"

-- | @Selector@ for @fileNameExtensionWasHiddenInLastRunSavePanel@
fileNameExtensionWasHiddenInLastRunSavePanelSelector :: Selector
fileNameExtensionWasHiddenInLastRunSavePanelSelector = mkSelector "fileNameExtensionWasHiddenInLastRunSavePanel"

-- | @Selector@ for @fileTypeFromLastRunSavePanel@
fileTypeFromLastRunSavePanelSelector :: Selector
fileTypeFromLastRunSavePanelSelector = mkSelector "fileTypeFromLastRunSavePanel"

-- | @Selector@ for @hasUnautosavedChanges@
hasUnautosavedChangesSelector :: Selector
hasUnautosavedChangesSelector = mkSelector "hasUnautosavedChanges"

-- | @Selector@ for @autosavesInPlace@
autosavesInPlaceSelector :: Selector
autosavesInPlaceSelector = mkSelector "autosavesInPlace"

-- | @Selector@ for @preservesVersions@
preservesVersionsSelector :: Selector
preservesVersionsSelector = mkSelector "preservesVersions"

-- | @Selector@ for @browsingVersions@
browsingVersionsSelector :: Selector
browsingVersionsSelector = mkSelector "browsingVersions"

-- | @Selector@ for @autosavesDrafts@
autosavesDraftsSelector :: Selector
autosavesDraftsSelector = mkSelector "autosavesDrafts"

-- | @Selector@ for @autosavingFileType@
autosavingFileTypeSelector :: Selector
autosavingFileTypeSelector = mkSelector "autosavingFileType"

-- | @Selector@ for @autosavedContentsFileURL@
autosavedContentsFileURLSelector :: Selector
autosavedContentsFileURLSelector = mkSelector "autosavedContentsFileURL"

-- | @Selector@ for @setAutosavedContentsFileURL:@
setAutosavedContentsFileURLSelector :: Selector
setAutosavedContentsFileURLSelector = mkSelector "setAutosavedContentsFileURL:"

-- | @Selector@ for @locked@
lockedSelector :: Selector
lockedSelector = mkSelector "locked"

-- | @Selector@ for @printInfo@
printInfoSelector :: Selector
printInfoSelector = mkSelector "printInfo"

-- | @Selector@ for @setPrintInfo:@
setPrintInfoSelector :: Selector
setPrintInfoSelector = mkSelector "setPrintInfo:"

-- | @Selector@ for @allowsDocumentSharing@
allowsDocumentSharingSelector :: Selector
allowsDocumentSharingSelector = mkSelector "allowsDocumentSharing"

-- | @Selector@ for @documentEdited@
documentEditedSelector :: Selector
documentEditedSelector = mkSelector "documentEdited"

-- | @Selector@ for @inViewingMode@
inViewingModeSelector :: Selector
inViewingModeSelector = mkSelector "inViewingMode"

-- | @Selector@ for @undoManager@
undoManagerSelector :: Selector
undoManagerSelector = mkSelector "undoManager"

-- | @Selector@ for @setUndoManager:@
setUndoManagerSelector :: Selector
setUndoManagerSelector = mkSelector "setUndoManager:"

-- | @Selector@ for @hasUndoManager@
hasUndoManagerSelector :: Selector
hasUndoManagerSelector = mkSelector "hasUndoManager"

-- | @Selector@ for @setHasUndoManager:@
setHasUndoManagerSelector :: Selector
setHasUndoManagerSelector = mkSelector "setHasUndoManager:"

-- | @Selector@ for @windowNibName@
windowNibNameSelector :: Selector
windowNibNameSelector = mkSelector "windowNibName"

-- | @Selector@ for @windowControllers@
windowControllersSelector :: Selector
windowControllersSelector = mkSelector "windowControllers"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @windowForSheet@
windowForSheetSelector :: Selector
windowForSheetSelector = mkSelector "windowForSheet"

-- | @Selector@ for @readableTypes@
readableTypesSelector :: Selector
readableTypesSelector = mkSelector "readableTypes"

-- | @Selector@ for @writableTypes@
writableTypesSelector :: Selector
writableTypesSelector = mkSelector "writableTypes"

-- | @Selector@ for @usesUbiquitousStorage@
usesUbiquitousStorageSelector :: Selector
usesUbiquitousStorageSelector = mkSelector "usesUbiquitousStorage"

-- | @Selector@ for @lastComponentOfFileName@
lastComponentOfFileNameSelector :: Selector
lastComponentOfFileNameSelector = mkSelector "lastComponentOfFileName"

-- | @Selector@ for @setLastComponentOfFileName:@
setLastComponentOfFileNameSelector :: Selector
setLastComponentOfFileNameSelector = mkSelector "setLastComponentOfFileName:"

-- | @Selector@ for @objectSpecifier@
objectSpecifierSelector :: Selector
objectSpecifierSelector = mkSelector "objectSpecifier"

-- | @Selector@ for @shouldRunSavePanelWithAccessoryView@
shouldRunSavePanelWithAccessoryViewSelector :: Selector
shouldRunSavePanelWithAccessoryViewSelector = mkSelector "shouldRunSavePanelWithAccessoryView"

