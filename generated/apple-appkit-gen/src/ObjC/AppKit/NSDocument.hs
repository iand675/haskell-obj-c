{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , backupFileURL
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
  , pdfPrintOperation
  , allowsDocumentSharing
  , previewRepresentableActivityItems
  , setPreviewRepresentableActivityItems
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
  , presentedItemURL
  , observedPresentedItemUbiquityAttributes
  , restorableStateKeyPaths
  , lastComponentOfFileName
  , setLastComponentOfFileName
  , objectSpecifier
  , userActivity
  , setUserActivity
  , shouldRunSavePanelWithAccessoryView
  , accommodatePresentedItemDeletionWithCompletionHandlerSelector
  , addWindowControllerSelector
  , allowedClassesForRestorableStateKeyPathSelector
  , allowsDocumentSharingSelector
  , autosaveDocumentWithDelegate_didAutosaveSelector_contextInfoSelector
  , autosaveWithImplicitCancellability_completionHandlerSelector
  , autosavedContentsFileURLSelector
  , autosavesDraftsSelector
  , autosavesInPlaceSelector
  , autosavingFileTypeSelector
  , autosavingIsImplicitlyCancellableSelector
  , backupFileURLSelector
  , browseDocumentVersionsSelector
  , browsingVersionsSelector
  , canAsynchronouslyWriteToURL_ofType_forSaveOperationSelector
  , canCloseDocumentWithDelegate_shouldCloseSelector_contextInfoSelector
  , canConcurrentlyReadDocumentsOfTypeSelector
  , changeCountTokenForSaveOperationSelector
  , checkAutosavingSafetyAndReturnErrorSelector
  , closeSelector
  , continueActivityUsingBlockSelector
  , continueAsynchronousWorkOnMainThreadUsingBlockSelector
  , dataOfType_errorSelector
  , dataRepresentationOfTypeSelector
  , defaultDraftNameSelector
  , displayNameSelector
  , documentEditedSelector
  , draftSelector
  , duplicateAndReturnErrorSelector
  , duplicateDocumentSelector
  , duplicateDocumentWithDelegate_didDuplicateSelector_contextInfoSelector
  , encodeRestorableStateWithCoderSelector
  , encodeRestorableStateWithCoder_backgroundQueueSelector
  , entireFileLoadedSelector
  , fileAttributesToWriteToFile_ofType_saveOperationSelector
  , fileAttributesToWriteToURL_ofType_forSaveOperation_originalContentsURL_errorSelector
  , fileModificationDateSelector
  , fileNameExtensionForType_saveOperationSelector
  , fileNameExtensionWasHiddenInLastRunSavePanelSelector
  , fileNameSelector
  , fileTypeFromLastRunSavePanelSelector
  , fileTypeSelector
  , fileURLSelector
  , fileWrapperOfType_errorSelector
  , fileWrapperRepresentationOfTypeSelector
  , handleCloseScriptCommandSelector
  , handlePrintScriptCommandSelector
  , handleSaveScriptCommandSelector
  , hasUnautosavedChangesSelector
  , hasUndoManagerSelector
  , inViewingModeSelector
  , initForURL_withContentsOfURL_ofType_errorSelector
  , initSelector
  , initWithContentsOfFile_ofTypeSelector
  , initWithContentsOfURL_ofTypeSelector
  , initWithContentsOfURL_ofType_errorSelector
  , initWithType_errorSelector
  , invalidateRestorableStateSelector
  , isNativeTypeSelector
  , keepBackupFileSelector
  , lastComponentOfFileNameSelector
  , loadDataRepresentation_ofTypeSelector
  , loadFileWrapperRepresentation_ofTypeSelector
  , lockDocumentSelector
  , lockDocumentWithCompletionHandlerSelector
  , lockWithCompletionHandlerSelector
  , lockedSelector
  , makeWindowControllersSelector
  , moveDocumentSelector
  , moveDocumentToUbiquityContainerSelector
  , moveDocumentWithCompletionHandlerSelector
  , moveToURL_completionHandlerSelector
  , objectSpecifierSelector
  , observedPresentedItemUbiquityAttributesSelector
  , pdfPrintOperationSelector
  , performActivityWithSynchronousWaiting_usingBlockSelector
  , performAsynchronousFileAccessUsingBlockSelector
  , performSynchronousFileAccessUsingBlockSelector
  , preparePageLayoutSelector
  , prepareSavePanelSelector
  , prepareSharingServicePickerSelector
  , presentErrorSelector
  , presentError_modalForWindow_delegate_didPresentSelector_contextInfoSelector
  , presentedItemDidChangeSelector
  , presentedItemDidChangeUbiquityAttributesSelector
  , presentedItemDidGainVersionSelector
  , presentedItemDidLoseVersionSelector
  , presentedItemDidMoveToURLSelector
  , presentedItemDidResolveConflictVersionSelector
  , presentedItemURLSelector
  , preservesVersionsSelector
  , previewRepresentableActivityItemsSelector
  , printDocumentSelector
  , printDocumentWithSettings_showPrintPanel_delegate_didPrintSelector_contextInfoSelector
  , printInfoSelector
  , printOperationWithSettings_errorSelector
  , printShowingPrintPanelSelector
  , readFromData_ofType_errorSelector
  , readFromFileWrapper_ofType_errorSelector
  , readFromFile_ofTypeSelector
  , readFromURL_ofTypeSelector
  , readFromURL_ofType_errorSelector
  , readableTypesSelector
  , relinquishPresentedItemToReaderSelector
  , relinquishPresentedItemToWriterSelector
  , removeWindowControllerSelector
  , renameDocumentSelector
  , restorableStateKeyPathsSelector
  , restoreDocumentWindowWithIdentifier_state_completionHandlerSelector
  , restoreStateWithCoderSelector
  , revertDocumentToSavedSelector
  , revertToContentsOfURL_ofType_errorSelector
  , revertToSavedFromFile_ofTypeSelector
  , revertToSavedFromURL_ofTypeSelector
  , runModalPageLayoutWithPrintInfoSelector
  , runModalPageLayoutWithPrintInfo_delegate_didRunSelector_contextInfoSelector
  , runModalPrintOperation_delegate_didRunSelector_contextInfoSelector
  , runModalSavePanelForSaveOperation_delegate_didSaveSelector_contextInfoSelector
  , runPageLayoutSelector
  , saveDocumentAsSelector
  , saveDocumentSelector
  , saveDocumentToPDFSelector
  , saveDocumentToSelector
  , saveDocumentWithDelegate_didSaveSelector_contextInfoSelector
  , savePanelShowsFileFormatsControlSelector
  , savePresentedItemChangesWithCompletionHandlerSelector
  , saveToFile_saveOperation_delegate_didSaveSelector_contextInfoSelector
  , saveToURL_ofType_forSaveOperation_completionHandlerSelector
  , saveToURL_ofType_forSaveOperation_delegate_didSaveSelector_contextInfoSelector
  , saveToURL_ofType_forSaveOperation_errorSelector
  , scheduleAutosavingSelector
  , setAutosavedContentsFileURLSelector
  , setDisplayNameSelector
  , setDraftSelector
  , setFileModificationDateSelector
  , setFileNameSelector
  , setFileTypeSelector
  , setFileURLSelector
  , setHasUndoManagerSelector
  , setLastComponentOfFileNameSelector
  , setPreviewRepresentableActivityItemsSelector
  , setPrintInfoSelector
  , setUndoManagerSelector
  , setUserActivitySelector
  , setWindowSelector
  , shareDocumentWithSharingService_completionHandlerSelector
  , shouldChangePrintInfoSelector
  , shouldCloseWindowController_delegate_shouldCloseSelector_contextInfoSelector
  , shouldRunSavePanelWithAccessoryViewSelector
  , showWindowsSelector
  , stopBrowsingVersionsWithCompletionHandlerSelector
  , unblockUserInteractionSelector
  , undoManagerSelector
  , unlockDocumentSelector
  , unlockDocumentWithCompletionHandlerSelector
  , unlockWithCompletionHandlerSelector
  , updateChangeCountSelector
  , updateChangeCountWithToken_forSaveOperationSelector
  , updateUserActivityStateSelector
  , userActivitySelector
  , usesUbiquitousStorageSelector
  , validateUserInterfaceItemSelector
  , willNotPresentErrorSelector
  , willPresentErrorSelector
  , windowControllerDidLoadNibSelector
  , windowControllerWillLoadNibSelector
  , windowControllersSelector
  , windowForSheetSelector
  , windowNibNameSelector
  , writableTypesForSaveOperationSelector
  , writableTypesSelector
  , writeSafelyToURL_ofType_forSaveOperation_errorSelector
  , writeToFile_ofTypeSelector
  , writeToFile_ofType_originalFile_saveOperationSelector
  , writeToURL_ofTypeSelector
  , writeToURL_ofType_errorSelector
  , writeToURL_ofType_forSaveOperation_originalContentsURL_errorSelector
  , writeWithBackupToFile_ofType_saveOperationSelector

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
init_ :: IsNSDocument nsDocument => nsDocument -> IO (Id NSDocument)
init_ nsDocument =
  sendOwnedMessage nsDocument initSelector

-- | @- initWithType:error:@
initWithType_error :: (IsNSDocument nsDocument, IsNSString typeName, IsNSError outError) => nsDocument -> typeName -> outError -> IO (Id NSDocument)
initWithType_error nsDocument typeName outError =
  sendOwnedMessage nsDocument initWithType_errorSelector (toNSString typeName) (toNSError outError)

-- | @+ canConcurrentlyReadDocumentsOfType:@
canConcurrentlyReadDocumentsOfType :: IsNSString typeName => typeName -> IO Bool
canConcurrentlyReadDocumentsOfType typeName =
  do
    cls' <- getRequiredClass "NSDocument"
    sendClassMessage cls' canConcurrentlyReadDocumentsOfTypeSelector (toNSString typeName)

-- | @- initWithContentsOfURL:ofType:error:@
initWithContentsOfURL_ofType_error :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName, IsNSError outError) => nsDocument -> url -> typeName -> outError -> IO (Id NSDocument)
initWithContentsOfURL_ofType_error nsDocument url typeName outError =
  sendOwnedMessage nsDocument initWithContentsOfURL_ofType_errorSelector (toNSURL url) (toNSString typeName) (toNSError outError)

-- | @- initForURL:withContentsOfURL:ofType:error:@
initForURL_withContentsOfURL_ofType_error :: (IsNSDocument nsDocument, IsNSURL urlOrNil, IsNSURL contentsURL, IsNSString typeName, IsNSError outError) => nsDocument -> urlOrNil -> contentsURL -> typeName -> outError -> IO (Id NSDocument)
initForURL_withContentsOfURL_ofType_error nsDocument urlOrNil contentsURL typeName outError =
  sendOwnedMessage nsDocument initForURL_withContentsOfURL_ofType_errorSelector (toNSURL urlOrNil) (toNSURL contentsURL) (toNSString typeName) (toNSError outError)

-- | @- performActivityWithSynchronousWaiting:usingBlock:@
performActivityWithSynchronousWaiting_usingBlock :: IsNSDocument nsDocument => nsDocument -> Bool -> Ptr () -> IO ()
performActivityWithSynchronousWaiting_usingBlock nsDocument waitSynchronously block =
  sendMessage nsDocument performActivityWithSynchronousWaiting_usingBlockSelector waitSynchronously block

-- | @- continueActivityUsingBlock:@
continueActivityUsingBlock :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
continueActivityUsingBlock nsDocument block =
  sendMessage nsDocument continueActivityUsingBlockSelector block

-- | @- continueAsynchronousWorkOnMainThreadUsingBlock:@
continueAsynchronousWorkOnMainThreadUsingBlock :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
continueAsynchronousWorkOnMainThreadUsingBlock nsDocument block =
  sendMessage nsDocument continueAsynchronousWorkOnMainThreadUsingBlockSelector block

-- | @- performSynchronousFileAccessUsingBlock:@
performSynchronousFileAccessUsingBlock :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
performSynchronousFileAccessUsingBlock nsDocument block =
  sendMessage nsDocument performSynchronousFileAccessUsingBlockSelector block

-- | @- performAsynchronousFileAccessUsingBlock:@
performAsynchronousFileAccessUsingBlock :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
performAsynchronousFileAccessUsingBlock nsDocument block =
  sendMessage nsDocument performAsynchronousFileAccessUsingBlockSelector block

-- | @- revertDocumentToSaved:@
revertDocumentToSaved :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
revertDocumentToSaved nsDocument sender =
  sendMessage nsDocument revertDocumentToSavedSelector sender

-- | @- revertToContentsOfURL:ofType:error:@
revertToContentsOfURL_ofType_error :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName, IsNSError outError) => nsDocument -> url -> typeName -> outError -> IO Bool
revertToContentsOfURL_ofType_error nsDocument url typeName outError =
  sendMessage nsDocument revertToContentsOfURL_ofType_errorSelector (toNSURL url) (toNSString typeName) (toNSError outError)

-- | @- readFromURL:ofType:error:@
readFromURL_ofType_error :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName, IsNSError outError) => nsDocument -> url -> typeName -> outError -> IO Bool
readFromURL_ofType_error nsDocument url typeName outError =
  sendMessage nsDocument readFromURL_ofType_errorSelector (toNSURL url) (toNSString typeName) (toNSError outError)

-- | @- readFromFileWrapper:ofType:error:@
readFromFileWrapper_ofType_error :: (IsNSDocument nsDocument, IsNSFileWrapper fileWrapper, IsNSString typeName, IsNSError outError) => nsDocument -> fileWrapper -> typeName -> outError -> IO Bool
readFromFileWrapper_ofType_error nsDocument fileWrapper typeName outError =
  sendMessage nsDocument readFromFileWrapper_ofType_errorSelector (toNSFileWrapper fileWrapper) (toNSString typeName) (toNSError outError)

-- | @- readFromData:ofType:error:@
readFromData_ofType_error :: (IsNSDocument nsDocument, IsNSData data_, IsNSString typeName, IsNSError outError) => nsDocument -> data_ -> typeName -> outError -> IO Bool
readFromData_ofType_error nsDocument data_ typeName outError =
  sendMessage nsDocument readFromData_ofType_errorSelector (toNSData data_) (toNSString typeName) (toNSError outError)

-- | @- writeToURL:ofType:error:@
writeToURL_ofType_error :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName, IsNSError outError) => nsDocument -> url -> typeName -> outError -> IO Bool
writeToURL_ofType_error nsDocument url typeName outError =
  sendMessage nsDocument writeToURL_ofType_errorSelector (toNSURL url) (toNSString typeName) (toNSError outError)

-- | @- fileWrapperOfType:error:@
fileWrapperOfType_error :: (IsNSDocument nsDocument, IsNSString typeName, IsNSError outError) => nsDocument -> typeName -> outError -> IO (Id NSFileWrapper)
fileWrapperOfType_error nsDocument typeName outError =
  sendMessage nsDocument fileWrapperOfType_errorSelector (toNSString typeName) (toNSError outError)

-- | @- dataOfType:error:@
dataOfType_error :: (IsNSDocument nsDocument, IsNSString typeName, IsNSError outError) => nsDocument -> typeName -> outError -> IO (Id NSData)
dataOfType_error nsDocument typeName outError =
  sendMessage nsDocument dataOfType_errorSelector (toNSString typeName) (toNSError outError)

-- | @- unblockUserInteraction@
unblockUserInteraction :: IsNSDocument nsDocument => nsDocument -> IO ()
unblockUserInteraction nsDocument =
  sendMessage nsDocument unblockUserInteractionSelector

-- | @- writeSafelyToURL:ofType:forSaveOperation:error:@
writeSafelyToURL_ofType_forSaveOperation_error :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName, IsNSError outError) => nsDocument -> url -> typeName -> NSSaveOperationType -> outError -> IO Bool
writeSafelyToURL_ofType_forSaveOperation_error nsDocument url typeName saveOperation outError =
  sendMessage nsDocument writeSafelyToURL_ofType_forSaveOperation_errorSelector (toNSURL url) (toNSString typeName) saveOperation (toNSError outError)

-- | @- writeToURL:ofType:forSaveOperation:originalContentsURL:error:@
writeToURL_ofType_forSaveOperation_originalContentsURL_error :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName, IsNSURL absoluteOriginalContentsURL, IsNSError outError) => nsDocument -> url -> typeName -> NSSaveOperationType -> absoluteOriginalContentsURL -> outError -> IO Bool
writeToURL_ofType_forSaveOperation_originalContentsURL_error nsDocument url typeName saveOperation absoluteOriginalContentsURL outError =
  sendMessage nsDocument writeToURL_ofType_forSaveOperation_originalContentsURL_errorSelector (toNSURL url) (toNSString typeName) saveOperation (toNSURL absoluteOriginalContentsURL) (toNSError outError)

-- | @- fileAttributesToWriteToURL:ofType:forSaveOperation:originalContentsURL:error:@
fileAttributesToWriteToURL_ofType_forSaveOperation_originalContentsURL_error :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName, IsNSURL absoluteOriginalContentsURL, IsNSError outError) => nsDocument -> url -> typeName -> NSSaveOperationType -> absoluteOriginalContentsURL -> outError -> IO (Id NSDictionary)
fileAttributesToWriteToURL_ofType_forSaveOperation_originalContentsURL_error nsDocument url typeName saveOperation absoluteOriginalContentsURL outError =
  sendMessage nsDocument fileAttributesToWriteToURL_ofType_forSaveOperation_originalContentsURL_errorSelector (toNSURL url) (toNSString typeName) saveOperation (toNSURL absoluteOriginalContentsURL) (toNSError outError)

-- | @- saveDocument:@
saveDocument :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
saveDocument nsDocument sender =
  sendMessage nsDocument saveDocumentSelector sender

-- | @- saveDocumentAs:@
saveDocumentAs :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
saveDocumentAs nsDocument sender =
  sendMessage nsDocument saveDocumentAsSelector sender

-- | @- saveDocumentTo:@
saveDocumentTo :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
saveDocumentTo nsDocument sender =
  sendMessage nsDocument saveDocumentToSelector sender

-- | @- saveDocumentWithDelegate:didSaveSelector:contextInfo:@
saveDocumentWithDelegate_didSaveSelector_contextInfo :: IsNSDocument nsDocument => nsDocument -> RawId -> Sel -> Ptr () -> IO ()
saveDocumentWithDelegate_didSaveSelector_contextInfo nsDocument delegate didSaveSelector contextInfo =
  sendMessage nsDocument saveDocumentWithDelegate_didSaveSelector_contextInfoSelector delegate didSaveSelector contextInfo

-- | @- runModalSavePanelForSaveOperation:delegate:didSaveSelector:contextInfo:@
runModalSavePanelForSaveOperation_delegate_didSaveSelector_contextInfo :: IsNSDocument nsDocument => nsDocument -> NSSaveOperationType -> RawId -> Sel -> Ptr () -> IO ()
runModalSavePanelForSaveOperation_delegate_didSaveSelector_contextInfo nsDocument saveOperation delegate didSaveSelector contextInfo =
  sendMessage nsDocument runModalSavePanelForSaveOperation_delegate_didSaveSelector_contextInfoSelector saveOperation delegate didSaveSelector contextInfo

-- | @- prepareSavePanel:@
prepareSavePanel :: (IsNSDocument nsDocument, IsNSSavePanel savePanel) => nsDocument -> savePanel -> IO Bool
prepareSavePanel nsDocument savePanel =
  sendMessage nsDocument prepareSavePanelSelector (toNSSavePanel savePanel)

-- | @- saveToURL:ofType:forSaveOperation:delegate:didSaveSelector:contextInfo:@
saveToURL_ofType_forSaveOperation_delegate_didSaveSelector_contextInfo :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName) => nsDocument -> url -> typeName -> NSSaveOperationType -> RawId -> Sel -> Ptr () -> IO ()
saveToURL_ofType_forSaveOperation_delegate_didSaveSelector_contextInfo nsDocument url typeName saveOperation delegate didSaveSelector contextInfo =
  sendMessage nsDocument saveToURL_ofType_forSaveOperation_delegate_didSaveSelector_contextInfoSelector (toNSURL url) (toNSString typeName) saveOperation delegate didSaveSelector contextInfo

-- | @- saveToURL:ofType:forSaveOperation:completionHandler:@
saveToURL_ofType_forSaveOperation_completionHandler :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName) => nsDocument -> url -> typeName -> NSSaveOperationType -> Ptr () -> IO ()
saveToURL_ofType_forSaveOperation_completionHandler nsDocument url typeName saveOperation completionHandler =
  sendMessage nsDocument saveToURL_ofType_forSaveOperation_completionHandlerSelector (toNSURL url) (toNSString typeName) saveOperation completionHandler

-- | @- canAsynchronouslyWriteToURL:ofType:forSaveOperation:@
canAsynchronouslyWriteToURL_ofType_forSaveOperation :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName) => nsDocument -> url -> typeName -> NSSaveOperationType -> IO Bool
canAsynchronouslyWriteToURL_ofType_forSaveOperation nsDocument url typeName saveOperation =
  sendMessage nsDocument canAsynchronouslyWriteToURL_ofType_forSaveOperationSelector (toNSURL url) (toNSString typeName) saveOperation

-- | @- checkAutosavingSafetyAndReturnError:@
checkAutosavingSafetyAndReturnError :: (IsNSDocument nsDocument, IsNSError outError) => nsDocument -> outError -> IO Bool
checkAutosavingSafetyAndReturnError nsDocument outError =
  sendMessage nsDocument checkAutosavingSafetyAndReturnErrorSelector (toNSError outError)

-- | @- scheduleAutosaving@
scheduleAutosaving :: IsNSDocument nsDocument => nsDocument -> IO ()
scheduleAutosaving nsDocument =
  sendMessage nsDocument scheduleAutosavingSelector

-- | @- autosaveDocumentWithDelegate:didAutosaveSelector:contextInfo:@
autosaveDocumentWithDelegate_didAutosaveSelector_contextInfo :: IsNSDocument nsDocument => nsDocument -> RawId -> Sel -> Ptr () -> IO ()
autosaveDocumentWithDelegate_didAutosaveSelector_contextInfo nsDocument delegate didAutosaveSelector contextInfo =
  sendMessage nsDocument autosaveDocumentWithDelegate_didAutosaveSelector_contextInfoSelector delegate didAutosaveSelector contextInfo

-- | @- autosaveWithImplicitCancellability:completionHandler:@
autosaveWithImplicitCancellability_completionHandler :: IsNSDocument nsDocument => nsDocument -> Bool -> Ptr () -> IO ()
autosaveWithImplicitCancellability_completionHandler nsDocument autosavingIsImplicitlyCancellable completionHandler =
  sendMessage nsDocument autosaveWithImplicitCancellability_completionHandlerSelector autosavingIsImplicitlyCancellable completionHandler

-- | @- browseDocumentVersions:@
browseDocumentVersions :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
browseDocumentVersions nsDocument sender =
  sendMessage nsDocument browseDocumentVersionsSelector sender

-- | @- stopBrowsingVersionsWithCompletionHandler:@
stopBrowsingVersionsWithCompletionHandler :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
stopBrowsingVersionsWithCompletionHandler nsDocument completionHandler =
  sendMessage nsDocument stopBrowsingVersionsWithCompletionHandlerSelector completionHandler

-- | @- canCloseDocumentWithDelegate:shouldCloseSelector:contextInfo:@
canCloseDocumentWithDelegate_shouldCloseSelector_contextInfo :: IsNSDocument nsDocument => nsDocument -> RawId -> Sel -> Ptr () -> IO ()
canCloseDocumentWithDelegate_shouldCloseSelector_contextInfo nsDocument delegate shouldCloseSelector contextInfo =
  sendMessage nsDocument canCloseDocumentWithDelegate_shouldCloseSelector_contextInfoSelector delegate shouldCloseSelector contextInfo

-- | @- close@
close :: IsNSDocument nsDocument => nsDocument -> IO ()
close nsDocument =
  sendMessage nsDocument closeSelector

-- | @- duplicateDocument:@
duplicateDocument :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
duplicateDocument nsDocument sender =
  sendMessage nsDocument duplicateDocumentSelector sender

-- | @- duplicateDocumentWithDelegate:didDuplicateSelector:contextInfo:@
duplicateDocumentWithDelegate_didDuplicateSelector_contextInfo :: IsNSDocument nsDocument => nsDocument -> RawId -> Sel -> Ptr () -> IO ()
duplicateDocumentWithDelegate_didDuplicateSelector_contextInfo nsDocument delegate didDuplicateSelector contextInfo =
  sendMessage nsDocument duplicateDocumentWithDelegate_didDuplicateSelector_contextInfoSelector delegate didDuplicateSelector contextInfo

-- | @- duplicateAndReturnError:@
duplicateAndReturnError :: (IsNSDocument nsDocument, IsNSError outError) => nsDocument -> outError -> IO (Id NSDocument)
duplicateAndReturnError nsDocument outError =
  sendMessage nsDocument duplicateAndReturnErrorSelector (toNSError outError)

-- | @- renameDocument:@
renameDocument :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
renameDocument nsDocument sender =
  sendMessage nsDocument renameDocumentSelector sender

-- | @- moveDocumentToUbiquityContainer:@
moveDocumentToUbiquityContainer :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
moveDocumentToUbiquityContainer nsDocument sender =
  sendMessage nsDocument moveDocumentToUbiquityContainerSelector sender

-- | @- moveDocument:@
moveDocument :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
moveDocument nsDocument sender =
  sendMessage nsDocument moveDocumentSelector sender

-- | @- moveDocumentWithCompletionHandler:@
moveDocumentWithCompletionHandler :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
moveDocumentWithCompletionHandler nsDocument completionHandler =
  sendMessage nsDocument moveDocumentWithCompletionHandlerSelector completionHandler

-- | @- moveToURL:completionHandler:@
moveToURL_completionHandler :: (IsNSDocument nsDocument, IsNSURL url) => nsDocument -> url -> Ptr () -> IO ()
moveToURL_completionHandler nsDocument url completionHandler =
  sendMessage nsDocument moveToURL_completionHandlerSelector (toNSURL url) completionHandler

-- | @- lockDocument:@
lockDocument :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
lockDocument nsDocument sender =
  sendMessage nsDocument lockDocumentSelector sender

-- | @- unlockDocument:@
unlockDocument :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
unlockDocument nsDocument sender =
  sendMessage nsDocument unlockDocumentSelector sender

-- | @- lockDocumentWithCompletionHandler:@
lockDocumentWithCompletionHandler :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
lockDocumentWithCompletionHandler nsDocument completionHandler =
  sendMessage nsDocument lockDocumentWithCompletionHandlerSelector completionHandler

-- | @- lockWithCompletionHandler:@
lockWithCompletionHandler :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
lockWithCompletionHandler nsDocument completionHandler =
  sendMessage nsDocument lockWithCompletionHandlerSelector completionHandler

-- | @- unlockDocumentWithCompletionHandler:@
unlockDocumentWithCompletionHandler :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
unlockDocumentWithCompletionHandler nsDocument completionHandler =
  sendMessage nsDocument unlockDocumentWithCompletionHandlerSelector completionHandler

-- | @- unlockWithCompletionHandler:@
unlockWithCompletionHandler :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
unlockWithCompletionHandler nsDocument completionHandler =
  sendMessage nsDocument unlockWithCompletionHandlerSelector completionHandler

-- | @- runPageLayout:@
runPageLayout :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
runPageLayout nsDocument sender =
  sendMessage nsDocument runPageLayoutSelector sender

-- | @- runModalPageLayoutWithPrintInfo:delegate:didRunSelector:contextInfo:@
runModalPageLayoutWithPrintInfo_delegate_didRunSelector_contextInfo :: (IsNSDocument nsDocument, IsNSPrintInfo printInfo) => nsDocument -> printInfo -> RawId -> Sel -> Ptr () -> IO ()
runModalPageLayoutWithPrintInfo_delegate_didRunSelector_contextInfo nsDocument printInfo delegate didRunSelector contextInfo =
  sendMessage nsDocument runModalPageLayoutWithPrintInfo_delegate_didRunSelector_contextInfoSelector (toNSPrintInfo printInfo) delegate didRunSelector contextInfo

-- | @- preparePageLayout:@
preparePageLayout :: (IsNSDocument nsDocument, IsNSPageLayout pageLayout) => nsDocument -> pageLayout -> IO Bool
preparePageLayout nsDocument pageLayout =
  sendMessage nsDocument preparePageLayoutSelector (toNSPageLayout pageLayout)

-- | @- shouldChangePrintInfo:@
shouldChangePrintInfo :: (IsNSDocument nsDocument, IsNSPrintInfo newPrintInfo) => nsDocument -> newPrintInfo -> IO Bool
shouldChangePrintInfo nsDocument newPrintInfo =
  sendMessage nsDocument shouldChangePrintInfoSelector (toNSPrintInfo newPrintInfo)

-- | @- printDocument:@
printDocument :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
printDocument nsDocument sender =
  sendMessage nsDocument printDocumentSelector sender

-- | @- printDocumentWithSettings:showPrintPanel:delegate:didPrintSelector:contextInfo:@
printDocumentWithSettings_showPrintPanel_delegate_didPrintSelector_contextInfo :: (IsNSDocument nsDocument, IsNSDictionary printSettings) => nsDocument -> printSettings -> Bool -> RawId -> Sel -> Ptr () -> IO ()
printDocumentWithSettings_showPrintPanel_delegate_didPrintSelector_contextInfo nsDocument printSettings showPrintPanel delegate didPrintSelector contextInfo =
  sendMessage nsDocument printDocumentWithSettings_showPrintPanel_delegate_didPrintSelector_contextInfoSelector (toNSDictionary printSettings) showPrintPanel delegate didPrintSelector contextInfo

-- | @- printOperationWithSettings:error:@
printOperationWithSettings_error :: (IsNSDocument nsDocument, IsNSDictionary printSettings, IsNSError outError) => nsDocument -> printSettings -> outError -> IO (Id NSPrintOperation)
printOperationWithSettings_error nsDocument printSettings outError =
  sendMessage nsDocument printOperationWithSettings_errorSelector (toNSDictionary printSettings) (toNSError outError)

-- | @- runModalPrintOperation:delegate:didRunSelector:contextInfo:@
runModalPrintOperation_delegate_didRunSelector_contextInfo :: (IsNSDocument nsDocument, IsNSPrintOperation printOperation) => nsDocument -> printOperation -> RawId -> Sel -> Ptr () -> IO ()
runModalPrintOperation_delegate_didRunSelector_contextInfo nsDocument printOperation delegate didRunSelector contextInfo =
  sendMessage nsDocument runModalPrintOperation_delegate_didRunSelector_contextInfoSelector (toNSPrintOperation printOperation) delegate didRunSelector contextInfo

-- | @- saveDocumentToPDF:@
saveDocumentToPDF :: IsNSDocument nsDocument => nsDocument -> RawId -> IO ()
saveDocumentToPDF nsDocument sender =
  sendMessage nsDocument saveDocumentToPDFSelector sender

-- | @- shareDocumentWithSharingService:completionHandler:@
shareDocumentWithSharingService_completionHandler :: (IsNSDocument nsDocument, IsNSSharingService sharingService) => nsDocument -> sharingService -> Ptr () -> IO ()
shareDocumentWithSharingService_completionHandler nsDocument sharingService completionHandler =
  sendMessage nsDocument shareDocumentWithSharingService_completionHandlerSelector (toNSSharingService sharingService) completionHandler

-- | @- prepareSharingServicePicker:@
prepareSharingServicePicker :: (IsNSDocument nsDocument, IsNSSharingServicePicker sharingServicePicker) => nsDocument -> sharingServicePicker -> IO ()
prepareSharingServicePicker nsDocument sharingServicePicker =
  sendMessage nsDocument prepareSharingServicePickerSelector (toNSSharingServicePicker sharingServicePicker)

-- | @- updateChangeCount:@
updateChangeCount :: IsNSDocument nsDocument => nsDocument -> NSDocumentChangeType -> IO ()
updateChangeCount nsDocument change =
  sendMessage nsDocument updateChangeCountSelector change

-- | @- changeCountTokenForSaveOperation:@
changeCountTokenForSaveOperation :: IsNSDocument nsDocument => nsDocument -> NSSaveOperationType -> IO RawId
changeCountTokenForSaveOperation nsDocument saveOperation =
  sendMessage nsDocument changeCountTokenForSaveOperationSelector saveOperation

-- | @- updateChangeCountWithToken:forSaveOperation:@
updateChangeCountWithToken_forSaveOperation :: IsNSDocument nsDocument => nsDocument -> RawId -> NSSaveOperationType -> IO ()
updateChangeCountWithToken_forSaveOperation nsDocument changeCountToken saveOperation =
  sendMessage nsDocument updateChangeCountWithToken_forSaveOperationSelector changeCountToken saveOperation

-- | @- presentError:modalForWindow:delegate:didPresentSelector:contextInfo:@
presentError_modalForWindow_delegate_didPresentSelector_contextInfo :: (IsNSDocument nsDocument, IsNSError error_, IsNSWindow window) => nsDocument -> error_ -> window -> RawId -> Sel -> Ptr () -> IO ()
presentError_modalForWindow_delegate_didPresentSelector_contextInfo nsDocument error_ window delegate didPresentSelector contextInfo =
  sendMessage nsDocument presentError_modalForWindow_delegate_didPresentSelector_contextInfoSelector (toNSError error_) (toNSWindow window) delegate didPresentSelector contextInfo

-- | @- presentError:@
presentError :: (IsNSDocument nsDocument, IsNSError error_) => nsDocument -> error_ -> IO Bool
presentError nsDocument error_ =
  sendMessage nsDocument presentErrorSelector (toNSError error_)

-- | @- willPresentError:@
willPresentError :: (IsNSDocument nsDocument, IsNSError error_) => nsDocument -> error_ -> IO (Id NSError)
willPresentError nsDocument error_ =
  sendMessage nsDocument willPresentErrorSelector (toNSError error_)

-- | @- willNotPresentError:@
willNotPresentError :: (IsNSDocument nsDocument, IsNSError error_) => nsDocument -> error_ -> IO ()
willNotPresentError nsDocument error_ =
  sendMessage nsDocument willNotPresentErrorSelector (toNSError error_)

-- | @- makeWindowControllers@
makeWindowControllers :: IsNSDocument nsDocument => nsDocument -> IO ()
makeWindowControllers nsDocument =
  sendMessage nsDocument makeWindowControllersSelector

-- | @- windowControllerWillLoadNib:@
windowControllerWillLoadNib :: (IsNSDocument nsDocument, IsNSWindowController windowController) => nsDocument -> windowController -> IO ()
windowControllerWillLoadNib nsDocument windowController =
  sendMessage nsDocument windowControllerWillLoadNibSelector (toNSWindowController windowController)

-- | @- windowControllerDidLoadNib:@
windowControllerDidLoadNib :: (IsNSDocument nsDocument, IsNSWindowController windowController) => nsDocument -> windowController -> IO ()
windowControllerDidLoadNib nsDocument windowController =
  sendMessage nsDocument windowControllerDidLoadNibSelector (toNSWindowController windowController)

-- | @- setWindow:@
setWindow :: (IsNSDocument nsDocument, IsNSWindow window) => nsDocument -> window -> IO ()
setWindow nsDocument window =
  sendMessage nsDocument setWindowSelector (toNSWindow window)

-- | @- addWindowController:@
addWindowController :: (IsNSDocument nsDocument, IsNSWindowController windowController) => nsDocument -> windowController -> IO ()
addWindowController nsDocument windowController =
  sendMessage nsDocument addWindowControllerSelector (toNSWindowController windowController)

-- | @- removeWindowController:@
removeWindowController :: (IsNSDocument nsDocument, IsNSWindowController windowController) => nsDocument -> windowController -> IO ()
removeWindowController nsDocument windowController =
  sendMessage nsDocument removeWindowControllerSelector (toNSWindowController windowController)

-- | @- showWindows@
showWindows :: IsNSDocument nsDocument => nsDocument -> IO ()
showWindows nsDocument =
  sendMessage nsDocument showWindowsSelector

-- | @- shouldCloseWindowController:delegate:shouldCloseSelector:contextInfo:@
shouldCloseWindowController_delegate_shouldCloseSelector_contextInfo :: (IsNSDocument nsDocument, IsNSWindowController windowController) => nsDocument -> windowController -> RawId -> Sel -> Ptr () -> IO ()
shouldCloseWindowController_delegate_shouldCloseSelector_contextInfo nsDocument windowController delegate shouldCloseSelector contextInfo =
  sendMessage nsDocument shouldCloseWindowController_delegate_shouldCloseSelector_contextInfoSelector (toNSWindowController windowController) delegate shouldCloseSelector contextInfo

-- | @- setDisplayName:@
setDisplayName :: (IsNSDocument nsDocument, IsNSString displayNameOrNil) => nsDocument -> displayNameOrNil -> IO ()
setDisplayName nsDocument displayNameOrNil =
  sendMessage nsDocument setDisplayNameSelector (toNSString displayNameOrNil)

-- | @- defaultDraftName@
defaultDraftName :: IsNSDocument nsDocument => nsDocument -> IO (Id NSString)
defaultDraftName nsDocument =
  sendMessage nsDocument defaultDraftNameSelector

-- | @+ isNativeType:@
isNativeType :: IsNSString type_ => type_ -> IO Bool
isNativeType type_ =
  do
    cls' <- getRequiredClass "NSDocument"
    sendClassMessage cls' isNativeTypeSelector (toNSString type_)

-- | @- writableTypesForSaveOperation:@
writableTypesForSaveOperation :: IsNSDocument nsDocument => nsDocument -> NSSaveOperationType -> IO (Id NSArray)
writableTypesForSaveOperation nsDocument saveOperation =
  sendMessage nsDocument writableTypesForSaveOperationSelector saveOperation

-- | @- fileNameExtensionForType:saveOperation:@
fileNameExtensionForType_saveOperation :: (IsNSDocument nsDocument, IsNSString typeName) => nsDocument -> typeName -> NSSaveOperationType -> IO (Id NSString)
fileNameExtensionForType_saveOperation nsDocument typeName saveOperation =
  sendMessage nsDocument fileNameExtensionForType_saveOperationSelector (toNSString typeName) saveOperation

-- | @- validateUserInterfaceItem:@
validateUserInterfaceItem :: IsNSDocument nsDocument => nsDocument -> RawId -> IO Bool
validateUserInterfaceItem nsDocument item =
  sendMessage nsDocument validateUserInterfaceItemSelector item

-- | @- relinquishPresentedItemToReader:@
relinquishPresentedItemToReader :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
relinquishPresentedItemToReader nsDocument reader =
  sendMessage nsDocument relinquishPresentedItemToReaderSelector reader

-- | @- relinquishPresentedItemToWriter:@
relinquishPresentedItemToWriter :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
relinquishPresentedItemToWriter nsDocument writer =
  sendMessage nsDocument relinquishPresentedItemToWriterSelector writer

-- | @- savePresentedItemChangesWithCompletionHandler:@
savePresentedItemChangesWithCompletionHandler :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
savePresentedItemChangesWithCompletionHandler nsDocument completionHandler =
  sendMessage nsDocument savePresentedItemChangesWithCompletionHandlerSelector completionHandler

-- | @- accommodatePresentedItemDeletionWithCompletionHandler:@
accommodatePresentedItemDeletionWithCompletionHandler :: IsNSDocument nsDocument => nsDocument -> Ptr () -> IO ()
accommodatePresentedItemDeletionWithCompletionHandler nsDocument completionHandler =
  sendMessage nsDocument accommodatePresentedItemDeletionWithCompletionHandlerSelector completionHandler

-- | @- presentedItemDidMoveToURL:@
presentedItemDidMoveToURL :: (IsNSDocument nsDocument, IsNSURL newURL) => nsDocument -> newURL -> IO ()
presentedItemDidMoveToURL nsDocument newURL =
  sendMessage nsDocument presentedItemDidMoveToURLSelector (toNSURL newURL)

-- | @- presentedItemDidChange@
presentedItemDidChange :: IsNSDocument nsDocument => nsDocument -> IO ()
presentedItemDidChange nsDocument =
  sendMessage nsDocument presentedItemDidChangeSelector

-- | @- presentedItemDidChangeUbiquityAttributes:@
presentedItemDidChangeUbiquityAttributes :: (IsNSDocument nsDocument, IsNSSet attributes) => nsDocument -> attributes -> IO ()
presentedItemDidChangeUbiquityAttributes nsDocument attributes =
  sendMessage nsDocument presentedItemDidChangeUbiquityAttributesSelector (toNSSet attributes)

-- | @- presentedItemDidGainVersion:@
presentedItemDidGainVersion :: (IsNSDocument nsDocument, IsNSFileVersion version) => nsDocument -> version -> IO ()
presentedItemDidGainVersion nsDocument version =
  sendMessage nsDocument presentedItemDidGainVersionSelector (toNSFileVersion version)

-- | @- presentedItemDidLoseVersion:@
presentedItemDidLoseVersion :: (IsNSDocument nsDocument, IsNSFileVersion version) => nsDocument -> version -> IO ()
presentedItemDidLoseVersion nsDocument version =
  sendMessage nsDocument presentedItemDidLoseVersionSelector (toNSFileVersion version)

-- | @- presentedItemDidResolveConflictVersion:@
presentedItemDidResolveConflictVersion :: (IsNSDocument nsDocument, IsNSFileVersion version) => nsDocument -> version -> IO ()
presentedItemDidResolveConflictVersion nsDocument version =
  sendMessage nsDocument presentedItemDidResolveConflictVersionSelector (toNSFileVersion version)

-- | @- restoreDocumentWindowWithIdentifier:state:completionHandler:@
restoreDocumentWindowWithIdentifier_state_completionHandler :: (IsNSDocument nsDocument, IsNSString identifier, IsNSCoder state) => nsDocument -> identifier -> state -> Ptr () -> IO ()
restoreDocumentWindowWithIdentifier_state_completionHandler nsDocument identifier state completionHandler =
  sendMessage nsDocument restoreDocumentWindowWithIdentifier_state_completionHandlerSelector (toNSString identifier) (toNSCoder state) completionHandler

-- | @- encodeRestorableStateWithCoder:@
encodeRestorableStateWithCoder :: (IsNSDocument nsDocument, IsNSCoder coder) => nsDocument -> coder -> IO ()
encodeRestorableStateWithCoder nsDocument coder =
  sendMessage nsDocument encodeRestorableStateWithCoderSelector (toNSCoder coder)

-- | @- encodeRestorableStateWithCoder:backgroundQueue:@
encodeRestorableStateWithCoder_backgroundQueue :: (IsNSDocument nsDocument, IsNSCoder coder, IsNSOperationQueue queue) => nsDocument -> coder -> queue -> IO ()
encodeRestorableStateWithCoder_backgroundQueue nsDocument coder queue =
  sendMessage nsDocument encodeRestorableStateWithCoder_backgroundQueueSelector (toNSCoder coder) (toNSOperationQueue queue)

-- | @- restoreStateWithCoder:@
restoreStateWithCoder :: (IsNSDocument nsDocument, IsNSCoder coder) => nsDocument -> coder -> IO ()
restoreStateWithCoder nsDocument coder =
  sendMessage nsDocument restoreStateWithCoderSelector (toNSCoder coder)

-- | @- invalidateRestorableState@
invalidateRestorableState :: IsNSDocument nsDocument => nsDocument -> IO ()
invalidateRestorableState nsDocument =
  sendMessage nsDocument invalidateRestorableStateSelector

-- | @+ allowedClassesForRestorableStateKeyPath:@
allowedClassesForRestorableStateKeyPath :: IsNSString keyPath => keyPath -> IO (Id NSArray)
allowedClassesForRestorableStateKeyPath keyPath =
  do
    cls' <- getRequiredClass "NSDocument"
    sendClassMessage cls' allowedClassesForRestorableStateKeyPathSelector (toNSString keyPath)

-- | @- handleSaveScriptCommand:@
handleSaveScriptCommand :: (IsNSDocument nsDocument, IsNSScriptCommand command) => nsDocument -> command -> IO RawId
handleSaveScriptCommand nsDocument command =
  sendMessage nsDocument handleSaveScriptCommandSelector (toNSScriptCommand command)

-- | @- handleCloseScriptCommand:@
handleCloseScriptCommand :: (IsNSDocument nsDocument, IsNSCloseCommand command) => nsDocument -> command -> IO RawId
handleCloseScriptCommand nsDocument command =
  sendMessage nsDocument handleCloseScriptCommandSelector (toNSCloseCommand command)

-- | @- handlePrintScriptCommand:@
handlePrintScriptCommand :: (IsNSDocument nsDocument, IsNSScriptCommand command) => nsDocument -> command -> IO RawId
handlePrintScriptCommand nsDocument command =
  sendMessage nsDocument handlePrintScriptCommandSelector (toNSScriptCommand command)

-- | @- updateUserActivityState:@
updateUserActivityState :: (IsNSDocument nsDocument, IsNSUserActivity activity) => nsDocument -> activity -> IO ()
updateUserActivityState nsDocument activity =
  sendMessage nsDocument updateUserActivityStateSelector (toNSUserActivity activity)

-- | @- saveToURL:ofType:forSaveOperation:error:@
saveToURL_ofType_forSaveOperation_error :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName, IsNSError outError) => nsDocument -> url -> typeName -> NSSaveOperationType -> outError -> IO Bool
saveToURL_ofType_forSaveOperation_error nsDocument url typeName saveOperation outError =
  sendMessage nsDocument saveToURL_ofType_forSaveOperation_errorSelector (toNSURL url) (toNSString typeName) saveOperation (toNSError outError)

-- | @- dataRepresentationOfType:@
dataRepresentationOfType :: (IsNSDocument nsDocument, IsNSString type_) => nsDocument -> type_ -> IO (Id NSData)
dataRepresentationOfType nsDocument type_ =
  sendMessage nsDocument dataRepresentationOfTypeSelector (toNSString type_)

-- | @- fileAttributesToWriteToFile:ofType:saveOperation:@
fileAttributesToWriteToFile_ofType_saveOperation :: (IsNSDocument nsDocument, IsNSString fullDocumentPath, IsNSString documentTypeName) => nsDocument -> fullDocumentPath -> documentTypeName -> NSSaveOperationType -> IO (Id NSDictionary)
fileAttributesToWriteToFile_ofType_saveOperation nsDocument fullDocumentPath documentTypeName saveOperationType =
  sendMessage nsDocument fileAttributesToWriteToFile_ofType_saveOperationSelector (toNSString fullDocumentPath) (toNSString documentTypeName) saveOperationType

-- | @- fileName@
fileName :: IsNSDocument nsDocument => nsDocument -> IO (Id NSString)
fileName nsDocument =
  sendMessage nsDocument fileNameSelector

-- | @- fileWrapperRepresentationOfType:@
fileWrapperRepresentationOfType :: (IsNSDocument nsDocument, IsNSString type_) => nsDocument -> type_ -> IO (Id NSFileWrapper)
fileWrapperRepresentationOfType nsDocument type_ =
  sendMessage nsDocument fileWrapperRepresentationOfTypeSelector (toNSString type_)

-- | @- initWithContentsOfFile:ofType:@
initWithContentsOfFile_ofType :: (IsNSDocument nsDocument, IsNSString absolutePath, IsNSString typeName) => nsDocument -> absolutePath -> typeName -> IO RawId
initWithContentsOfFile_ofType nsDocument absolutePath typeName =
  sendOwnedMessage nsDocument initWithContentsOfFile_ofTypeSelector (toNSString absolutePath) (toNSString typeName)

-- | @- initWithContentsOfURL:ofType:@
initWithContentsOfURL_ofType :: (IsNSDocument nsDocument, IsNSURL url, IsNSString typeName) => nsDocument -> url -> typeName -> IO RawId
initWithContentsOfURL_ofType nsDocument url typeName =
  sendOwnedMessage nsDocument initWithContentsOfURL_ofTypeSelector (toNSURL url) (toNSString typeName)

-- | @- loadDataRepresentation:ofType:@
loadDataRepresentation_ofType :: (IsNSDocument nsDocument, IsNSData data_, IsNSString type_) => nsDocument -> data_ -> type_ -> IO Bool
loadDataRepresentation_ofType nsDocument data_ type_ =
  sendMessage nsDocument loadDataRepresentation_ofTypeSelector (toNSData data_) (toNSString type_)

-- | @- loadFileWrapperRepresentation:ofType:@
loadFileWrapperRepresentation_ofType :: (IsNSDocument nsDocument, IsNSFileWrapper wrapper, IsNSString type_) => nsDocument -> wrapper -> type_ -> IO Bool
loadFileWrapperRepresentation_ofType nsDocument wrapper type_ =
  sendMessage nsDocument loadFileWrapperRepresentation_ofTypeSelector (toNSFileWrapper wrapper) (toNSString type_)

-- | @- printShowingPrintPanel:@
printShowingPrintPanel :: IsNSDocument nsDocument => nsDocument -> Bool -> IO ()
printShowingPrintPanel nsDocument flag =
  sendMessage nsDocument printShowingPrintPanelSelector flag

-- | @- readFromFile:ofType:@
readFromFile_ofType :: (IsNSDocument nsDocument, IsNSString fileName, IsNSString type_) => nsDocument -> fileName -> type_ -> IO Bool
readFromFile_ofType nsDocument fileName type_ =
  sendMessage nsDocument readFromFile_ofTypeSelector (toNSString fileName) (toNSString type_)

-- | @- readFromURL:ofType:@
readFromURL_ofType :: (IsNSDocument nsDocument, IsNSURL url, IsNSString type_) => nsDocument -> url -> type_ -> IO Bool
readFromURL_ofType nsDocument url type_ =
  sendMessage nsDocument readFromURL_ofTypeSelector (toNSURL url) (toNSString type_)

-- | @- revertToSavedFromFile:ofType:@
revertToSavedFromFile_ofType :: (IsNSDocument nsDocument, IsNSString fileName, IsNSString type_) => nsDocument -> fileName -> type_ -> IO Bool
revertToSavedFromFile_ofType nsDocument fileName type_ =
  sendMessage nsDocument revertToSavedFromFile_ofTypeSelector (toNSString fileName) (toNSString type_)

-- | @- revertToSavedFromURL:ofType:@
revertToSavedFromURL_ofType :: (IsNSDocument nsDocument, IsNSURL url, IsNSString type_) => nsDocument -> url -> type_ -> IO Bool
revertToSavedFromURL_ofType nsDocument url type_ =
  sendMessage nsDocument revertToSavedFromURL_ofTypeSelector (toNSURL url) (toNSString type_)

-- | @- runModalPageLayoutWithPrintInfo:@
runModalPageLayoutWithPrintInfo :: (IsNSDocument nsDocument, IsNSPrintInfo printInfo) => nsDocument -> printInfo -> IO CLong
runModalPageLayoutWithPrintInfo nsDocument printInfo =
  sendMessage nsDocument runModalPageLayoutWithPrintInfoSelector (toNSPrintInfo printInfo)

-- | @- saveToFile:saveOperation:delegate:didSaveSelector:contextInfo:@
saveToFile_saveOperation_delegate_didSaveSelector_contextInfo :: (IsNSDocument nsDocument, IsNSString fileName) => nsDocument -> fileName -> NSSaveOperationType -> RawId -> Sel -> Ptr () -> IO ()
saveToFile_saveOperation_delegate_didSaveSelector_contextInfo nsDocument fileName saveOperation delegate didSaveSelector contextInfo =
  sendMessage nsDocument saveToFile_saveOperation_delegate_didSaveSelector_contextInfoSelector (toNSString fileName) saveOperation delegate didSaveSelector contextInfo

-- | @- setFileName:@
setFileName :: (IsNSDocument nsDocument, IsNSString fileName) => nsDocument -> fileName -> IO ()
setFileName nsDocument fileName =
  sendMessage nsDocument setFileNameSelector (toNSString fileName)

-- | @- writeToFile:ofType:@
writeToFile_ofType :: (IsNSDocument nsDocument, IsNSString fileName, IsNSString type_) => nsDocument -> fileName -> type_ -> IO Bool
writeToFile_ofType nsDocument fileName type_ =
  sendMessage nsDocument writeToFile_ofTypeSelector (toNSString fileName) (toNSString type_)

-- | @- writeToFile:ofType:originalFile:saveOperation:@
writeToFile_ofType_originalFile_saveOperation :: (IsNSDocument nsDocument, IsNSString fullDocumentPath, IsNSString documentTypeName, IsNSString fullOriginalDocumentPath) => nsDocument -> fullDocumentPath -> documentTypeName -> fullOriginalDocumentPath -> NSSaveOperationType -> IO Bool
writeToFile_ofType_originalFile_saveOperation nsDocument fullDocumentPath documentTypeName fullOriginalDocumentPath saveOperationType =
  sendMessage nsDocument writeToFile_ofType_originalFile_saveOperationSelector (toNSString fullDocumentPath) (toNSString documentTypeName) (toNSString fullOriginalDocumentPath) saveOperationType

-- | @- writeToURL:ofType:@
writeToURL_ofType :: (IsNSDocument nsDocument, IsNSURL url, IsNSString type_) => nsDocument -> url -> type_ -> IO Bool
writeToURL_ofType nsDocument url type_ =
  sendMessage nsDocument writeToURL_ofTypeSelector (toNSURL url) (toNSString type_)

-- | @- writeWithBackupToFile:ofType:saveOperation:@
writeWithBackupToFile_ofType_saveOperation :: (IsNSDocument nsDocument, IsNSString fullDocumentPath, IsNSString documentTypeName) => nsDocument -> fullDocumentPath -> documentTypeName -> NSSaveOperationType -> IO Bool
writeWithBackupToFile_ofType_saveOperation nsDocument fullDocumentPath documentTypeName saveOperationType =
  sendMessage nsDocument writeWithBackupToFile_ofType_saveOperationSelector (toNSString fullDocumentPath) (toNSString documentTypeName) saveOperationType

-- | @- fileType@
fileType :: IsNSDocument nsDocument => nsDocument -> IO (Id NSString)
fileType nsDocument =
  sendMessage nsDocument fileTypeSelector

-- | @- setFileType:@
setFileType :: (IsNSDocument nsDocument, IsNSString value) => nsDocument -> value -> IO ()
setFileType nsDocument value =
  sendMessage nsDocument setFileTypeSelector (toNSString value)

-- | @- fileURL@
fileURL :: IsNSDocument nsDocument => nsDocument -> IO (Id NSURL)
fileURL nsDocument =
  sendMessage nsDocument fileURLSelector

-- | @- setFileURL:@
setFileURL :: (IsNSDocument nsDocument, IsNSURL value) => nsDocument -> value -> IO ()
setFileURL nsDocument value =
  sendMessage nsDocument setFileURLSelector (toNSURL value)

-- | @- fileModificationDate@
fileModificationDate :: IsNSDocument nsDocument => nsDocument -> IO (Id NSDate)
fileModificationDate nsDocument =
  sendMessage nsDocument fileModificationDateSelector

-- | @- setFileModificationDate:@
setFileModificationDate :: (IsNSDocument nsDocument, IsNSDate value) => nsDocument -> value -> IO ()
setFileModificationDate nsDocument value =
  sendMessage nsDocument setFileModificationDateSelector (toNSDate value)

-- | @- draft@
draft :: IsNSDocument nsDocument => nsDocument -> IO Bool
draft nsDocument =
  sendMessage nsDocument draftSelector

-- | @- setDraft:@
setDraft :: IsNSDocument nsDocument => nsDocument -> Bool -> IO ()
setDraft nsDocument value =
  sendMessage nsDocument setDraftSelector value

-- | @- entireFileLoaded@
entireFileLoaded :: IsNSDocument nsDocument => nsDocument -> IO Bool
entireFileLoaded nsDocument =
  sendMessage nsDocument entireFileLoadedSelector

-- | @- autosavingIsImplicitlyCancellable@
autosavingIsImplicitlyCancellable :: IsNSDocument nsDocument => nsDocument -> IO Bool
autosavingIsImplicitlyCancellable nsDocument =
  sendMessage nsDocument autosavingIsImplicitlyCancellableSelector

-- | @- keepBackupFile@
keepBackupFile :: IsNSDocument nsDocument => nsDocument -> IO Bool
keepBackupFile nsDocument =
  sendMessage nsDocument keepBackupFileSelector

-- | @- backupFileURL@
backupFileURL :: IsNSDocument nsDocument => nsDocument -> IO (Id NSURL)
backupFileURL nsDocument =
  sendMessage nsDocument backupFileURLSelector

-- | @- savePanelShowsFileFormatsControl@
savePanelShowsFileFormatsControl :: IsNSDocument nsDocument => nsDocument -> IO Bool
savePanelShowsFileFormatsControl nsDocument =
  sendMessage nsDocument savePanelShowsFileFormatsControlSelector

-- | @- fileNameExtensionWasHiddenInLastRunSavePanel@
fileNameExtensionWasHiddenInLastRunSavePanel :: IsNSDocument nsDocument => nsDocument -> IO Bool
fileNameExtensionWasHiddenInLastRunSavePanel nsDocument =
  sendMessage nsDocument fileNameExtensionWasHiddenInLastRunSavePanelSelector

-- | @- fileTypeFromLastRunSavePanel@
fileTypeFromLastRunSavePanel :: IsNSDocument nsDocument => nsDocument -> IO (Id NSString)
fileTypeFromLastRunSavePanel nsDocument =
  sendMessage nsDocument fileTypeFromLastRunSavePanelSelector

-- | @- hasUnautosavedChanges@
hasUnautosavedChanges :: IsNSDocument nsDocument => nsDocument -> IO Bool
hasUnautosavedChanges nsDocument =
  sendMessage nsDocument hasUnautosavedChangesSelector

-- | @+ autosavesInPlace@
autosavesInPlace :: IO Bool
autosavesInPlace  =
  do
    cls' <- getRequiredClass "NSDocument"
    sendClassMessage cls' autosavesInPlaceSelector

-- | @+ preservesVersions@
preservesVersions :: IO Bool
preservesVersions  =
  do
    cls' <- getRequiredClass "NSDocument"
    sendClassMessage cls' preservesVersionsSelector

-- | @- browsingVersions@
browsingVersions :: IsNSDocument nsDocument => nsDocument -> IO Bool
browsingVersions nsDocument =
  sendMessage nsDocument browsingVersionsSelector

-- | @+ autosavesDrafts@
autosavesDrafts :: IO Bool
autosavesDrafts  =
  do
    cls' <- getRequiredClass "NSDocument"
    sendClassMessage cls' autosavesDraftsSelector

-- | @- autosavingFileType@
autosavingFileType :: IsNSDocument nsDocument => nsDocument -> IO (Id NSString)
autosavingFileType nsDocument =
  sendMessage nsDocument autosavingFileTypeSelector

-- | @- autosavedContentsFileURL@
autosavedContentsFileURL :: IsNSDocument nsDocument => nsDocument -> IO (Id NSURL)
autosavedContentsFileURL nsDocument =
  sendMessage nsDocument autosavedContentsFileURLSelector

-- | @- setAutosavedContentsFileURL:@
setAutosavedContentsFileURL :: (IsNSDocument nsDocument, IsNSURL value) => nsDocument -> value -> IO ()
setAutosavedContentsFileURL nsDocument value =
  sendMessage nsDocument setAutosavedContentsFileURLSelector (toNSURL value)

-- | @- locked@
locked :: IsNSDocument nsDocument => nsDocument -> IO Bool
locked nsDocument =
  sendMessage nsDocument lockedSelector

-- | @- printInfo@
printInfo :: IsNSDocument nsDocument => nsDocument -> IO (Id NSPrintInfo)
printInfo nsDocument =
  sendMessage nsDocument printInfoSelector

-- | @- setPrintInfo:@
setPrintInfo :: (IsNSDocument nsDocument, IsNSPrintInfo value) => nsDocument -> value -> IO ()
setPrintInfo nsDocument value =
  sendMessage nsDocument setPrintInfoSelector (toNSPrintInfo value)

-- | @- PDFPrintOperation@
pdfPrintOperation :: IsNSDocument nsDocument => nsDocument -> IO (Id NSPrintOperation)
pdfPrintOperation nsDocument =
  sendMessage nsDocument pdfPrintOperationSelector

-- | @- allowsDocumentSharing@
allowsDocumentSharing :: IsNSDocument nsDocument => nsDocument -> IO Bool
allowsDocumentSharing nsDocument =
  sendMessage nsDocument allowsDocumentSharingSelector

-- | @- previewRepresentableActivityItems@
previewRepresentableActivityItems :: IsNSDocument nsDocument => nsDocument -> IO (Id NSArray)
previewRepresentableActivityItems nsDocument =
  sendMessage nsDocument previewRepresentableActivityItemsSelector

-- | @- setPreviewRepresentableActivityItems:@
setPreviewRepresentableActivityItems :: (IsNSDocument nsDocument, IsNSArray value) => nsDocument -> value -> IO ()
setPreviewRepresentableActivityItems nsDocument value =
  sendMessage nsDocument setPreviewRepresentableActivityItemsSelector (toNSArray value)

-- | @- documentEdited@
documentEdited :: IsNSDocument nsDocument => nsDocument -> IO Bool
documentEdited nsDocument =
  sendMessage nsDocument documentEditedSelector

-- | @- inViewingMode@
inViewingMode :: IsNSDocument nsDocument => nsDocument -> IO Bool
inViewingMode nsDocument =
  sendMessage nsDocument inViewingModeSelector

-- | @- undoManager@
undoManager :: IsNSDocument nsDocument => nsDocument -> IO (Id NSUndoManager)
undoManager nsDocument =
  sendMessage nsDocument undoManagerSelector

-- | @- setUndoManager:@
setUndoManager :: (IsNSDocument nsDocument, IsNSUndoManager value) => nsDocument -> value -> IO ()
setUndoManager nsDocument value =
  sendMessage nsDocument setUndoManagerSelector (toNSUndoManager value)

-- | @- hasUndoManager@
hasUndoManager :: IsNSDocument nsDocument => nsDocument -> IO Bool
hasUndoManager nsDocument =
  sendMessage nsDocument hasUndoManagerSelector

-- | @- setHasUndoManager:@
setHasUndoManager :: IsNSDocument nsDocument => nsDocument -> Bool -> IO ()
setHasUndoManager nsDocument value =
  sendMessage nsDocument setHasUndoManagerSelector value

-- | @- windowNibName@
windowNibName :: IsNSDocument nsDocument => nsDocument -> IO (Id NSString)
windowNibName nsDocument =
  sendMessage nsDocument windowNibNameSelector

-- | @- windowControllers@
windowControllers :: IsNSDocument nsDocument => nsDocument -> IO (Id NSArray)
windowControllers nsDocument =
  sendMessage nsDocument windowControllersSelector

-- | @- displayName@
displayName :: IsNSDocument nsDocument => nsDocument -> IO (Id NSString)
displayName nsDocument =
  sendMessage nsDocument displayNameSelector

-- | @- windowForSheet@
windowForSheet :: IsNSDocument nsDocument => nsDocument -> IO (Id NSWindow)
windowForSheet nsDocument =
  sendMessage nsDocument windowForSheetSelector

-- | @+ readableTypes@
readableTypes :: IO (Id NSArray)
readableTypes  =
  do
    cls' <- getRequiredClass "NSDocument"
    sendClassMessage cls' readableTypesSelector

-- | @+ writableTypes@
writableTypes :: IO (Id NSArray)
writableTypes  =
  do
    cls' <- getRequiredClass "NSDocument"
    sendClassMessage cls' writableTypesSelector

-- | @+ usesUbiquitousStorage@
usesUbiquitousStorage :: IO Bool
usesUbiquitousStorage  =
  do
    cls' <- getRequiredClass "NSDocument"
    sendClassMessage cls' usesUbiquitousStorageSelector

-- | @- presentedItemURL@
presentedItemURL :: IsNSDocument nsDocument => nsDocument -> IO (Id NSURL)
presentedItemURL nsDocument =
  sendMessage nsDocument presentedItemURLSelector

-- | @- observedPresentedItemUbiquityAttributes@
observedPresentedItemUbiquityAttributes :: IsNSDocument nsDocument => nsDocument -> IO (Id NSSet)
observedPresentedItemUbiquityAttributes nsDocument =
  sendMessage nsDocument observedPresentedItemUbiquityAttributesSelector

-- | @+ restorableStateKeyPaths@
restorableStateKeyPaths :: IO (Id NSArray)
restorableStateKeyPaths  =
  do
    cls' <- getRequiredClass "NSDocument"
    sendClassMessage cls' restorableStateKeyPathsSelector

-- | @- lastComponentOfFileName@
lastComponentOfFileName :: IsNSDocument nsDocument => nsDocument -> IO (Id NSString)
lastComponentOfFileName nsDocument =
  sendMessage nsDocument lastComponentOfFileNameSelector

-- | @- setLastComponentOfFileName:@
setLastComponentOfFileName :: (IsNSDocument nsDocument, IsNSString value) => nsDocument -> value -> IO ()
setLastComponentOfFileName nsDocument value =
  sendMessage nsDocument setLastComponentOfFileNameSelector (toNSString value)

-- | @- objectSpecifier@
objectSpecifier :: IsNSDocument nsDocument => nsDocument -> IO (Id NSScriptObjectSpecifier)
objectSpecifier nsDocument =
  sendMessage nsDocument objectSpecifierSelector

-- | @- userActivity@
userActivity :: IsNSDocument nsDocument => nsDocument -> IO (Id NSUserActivity)
userActivity nsDocument =
  sendMessage nsDocument userActivitySelector

-- | @- setUserActivity:@
setUserActivity :: (IsNSDocument nsDocument, IsNSUserActivity value) => nsDocument -> value -> IO ()
setUserActivity nsDocument value =
  sendMessage nsDocument setUserActivitySelector (toNSUserActivity value)

-- | @- shouldRunSavePanelWithAccessoryView@
shouldRunSavePanelWithAccessoryView :: IsNSDocument nsDocument => nsDocument -> IO Bool
shouldRunSavePanelWithAccessoryView nsDocument =
  sendMessage nsDocument shouldRunSavePanelWithAccessoryViewSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSDocument)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithType:error:@
initWithType_errorSelector :: Selector '[Id NSString, Id NSError] (Id NSDocument)
initWithType_errorSelector = mkSelector "initWithType:error:"

-- | @Selector@ for @canConcurrentlyReadDocumentsOfType:@
canConcurrentlyReadDocumentsOfTypeSelector :: Selector '[Id NSString] Bool
canConcurrentlyReadDocumentsOfTypeSelector = mkSelector "canConcurrentlyReadDocumentsOfType:"

-- | @Selector@ for @initWithContentsOfURL:ofType:error:@
initWithContentsOfURL_ofType_errorSelector :: Selector '[Id NSURL, Id NSString, Id NSError] (Id NSDocument)
initWithContentsOfURL_ofType_errorSelector = mkSelector "initWithContentsOfURL:ofType:error:"

-- | @Selector@ for @initForURL:withContentsOfURL:ofType:error:@
initForURL_withContentsOfURL_ofType_errorSelector :: Selector '[Id NSURL, Id NSURL, Id NSString, Id NSError] (Id NSDocument)
initForURL_withContentsOfURL_ofType_errorSelector = mkSelector "initForURL:withContentsOfURL:ofType:error:"

-- | @Selector@ for @performActivityWithSynchronousWaiting:usingBlock:@
performActivityWithSynchronousWaiting_usingBlockSelector :: Selector '[Bool, Ptr ()] ()
performActivityWithSynchronousWaiting_usingBlockSelector = mkSelector "performActivityWithSynchronousWaiting:usingBlock:"

-- | @Selector@ for @continueActivityUsingBlock:@
continueActivityUsingBlockSelector :: Selector '[Ptr ()] ()
continueActivityUsingBlockSelector = mkSelector "continueActivityUsingBlock:"

-- | @Selector@ for @continueAsynchronousWorkOnMainThreadUsingBlock:@
continueAsynchronousWorkOnMainThreadUsingBlockSelector :: Selector '[Ptr ()] ()
continueAsynchronousWorkOnMainThreadUsingBlockSelector = mkSelector "continueAsynchronousWorkOnMainThreadUsingBlock:"

-- | @Selector@ for @performSynchronousFileAccessUsingBlock:@
performSynchronousFileAccessUsingBlockSelector :: Selector '[Ptr ()] ()
performSynchronousFileAccessUsingBlockSelector = mkSelector "performSynchronousFileAccessUsingBlock:"

-- | @Selector@ for @performAsynchronousFileAccessUsingBlock:@
performAsynchronousFileAccessUsingBlockSelector :: Selector '[Ptr ()] ()
performAsynchronousFileAccessUsingBlockSelector = mkSelector "performAsynchronousFileAccessUsingBlock:"

-- | @Selector@ for @revertDocumentToSaved:@
revertDocumentToSavedSelector :: Selector '[RawId] ()
revertDocumentToSavedSelector = mkSelector "revertDocumentToSaved:"

-- | @Selector@ for @revertToContentsOfURL:ofType:error:@
revertToContentsOfURL_ofType_errorSelector :: Selector '[Id NSURL, Id NSString, Id NSError] Bool
revertToContentsOfURL_ofType_errorSelector = mkSelector "revertToContentsOfURL:ofType:error:"

-- | @Selector@ for @readFromURL:ofType:error:@
readFromURL_ofType_errorSelector :: Selector '[Id NSURL, Id NSString, Id NSError] Bool
readFromURL_ofType_errorSelector = mkSelector "readFromURL:ofType:error:"

-- | @Selector@ for @readFromFileWrapper:ofType:error:@
readFromFileWrapper_ofType_errorSelector :: Selector '[Id NSFileWrapper, Id NSString, Id NSError] Bool
readFromFileWrapper_ofType_errorSelector = mkSelector "readFromFileWrapper:ofType:error:"

-- | @Selector@ for @readFromData:ofType:error:@
readFromData_ofType_errorSelector :: Selector '[Id NSData, Id NSString, Id NSError] Bool
readFromData_ofType_errorSelector = mkSelector "readFromData:ofType:error:"

-- | @Selector@ for @writeToURL:ofType:error:@
writeToURL_ofType_errorSelector :: Selector '[Id NSURL, Id NSString, Id NSError] Bool
writeToURL_ofType_errorSelector = mkSelector "writeToURL:ofType:error:"

-- | @Selector@ for @fileWrapperOfType:error:@
fileWrapperOfType_errorSelector :: Selector '[Id NSString, Id NSError] (Id NSFileWrapper)
fileWrapperOfType_errorSelector = mkSelector "fileWrapperOfType:error:"

-- | @Selector@ for @dataOfType:error:@
dataOfType_errorSelector :: Selector '[Id NSString, Id NSError] (Id NSData)
dataOfType_errorSelector = mkSelector "dataOfType:error:"

-- | @Selector@ for @unblockUserInteraction@
unblockUserInteractionSelector :: Selector '[] ()
unblockUserInteractionSelector = mkSelector "unblockUserInteraction"

-- | @Selector@ for @writeSafelyToURL:ofType:forSaveOperation:error:@
writeSafelyToURL_ofType_forSaveOperation_errorSelector :: Selector '[Id NSURL, Id NSString, NSSaveOperationType, Id NSError] Bool
writeSafelyToURL_ofType_forSaveOperation_errorSelector = mkSelector "writeSafelyToURL:ofType:forSaveOperation:error:"

-- | @Selector@ for @writeToURL:ofType:forSaveOperation:originalContentsURL:error:@
writeToURL_ofType_forSaveOperation_originalContentsURL_errorSelector :: Selector '[Id NSURL, Id NSString, NSSaveOperationType, Id NSURL, Id NSError] Bool
writeToURL_ofType_forSaveOperation_originalContentsURL_errorSelector = mkSelector "writeToURL:ofType:forSaveOperation:originalContentsURL:error:"

-- | @Selector@ for @fileAttributesToWriteToURL:ofType:forSaveOperation:originalContentsURL:error:@
fileAttributesToWriteToURL_ofType_forSaveOperation_originalContentsURL_errorSelector :: Selector '[Id NSURL, Id NSString, NSSaveOperationType, Id NSURL, Id NSError] (Id NSDictionary)
fileAttributesToWriteToURL_ofType_forSaveOperation_originalContentsURL_errorSelector = mkSelector "fileAttributesToWriteToURL:ofType:forSaveOperation:originalContentsURL:error:"

-- | @Selector@ for @saveDocument:@
saveDocumentSelector :: Selector '[RawId] ()
saveDocumentSelector = mkSelector "saveDocument:"

-- | @Selector@ for @saveDocumentAs:@
saveDocumentAsSelector :: Selector '[RawId] ()
saveDocumentAsSelector = mkSelector "saveDocumentAs:"

-- | @Selector@ for @saveDocumentTo:@
saveDocumentToSelector :: Selector '[RawId] ()
saveDocumentToSelector = mkSelector "saveDocumentTo:"

-- | @Selector@ for @saveDocumentWithDelegate:didSaveSelector:contextInfo:@
saveDocumentWithDelegate_didSaveSelector_contextInfoSelector :: Selector '[RawId, Sel, Ptr ()] ()
saveDocumentWithDelegate_didSaveSelector_contextInfoSelector = mkSelector "saveDocumentWithDelegate:didSaveSelector:contextInfo:"

-- | @Selector@ for @runModalSavePanelForSaveOperation:delegate:didSaveSelector:contextInfo:@
runModalSavePanelForSaveOperation_delegate_didSaveSelector_contextInfoSelector :: Selector '[NSSaveOperationType, RawId, Sel, Ptr ()] ()
runModalSavePanelForSaveOperation_delegate_didSaveSelector_contextInfoSelector = mkSelector "runModalSavePanelForSaveOperation:delegate:didSaveSelector:contextInfo:"

-- | @Selector@ for @prepareSavePanel:@
prepareSavePanelSelector :: Selector '[Id NSSavePanel] Bool
prepareSavePanelSelector = mkSelector "prepareSavePanel:"

-- | @Selector@ for @saveToURL:ofType:forSaveOperation:delegate:didSaveSelector:contextInfo:@
saveToURL_ofType_forSaveOperation_delegate_didSaveSelector_contextInfoSelector :: Selector '[Id NSURL, Id NSString, NSSaveOperationType, RawId, Sel, Ptr ()] ()
saveToURL_ofType_forSaveOperation_delegate_didSaveSelector_contextInfoSelector = mkSelector "saveToURL:ofType:forSaveOperation:delegate:didSaveSelector:contextInfo:"

-- | @Selector@ for @saveToURL:ofType:forSaveOperation:completionHandler:@
saveToURL_ofType_forSaveOperation_completionHandlerSelector :: Selector '[Id NSURL, Id NSString, NSSaveOperationType, Ptr ()] ()
saveToURL_ofType_forSaveOperation_completionHandlerSelector = mkSelector "saveToURL:ofType:forSaveOperation:completionHandler:"

-- | @Selector@ for @canAsynchronouslyWriteToURL:ofType:forSaveOperation:@
canAsynchronouslyWriteToURL_ofType_forSaveOperationSelector :: Selector '[Id NSURL, Id NSString, NSSaveOperationType] Bool
canAsynchronouslyWriteToURL_ofType_forSaveOperationSelector = mkSelector "canAsynchronouslyWriteToURL:ofType:forSaveOperation:"

-- | @Selector@ for @checkAutosavingSafetyAndReturnError:@
checkAutosavingSafetyAndReturnErrorSelector :: Selector '[Id NSError] Bool
checkAutosavingSafetyAndReturnErrorSelector = mkSelector "checkAutosavingSafetyAndReturnError:"

-- | @Selector@ for @scheduleAutosaving@
scheduleAutosavingSelector :: Selector '[] ()
scheduleAutosavingSelector = mkSelector "scheduleAutosaving"

-- | @Selector@ for @autosaveDocumentWithDelegate:didAutosaveSelector:contextInfo:@
autosaveDocumentWithDelegate_didAutosaveSelector_contextInfoSelector :: Selector '[RawId, Sel, Ptr ()] ()
autosaveDocumentWithDelegate_didAutosaveSelector_contextInfoSelector = mkSelector "autosaveDocumentWithDelegate:didAutosaveSelector:contextInfo:"

-- | @Selector@ for @autosaveWithImplicitCancellability:completionHandler:@
autosaveWithImplicitCancellability_completionHandlerSelector :: Selector '[Bool, Ptr ()] ()
autosaveWithImplicitCancellability_completionHandlerSelector = mkSelector "autosaveWithImplicitCancellability:completionHandler:"

-- | @Selector@ for @browseDocumentVersions:@
browseDocumentVersionsSelector :: Selector '[RawId] ()
browseDocumentVersionsSelector = mkSelector "browseDocumentVersions:"

-- | @Selector@ for @stopBrowsingVersionsWithCompletionHandler:@
stopBrowsingVersionsWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
stopBrowsingVersionsWithCompletionHandlerSelector = mkSelector "stopBrowsingVersionsWithCompletionHandler:"

-- | @Selector@ for @canCloseDocumentWithDelegate:shouldCloseSelector:contextInfo:@
canCloseDocumentWithDelegate_shouldCloseSelector_contextInfoSelector :: Selector '[RawId, Sel, Ptr ()] ()
canCloseDocumentWithDelegate_shouldCloseSelector_contextInfoSelector = mkSelector "canCloseDocumentWithDelegate:shouldCloseSelector:contextInfo:"

-- | @Selector@ for @close@
closeSelector :: Selector '[] ()
closeSelector = mkSelector "close"

-- | @Selector@ for @duplicateDocument:@
duplicateDocumentSelector :: Selector '[RawId] ()
duplicateDocumentSelector = mkSelector "duplicateDocument:"

-- | @Selector@ for @duplicateDocumentWithDelegate:didDuplicateSelector:contextInfo:@
duplicateDocumentWithDelegate_didDuplicateSelector_contextInfoSelector :: Selector '[RawId, Sel, Ptr ()] ()
duplicateDocumentWithDelegate_didDuplicateSelector_contextInfoSelector = mkSelector "duplicateDocumentWithDelegate:didDuplicateSelector:contextInfo:"

-- | @Selector@ for @duplicateAndReturnError:@
duplicateAndReturnErrorSelector :: Selector '[Id NSError] (Id NSDocument)
duplicateAndReturnErrorSelector = mkSelector "duplicateAndReturnError:"

-- | @Selector@ for @renameDocument:@
renameDocumentSelector :: Selector '[RawId] ()
renameDocumentSelector = mkSelector "renameDocument:"

-- | @Selector@ for @moveDocumentToUbiquityContainer:@
moveDocumentToUbiquityContainerSelector :: Selector '[RawId] ()
moveDocumentToUbiquityContainerSelector = mkSelector "moveDocumentToUbiquityContainer:"

-- | @Selector@ for @moveDocument:@
moveDocumentSelector :: Selector '[RawId] ()
moveDocumentSelector = mkSelector "moveDocument:"

-- | @Selector@ for @moveDocumentWithCompletionHandler:@
moveDocumentWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
moveDocumentWithCompletionHandlerSelector = mkSelector "moveDocumentWithCompletionHandler:"

-- | @Selector@ for @moveToURL:completionHandler:@
moveToURL_completionHandlerSelector :: Selector '[Id NSURL, Ptr ()] ()
moveToURL_completionHandlerSelector = mkSelector "moveToURL:completionHandler:"

-- | @Selector@ for @lockDocument:@
lockDocumentSelector :: Selector '[RawId] ()
lockDocumentSelector = mkSelector "lockDocument:"

-- | @Selector@ for @unlockDocument:@
unlockDocumentSelector :: Selector '[RawId] ()
unlockDocumentSelector = mkSelector "unlockDocument:"

-- | @Selector@ for @lockDocumentWithCompletionHandler:@
lockDocumentWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
lockDocumentWithCompletionHandlerSelector = mkSelector "lockDocumentWithCompletionHandler:"

-- | @Selector@ for @lockWithCompletionHandler:@
lockWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
lockWithCompletionHandlerSelector = mkSelector "lockWithCompletionHandler:"

-- | @Selector@ for @unlockDocumentWithCompletionHandler:@
unlockDocumentWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
unlockDocumentWithCompletionHandlerSelector = mkSelector "unlockDocumentWithCompletionHandler:"

-- | @Selector@ for @unlockWithCompletionHandler:@
unlockWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
unlockWithCompletionHandlerSelector = mkSelector "unlockWithCompletionHandler:"

-- | @Selector@ for @runPageLayout:@
runPageLayoutSelector :: Selector '[RawId] ()
runPageLayoutSelector = mkSelector "runPageLayout:"

-- | @Selector@ for @runModalPageLayoutWithPrintInfo:delegate:didRunSelector:contextInfo:@
runModalPageLayoutWithPrintInfo_delegate_didRunSelector_contextInfoSelector :: Selector '[Id NSPrintInfo, RawId, Sel, Ptr ()] ()
runModalPageLayoutWithPrintInfo_delegate_didRunSelector_contextInfoSelector = mkSelector "runModalPageLayoutWithPrintInfo:delegate:didRunSelector:contextInfo:"

-- | @Selector@ for @preparePageLayout:@
preparePageLayoutSelector :: Selector '[Id NSPageLayout] Bool
preparePageLayoutSelector = mkSelector "preparePageLayout:"

-- | @Selector@ for @shouldChangePrintInfo:@
shouldChangePrintInfoSelector :: Selector '[Id NSPrintInfo] Bool
shouldChangePrintInfoSelector = mkSelector "shouldChangePrintInfo:"

-- | @Selector@ for @printDocument:@
printDocumentSelector :: Selector '[RawId] ()
printDocumentSelector = mkSelector "printDocument:"

-- | @Selector@ for @printDocumentWithSettings:showPrintPanel:delegate:didPrintSelector:contextInfo:@
printDocumentWithSettings_showPrintPanel_delegate_didPrintSelector_contextInfoSelector :: Selector '[Id NSDictionary, Bool, RawId, Sel, Ptr ()] ()
printDocumentWithSettings_showPrintPanel_delegate_didPrintSelector_contextInfoSelector = mkSelector "printDocumentWithSettings:showPrintPanel:delegate:didPrintSelector:contextInfo:"

-- | @Selector@ for @printOperationWithSettings:error:@
printOperationWithSettings_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id NSPrintOperation)
printOperationWithSettings_errorSelector = mkSelector "printOperationWithSettings:error:"

-- | @Selector@ for @runModalPrintOperation:delegate:didRunSelector:contextInfo:@
runModalPrintOperation_delegate_didRunSelector_contextInfoSelector :: Selector '[Id NSPrintOperation, RawId, Sel, Ptr ()] ()
runModalPrintOperation_delegate_didRunSelector_contextInfoSelector = mkSelector "runModalPrintOperation:delegate:didRunSelector:contextInfo:"

-- | @Selector@ for @saveDocumentToPDF:@
saveDocumentToPDFSelector :: Selector '[RawId] ()
saveDocumentToPDFSelector = mkSelector "saveDocumentToPDF:"

-- | @Selector@ for @shareDocumentWithSharingService:completionHandler:@
shareDocumentWithSharingService_completionHandlerSelector :: Selector '[Id NSSharingService, Ptr ()] ()
shareDocumentWithSharingService_completionHandlerSelector = mkSelector "shareDocumentWithSharingService:completionHandler:"

-- | @Selector@ for @prepareSharingServicePicker:@
prepareSharingServicePickerSelector :: Selector '[Id NSSharingServicePicker] ()
prepareSharingServicePickerSelector = mkSelector "prepareSharingServicePicker:"

-- | @Selector@ for @updateChangeCount:@
updateChangeCountSelector :: Selector '[NSDocumentChangeType] ()
updateChangeCountSelector = mkSelector "updateChangeCount:"

-- | @Selector@ for @changeCountTokenForSaveOperation:@
changeCountTokenForSaveOperationSelector :: Selector '[NSSaveOperationType] RawId
changeCountTokenForSaveOperationSelector = mkSelector "changeCountTokenForSaveOperation:"

-- | @Selector@ for @updateChangeCountWithToken:forSaveOperation:@
updateChangeCountWithToken_forSaveOperationSelector :: Selector '[RawId, NSSaveOperationType] ()
updateChangeCountWithToken_forSaveOperationSelector = mkSelector "updateChangeCountWithToken:forSaveOperation:"

-- | @Selector@ for @presentError:modalForWindow:delegate:didPresentSelector:contextInfo:@
presentError_modalForWindow_delegate_didPresentSelector_contextInfoSelector :: Selector '[Id NSError, Id NSWindow, RawId, Sel, Ptr ()] ()
presentError_modalForWindow_delegate_didPresentSelector_contextInfoSelector = mkSelector "presentError:modalForWindow:delegate:didPresentSelector:contextInfo:"

-- | @Selector@ for @presentError:@
presentErrorSelector :: Selector '[Id NSError] Bool
presentErrorSelector = mkSelector "presentError:"

-- | @Selector@ for @willPresentError:@
willPresentErrorSelector :: Selector '[Id NSError] (Id NSError)
willPresentErrorSelector = mkSelector "willPresentError:"

-- | @Selector@ for @willNotPresentError:@
willNotPresentErrorSelector :: Selector '[Id NSError] ()
willNotPresentErrorSelector = mkSelector "willNotPresentError:"

-- | @Selector@ for @makeWindowControllers@
makeWindowControllersSelector :: Selector '[] ()
makeWindowControllersSelector = mkSelector "makeWindowControllers"

-- | @Selector@ for @windowControllerWillLoadNib:@
windowControllerWillLoadNibSelector :: Selector '[Id NSWindowController] ()
windowControllerWillLoadNibSelector = mkSelector "windowControllerWillLoadNib:"

-- | @Selector@ for @windowControllerDidLoadNib:@
windowControllerDidLoadNibSelector :: Selector '[Id NSWindowController] ()
windowControllerDidLoadNibSelector = mkSelector "windowControllerDidLoadNib:"

-- | @Selector@ for @setWindow:@
setWindowSelector :: Selector '[Id NSWindow] ()
setWindowSelector = mkSelector "setWindow:"

-- | @Selector@ for @addWindowController:@
addWindowControllerSelector :: Selector '[Id NSWindowController] ()
addWindowControllerSelector = mkSelector "addWindowController:"

-- | @Selector@ for @removeWindowController:@
removeWindowControllerSelector :: Selector '[Id NSWindowController] ()
removeWindowControllerSelector = mkSelector "removeWindowController:"

-- | @Selector@ for @showWindows@
showWindowsSelector :: Selector '[] ()
showWindowsSelector = mkSelector "showWindows"

-- | @Selector@ for @shouldCloseWindowController:delegate:shouldCloseSelector:contextInfo:@
shouldCloseWindowController_delegate_shouldCloseSelector_contextInfoSelector :: Selector '[Id NSWindowController, RawId, Sel, Ptr ()] ()
shouldCloseWindowController_delegate_shouldCloseSelector_contextInfoSelector = mkSelector "shouldCloseWindowController:delegate:shouldCloseSelector:contextInfo:"

-- | @Selector@ for @setDisplayName:@
setDisplayNameSelector :: Selector '[Id NSString] ()
setDisplayNameSelector = mkSelector "setDisplayName:"

-- | @Selector@ for @defaultDraftName@
defaultDraftNameSelector :: Selector '[] (Id NSString)
defaultDraftNameSelector = mkSelector "defaultDraftName"

-- | @Selector@ for @isNativeType:@
isNativeTypeSelector :: Selector '[Id NSString] Bool
isNativeTypeSelector = mkSelector "isNativeType:"

-- | @Selector@ for @writableTypesForSaveOperation:@
writableTypesForSaveOperationSelector :: Selector '[NSSaveOperationType] (Id NSArray)
writableTypesForSaveOperationSelector = mkSelector "writableTypesForSaveOperation:"

-- | @Selector@ for @fileNameExtensionForType:saveOperation:@
fileNameExtensionForType_saveOperationSelector :: Selector '[Id NSString, NSSaveOperationType] (Id NSString)
fileNameExtensionForType_saveOperationSelector = mkSelector "fileNameExtensionForType:saveOperation:"

-- | @Selector@ for @validateUserInterfaceItem:@
validateUserInterfaceItemSelector :: Selector '[RawId] Bool
validateUserInterfaceItemSelector = mkSelector "validateUserInterfaceItem:"

-- | @Selector@ for @relinquishPresentedItemToReader:@
relinquishPresentedItemToReaderSelector :: Selector '[Ptr ()] ()
relinquishPresentedItemToReaderSelector = mkSelector "relinquishPresentedItemToReader:"

-- | @Selector@ for @relinquishPresentedItemToWriter:@
relinquishPresentedItemToWriterSelector :: Selector '[Ptr ()] ()
relinquishPresentedItemToWriterSelector = mkSelector "relinquishPresentedItemToWriter:"

-- | @Selector@ for @savePresentedItemChangesWithCompletionHandler:@
savePresentedItemChangesWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
savePresentedItemChangesWithCompletionHandlerSelector = mkSelector "savePresentedItemChangesWithCompletionHandler:"

-- | @Selector@ for @accommodatePresentedItemDeletionWithCompletionHandler:@
accommodatePresentedItemDeletionWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
accommodatePresentedItemDeletionWithCompletionHandlerSelector = mkSelector "accommodatePresentedItemDeletionWithCompletionHandler:"

-- | @Selector@ for @presentedItemDidMoveToURL:@
presentedItemDidMoveToURLSelector :: Selector '[Id NSURL] ()
presentedItemDidMoveToURLSelector = mkSelector "presentedItemDidMoveToURL:"

-- | @Selector@ for @presentedItemDidChange@
presentedItemDidChangeSelector :: Selector '[] ()
presentedItemDidChangeSelector = mkSelector "presentedItemDidChange"

-- | @Selector@ for @presentedItemDidChangeUbiquityAttributes:@
presentedItemDidChangeUbiquityAttributesSelector :: Selector '[Id NSSet] ()
presentedItemDidChangeUbiquityAttributesSelector = mkSelector "presentedItemDidChangeUbiquityAttributes:"

-- | @Selector@ for @presentedItemDidGainVersion:@
presentedItemDidGainVersionSelector :: Selector '[Id NSFileVersion] ()
presentedItemDidGainVersionSelector = mkSelector "presentedItemDidGainVersion:"

-- | @Selector@ for @presentedItemDidLoseVersion:@
presentedItemDidLoseVersionSelector :: Selector '[Id NSFileVersion] ()
presentedItemDidLoseVersionSelector = mkSelector "presentedItemDidLoseVersion:"

-- | @Selector@ for @presentedItemDidResolveConflictVersion:@
presentedItemDidResolveConflictVersionSelector :: Selector '[Id NSFileVersion] ()
presentedItemDidResolveConflictVersionSelector = mkSelector "presentedItemDidResolveConflictVersion:"

-- | @Selector@ for @restoreDocumentWindowWithIdentifier:state:completionHandler:@
restoreDocumentWindowWithIdentifier_state_completionHandlerSelector :: Selector '[Id NSString, Id NSCoder, Ptr ()] ()
restoreDocumentWindowWithIdentifier_state_completionHandlerSelector = mkSelector "restoreDocumentWindowWithIdentifier:state:completionHandler:"

-- | @Selector@ for @encodeRestorableStateWithCoder:@
encodeRestorableStateWithCoderSelector :: Selector '[Id NSCoder] ()
encodeRestorableStateWithCoderSelector = mkSelector "encodeRestorableStateWithCoder:"

-- | @Selector@ for @encodeRestorableStateWithCoder:backgroundQueue:@
encodeRestorableStateWithCoder_backgroundQueueSelector :: Selector '[Id NSCoder, Id NSOperationQueue] ()
encodeRestorableStateWithCoder_backgroundQueueSelector = mkSelector "encodeRestorableStateWithCoder:backgroundQueue:"

-- | @Selector@ for @restoreStateWithCoder:@
restoreStateWithCoderSelector :: Selector '[Id NSCoder] ()
restoreStateWithCoderSelector = mkSelector "restoreStateWithCoder:"

-- | @Selector@ for @invalidateRestorableState@
invalidateRestorableStateSelector :: Selector '[] ()
invalidateRestorableStateSelector = mkSelector "invalidateRestorableState"

-- | @Selector@ for @allowedClassesForRestorableStateKeyPath:@
allowedClassesForRestorableStateKeyPathSelector :: Selector '[Id NSString] (Id NSArray)
allowedClassesForRestorableStateKeyPathSelector = mkSelector "allowedClassesForRestorableStateKeyPath:"

-- | @Selector@ for @handleSaveScriptCommand:@
handleSaveScriptCommandSelector :: Selector '[Id NSScriptCommand] RawId
handleSaveScriptCommandSelector = mkSelector "handleSaveScriptCommand:"

-- | @Selector@ for @handleCloseScriptCommand:@
handleCloseScriptCommandSelector :: Selector '[Id NSCloseCommand] RawId
handleCloseScriptCommandSelector = mkSelector "handleCloseScriptCommand:"

-- | @Selector@ for @handlePrintScriptCommand:@
handlePrintScriptCommandSelector :: Selector '[Id NSScriptCommand] RawId
handlePrintScriptCommandSelector = mkSelector "handlePrintScriptCommand:"

-- | @Selector@ for @updateUserActivityState:@
updateUserActivityStateSelector :: Selector '[Id NSUserActivity] ()
updateUserActivityStateSelector = mkSelector "updateUserActivityState:"

-- | @Selector@ for @saveToURL:ofType:forSaveOperation:error:@
saveToURL_ofType_forSaveOperation_errorSelector :: Selector '[Id NSURL, Id NSString, NSSaveOperationType, Id NSError] Bool
saveToURL_ofType_forSaveOperation_errorSelector = mkSelector "saveToURL:ofType:forSaveOperation:error:"

-- | @Selector@ for @dataRepresentationOfType:@
dataRepresentationOfTypeSelector :: Selector '[Id NSString] (Id NSData)
dataRepresentationOfTypeSelector = mkSelector "dataRepresentationOfType:"

-- | @Selector@ for @fileAttributesToWriteToFile:ofType:saveOperation:@
fileAttributesToWriteToFile_ofType_saveOperationSelector :: Selector '[Id NSString, Id NSString, NSSaveOperationType] (Id NSDictionary)
fileAttributesToWriteToFile_ofType_saveOperationSelector = mkSelector "fileAttributesToWriteToFile:ofType:saveOperation:"

-- | @Selector@ for @fileName@
fileNameSelector :: Selector '[] (Id NSString)
fileNameSelector = mkSelector "fileName"

-- | @Selector@ for @fileWrapperRepresentationOfType:@
fileWrapperRepresentationOfTypeSelector :: Selector '[Id NSString] (Id NSFileWrapper)
fileWrapperRepresentationOfTypeSelector = mkSelector "fileWrapperRepresentationOfType:"

-- | @Selector@ for @initWithContentsOfFile:ofType:@
initWithContentsOfFile_ofTypeSelector :: Selector '[Id NSString, Id NSString] RawId
initWithContentsOfFile_ofTypeSelector = mkSelector "initWithContentsOfFile:ofType:"

-- | @Selector@ for @initWithContentsOfURL:ofType:@
initWithContentsOfURL_ofTypeSelector :: Selector '[Id NSURL, Id NSString] RawId
initWithContentsOfURL_ofTypeSelector = mkSelector "initWithContentsOfURL:ofType:"

-- | @Selector@ for @loadDataRepresentation:ofType:@
loadDataRepresentation_ofTypeSelector :: Selector '[Id NSData, Id NSString] Bool
loadDataRepresentation_ofTypeSelector = mkSelector "loadDataRepresentation:ofType:"

-- | @Selector@ for @loadFileWrapperRepresentation:ofType:@
loadFileWrapperRepresentation_ofTypeSelector :: Selector '[Id NSFileWrapper, Id NSString] Bool
loadFileWrapperRepresentation_ofTypeSelector = mkSelector "loadFileWrapperRepresentation:ofType:"

-- | @Selector@ for @printShowingPrintPanel:@
printShowingPrintPanelSelector :: Selector '[Bool] ()
printShowingPrintPanelSelector = mkSelector "printShowingPrintPanel:"

-- | @Selector@ for @readFromFile:ofType:@
readFromFile_ofTypeSelector :: Selector '[Id NSString, Id NSString] Bool
readFromFile_ofTypeSelector = mkSelector "readFromFile:ofType:"

-- | @Selector@ for @readFromURL:ofType:@
readFromURL_ofTypeSelector :: Selector '[Id NSURL, Id NSString] Bool
readFromURL_ofTypeSelector = mkSelector "readFromURL:ofType:"

-- | @Selector@ for @revertToSavedFromFile:ofType:@
revertToSavedFromFile_ofTypeSelector :: Selector '[Id NSString, Id NSString] Bool
revertToSavedFromFile_ofTypeSelector = mkSelector "revertToSavedFromFile:ofType:"

-- | @Selector@ for @revertToSavedFromURL:ofType:@
revertToSavedFromURL_ofTypeSelector :: Selector '[Id NSURL, Id NSString] Bool
revertToSavedFromURL_ofTypeSelector = mkSelector "revertToSavedFromURL:ofType:"

-- | @Selector@ for @runModalPageLayoutWithPrintInfo:@
runModalPageLayoutWithPrintInfoSelector :: Selector '[Id NSPrintInfo] CLong
runModalPageLayoutWithPrintInfoSelector = mkSelector "runModalPageLayoutWithPrintInfo:"

-- | @Selector@ for @saveToFile:saveOperation:delegate:didSaveSelector:contextInfo:@
saveToFile_saveOperation_delegate_didSaveSelector_contextInfoSelector :: Selector '[Id NSString, NSSaveOperationType, RawId, Sel, Ptr ()] ()
saveToFile_saveOperation_delegate_didSaveSelector_contextInfoSelector = mkSelector "saveToFile:saveOperation:delegate:didSaveSelector:contextInfo:"

-- | @Selector@ for @setFileName:@
setFileNameSelector :: Selector '[Id NSString] ()
setFileNameSelector = mkSelector "setFileName:"

-- | @Selector@ for @writeToFile:ofType:@
writeToFile_ofTypeSelector :: Selector '[Id NSString, Id NSString] Bool
writeToFile_ofTypeSelector = mkSelector "writeToFile:ofType:"

-- | @Selector@ for @writeToFile:ofType:originalFile:saveOperation:@
writeToFile_ofType_originalFile_saveOperationSelector :: Selector '[Id NSString, Id NSString, Id NSString, NSSaveOperationType] Bool
writeToFile_ofType_originalFile_saveOperationSelector = mkSelector "writeToFile:ofType:originalFile:saveOperation:"

-- | @Selector@ for @writeToURL:ofType:@
writeToURL_ofTypeSelector :: Selector '[Id NSURL, Id NSString] Bool
writeToURL_ofTypeSelector = mkSelector "writeToURL:ofType:"

-- | @Selector@ for @writeWithBackupToFile:ofType:saveOperation:@
writeWithBackupToFile_ofType_saveOperationSelector :: Selector '[Id NSString, Id NSString, NSSaveOperationType] Bool
writeWithBackupToFile_ofType_saveOperationSelector = mkSelector "writeWithBackupToFile:ofType:saveOperation:"

-- | @Selector@ for @fileType@
fileTypeSelector :: Selector '[] (Id NSString)
fileTypeSelector = mkSelector "fileType"

-- | @Selector@ for @setFileType:@
setFileTypeSelector :: Selector '[Id NSString] ()
setFileTypeSelector = mkSelector "setFileType:"

-- | @Selector@ for @fileURL@
fileURLSelector :: Selector '[] (Id NSURL)
fileURLSelector = mkSelector "fileURL"

-- | @Selector@ for @setFileURL:@
setFileURLSelector :: Selector '[Id NSURL] ()
setFileURLSelector = mkSelector "setFileURL:"

-- | @Selector@ for @fileModificationDate@
fileModificationDateSelector :: Selector '[] (Id NSDate)
fileModificationDateSelector = mkSelector "fileModificationDate"

-- | @Selector@ for @setFileModificationDate:@
setFileModificationDateSelector :: Selector '[Id NSDate] ()
setFileModificationDateSelector = mkSelector "setFileModificationDate:"

-- | @Selector@ for @draft@
draftSelector :: Selector '[] Bool
draftSelector = mkSelector "draft"

-- | @Selector@ for @setDraft:@
setDraftSelector :: Selector '[Bool] ()
setDraftSelector = mkSelector "setDraft:"

-- | @Selector@ for @entireFileLoaded@
entireFileLoadedSelector :: Selector '[] Bool
entireFileLoadedSelector = mkSelector "entireFileLoaded"

-- | @Selector@ for @autosavingIsImplicitlyCancellable@
autosavingIsImplicitlyCancellableSelector :: Selector '[] Bool
autosavingIsImplicitlyCancellableSelector = mkSelector "autosavingIsImplicitlyCancellable"

-- | @Selector@ for @keepBackupFile@
keepBackupFileSelector :: Selector '[] Bool
keepBackupFileSelector = mkSelector "keepBackupFile"

-- | @Selector@ for @backupFileURL@
backupFileURLSelector :: Selector '[] (Id NSURL)
backupFileURLSelector = mkSelector "backupFileURL"

-- | @Selector@ for @savePanelShowsFileFormatsControl@
savePanelShowsFileFormatsControlSelector :: Selector '[] Bool
savePanelShowsFileFormatsControlSelector = mkSelector "savePanelShowsFileFormatsControl"

-- | @Selector@ for @fileNameExtensionWasHiddenInLastRunSavePanel@
fileNameExtensionWasHiddenInLastRunSavePanelSelector :: Selector '[] Bool
fileNameExtensionWasHiddenInLastRunSavePanelSelector = mkSelector "fileNameExtensionWasHiddenInLastRunSavePanel"

-- | @Selector@ for @fileTypeFromLastRunSavePanel@
fileTypeFromLastRunSavePanelSelector :: Selector '[] (Id NSString)
fileTypeFromLastRunSavePanelSelector = mkSelector "fileTypeFromLastRunSavePanel"

-- | @Selector@ for @hasUnautosavedChanges@
hasUnautosavedChangesSelector :: Selector '[] Bool
hasUnautosavedChangesSelector = mkSelector "hasUnautosavedChanges"

-- | @Selector@ for @autosavesInPlace@
autosavesInPlaceSelector :: Selector '[] Bool
autosavesInPlaceSelector = mkSelector "autosavesInPlace"

-- | @Selector@ for @preservesVersions@
preservesVersionsSelector :: Selector '[] Bool
preservesVersionsSelector = mkSelector "preservesVersions"

-- | @Selector@ for @browsingVersions@
browsingVersionsSelector :: Selector '[] Bool
browsingVersionsSelector = mkSelector "browsingVersions"

-- | @Selector@ for @autosavesDrafts@
autosavesDraftsSelector :: Selector '[] Bool
autosavesDraftsSelector = mkSelector "autosavesDrafts"

-- | @Selector@ for @autosavingFileType@
autosavingFileTypeSelector :: Selector '[] (Id NSString)
autosavingFileTypeSelector = mkSelector "autosavingFileType"

-- | @Selector@ for @autosavedContentsFileURL@
autosavedContentsFileURLSelector :: Selector '[] (Id NSURL)
autosavedContentsFileURLSelector = mkSelector "autosavedContentsFileURL"

-- | @Selector@ for @setAutosavedContentsFileURL:@
setAutosavedContentsFileURLSelector :: Selector '[Id NSURL] ()
setAutosavedContentsFileURLSelector = mkSelector "setAutosavedContentsFileURL:"

-- | @Selector@ for @locked@
lockedSelector :: Selector '[] Bool
lockedSelector = mkSelector "locked"

-- | @Selector@ for @printInfo@
printInfoSelector :: Selector '[] (Id NSPrintInfo)
printInfoSelector = mkSelector "printInfo"

-- | @Selector@ for @setPrintInfo:@
setPrintInfoSelector :: Selector '[Id NSPrintInfo] ()
setPrintInfoSelector = mkSelector "setPrintInfo:"

-- | @Selector@ for @PDFPrintOperation@
pdfPrintOperationSelector :: Selector '[] (Id NSPrintOperation)
pdfPrintOperationSelector = mkSelector "PDFPrintOperation"

-- | @Selector@ for @allowsDocumentSharing@
allowsDocumentSharingSelector :: Selector '[] Bool
allowsDocumentSharingSelector = mkSelector "allowsDocumentSharing"

-- | @Selector@ for @previewRepresentableActivityItems@
previewRepresentableActivityItemsSelector :: Selector '[] (Id NSArray)
previewRepresentableActivityItemsSelector = mkSelector "previewRepresentableActivityItems"

-- | @Selector@ for @setPreviewRepresentableActivityItems:@
setPreviewRepresentableActivityItemsSelector :: Selector '[Id NSArray] ()
setPreviewRepresentableActivityItemsSelector = mkSelector "setPreviewRepresentableActivityItems:"

-- | @Selector@ for @documentEdited@
documentEditedSelector :: Selector '[] Bool
documentEditedSelector = mkSelector "documentEdited"

-- | @Selector@ for @inViewingMode@
inViewingModeSelector :: Selector '[] Bool
inViewingModeSelector = mkSelector "inViewingMode"

-- | @Selector@ for @undoManager@
undoManagerSelector :: Selector '[] (Id NSUndoManager)
undoManagerSelector = mkSelector "undoManager"

-- | @Selector@ for @setUndoManager:@
setUndoManagerSelector :: Selector '[Id NSUndoManager] ()
setUndoManagerSelector = mkSelector "setUndoManager:"

-- | @Selector@ for @hasUndoManager@
hasUndoManagerSelector :: Selector '[] Bool
hasUndoManagerSelector = mkSelector "hasUndoManager"

-- | @Selector@ for @setHasUndoManager:@
setHasUndoManagerSelector :: Selector '[Bool] ()
setHasUndoManagerSelector = mkSelector "setHasUndoManager:"

-- | @Selector@ for @windowNibName@
windowNibNameSelector :: Selector '[] (Id NSString)
windowNibNameSelector = mkSelector "windowNibName"

-- | @Selector@ for @windowControllers@
windowControllersSelector :: Selector '[] (Id NSArray)
windowControllersSelector = mkSelector "windowControllers"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector '[] (Id NSString)
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @windowForSheet@
windowForSheetSelector :: Selector '[] (Id NSWindow)
windowForSheetSelector = mkSelector "windowForSheet"

-- | @Selector@ for @readableTypes@
readableTypesSelector :: Selector '[] (Id NSArray)
readableTypesSelector = mkSelector "readableTypes"

-- | @Selector@ for @writableTypes@
writableTypesSelector :: Selector '[] (Id NSArray)
writableTypesSelector = mkSelector "writableTypes"

-- | @Selector@ for @usesUbiquitousStorage@
usesUbiquitousStorageSelector :: Selector '[] Bool
usesUbiquitousStorageSelector = mkSelector "usesUbiquitousStorage"

-- | @Selector@ for @presentedItemURL@
presentedItemURLSelector :: Selector '[] (Id NSURL)
presentedItemURLSelector = mkSelector "presentedItemURL"

-- | @Selector@ for @observedPresentedItemUbiquityAttributes@
observedPresentedItemUbiquityAttributesSelector :: Selector '[] (Id NSSet)
observedPresentedItemUbiquityAttributesSelector = mkSelector "observedPresentedItemUbiquityAttributes"

-- | @Selector@ for @restorableStateKeyPaths@
restorableStateKeyPathsSelector :: Selector '[] (Id NSArray)
restorableStateKeyPathsSelector = mkSelector "restorableStateKeyPaths"

-- | @Selector@ for @lastComponentOfFileName@
lastComponentOfFileNameSelector :: Selector '[] (Id NSString)
lastComponentOfFileNameSelector = mkSelector "lastComponentOfFileName"

-- | @Selector@ for @setLastComponentOfFileName:@
setLastComponentOfFileNameSelector :: Selector '[Id NSString] ()
setLastComponentOfFileNameSelector = mkSelector "setLastComponentOfFileName:"

-- | @Selector@ for @objectSpecifier@
objectSpecifierSelector :: Selector '[] (Id NSScriptObjectSpecifier)
objectSpecifierSelector = mkSelector "objectSpecifier"

-- | @Selector@ for @userActivity@
userActivitySelector :: Selector '[] (Id NSUserActivity)
userActivitySelector = mkSelector "userActivity"

-- | @Selector@ for @setUserActivity:@
setUserActivitySelector :: Selector '[Id NSUserActivity] ()
setUserActivitySelector = mkSelector "setUserActivity:"

-- | @Selector@ for @shouldRunSavePanelWithAccessoryView@
shouldRunSavePanelWithAccessoryViewSelector :: Selector '[] Bool
shouldRunSavePanelWithAccessoryViewSelector = mkSelector "shouldRunSavePanelWithAccessoryView"

