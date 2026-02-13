{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDocumentController@.
module ObjC.AppKit.NSDocumentController
  ( NSDocumentController
  , IsNSDocumentController(..)
  , init_
  , initWithCoder
  , documentForURL
  , documentForWindow
  , addDocument
  , removeDocument
  , newDocument
  , openUntitledDocumentAndDisplay_error
  , makeUntitledDocumentOfType_error
  , openDocument
  , urLsFromRunningOpenPanel
  , runModalOpenPanel_forTypes
  , beginOpenPanel_forTypes_completionHandler
  , openDocumentWithContentsOfURL_display_completionHandler
  , makeDocumentWithContentsOfURL_ofType_error
  , reopenDocumentForURL_withContentsOfURL_display_completionHandler
  , makeDocumentForURL_withContentsOfURL_ofType_error
  , saveAllDocuments
  , reviewUnsavedDocumentsWithAlertTitle_cancellable_delegate_didReviewAllSelector_contextInfo
  , closeAllDocumentsWithDelegate_didCloseAllSelector_contextInfo
  , duplicateDocumentWithContentsOfURL_copying_displayName_error
  , standardShareMenuItem
  , presentError_modalForWindow_delegate_didPresentSelector_contextInfo
  , presentError
  , willPresentError
  , clearRecentDocuments
  , noteNewRecentDocument
  , noteNewRecentDocumentURL
  , typeForContentsOfURL_error
  , documentClassForType
  , displayNameForType
  , validateUserInterfaceItem
  , openDocumentWithContentsOfURL_display_error
  , reopenDocumentForURL_withContentsOfURL_error
  , fileExtensionsFromType
  , typeFromFileExtension
  , documentForFileName
  , fileNamesFromRunningOpenPanel
  , makeDocumentWithContentsOfFile_ofType
  , makeDocumentWithContentsOfURL_ofType
  , makeUntitledDocumentOfType
  , openDocumentWithContentsOfFile_display
  , openDocumentWithContentsOfURL_display
  , openUntitledDocumentOfType_display
  , setShouldCreateUI
  , shouldCreateUI
  , sharedDocumentController
  , documents
  , currentDocument
  , currentDirectory
  , autosavingDelay
  , setAutosavingDelay
  , hasEditedDocuments
  , allowsAutomaticShareMenu
  , maximumRecentDocumentCount
  , recentDocumentURLs
  , defaultType
  , documentClassNames
  , addDocumentSelector
  , allowsAutomaticShareMenuSelector
  , autosavingDelaySelector
  , beginOpenPanel_forTypes_completionHandlerSelector
  , clearRecentDocumentsSelector
  , closeAllDocumentsWithDelegate_didCloseAllSelector_contextInfoSelector
  , currentDirectorySelector
  , currentDocumentSelector
  , defaultTypeSelector
  , displayNameForTypeSelector
  , documentClassForTypeSelector
  , documentClassNamesSelector
  , documentForFileNameSelector
  , documentForURLSelector
  , documentForWindowSelector
  , documentsSelector
  , duplicateDocumentWithContentsOfURL_copying_displayName_errorSelector
  , fileExtensionsFromTypeSelector
  , fileNamesFromRunningOpenPanelSelector
  , hasEditedDocumentsSelector
  , initSelector
  , initWithCoderSelector
  , makeDocumentForURL_withContentsOfURL_ofType_errorSelector
  , makeDocumentWithContentsOfFile_ofTypeSelector
  , makeDocumentWithContentsOfURL_ofTypeSelector
  , makeDocumentWithContentsOfURL_ofType_errorSelector
  , makeUntitledDocumentOfTypeSelector
  , makeUntitledDocumentOfType_errorSelector
  , maximumRecentDocumentCountSelector
  , newDocumentSelector
  , noteNewRecentDocumentSelector
  , noteNewRecentDocumentURLSelector
  , openDocumentSelector
  , openDocumentWithContentsOfFile_displaySelector
  , openDocumentWithContentsOfURL_displaySelector
  , openDocumentWithContentsOfURL_display_completionHandlerSelector
  , openDocumentWithContentsOfURL_display_errorSelector
  , openUntitledDocumentAndDisplay_errorSelector
  , openUntitledDocumentOfType_displaySelector
  , presentErrorSelector
  , presentError_modalForWindow_delegate_didPresentSelector_contextInfoSelector
  , recentDocumentURLsSelector
  , removeDocumentSelector
  , reopenDocumentForURL_withContentsOfURL_display_completionHandlerSelector
  , reopenDocumentForURL_withContentsOfURL_errorSelector
  , reviewUnsavedDocumentsWithAlertTitle_cancellable_delegate_didReviewAllSelector_contextInfoSelector
  , runModalOpenPanel_forTypesSelector
  , saveAllDocumentsSelector
  , setAutosavingDelaySelector
  , setShouldCreateUISelector
  , sharedDocumentControllerSelector
  , shouldCreateUISelector
  , standardShareMenuItemSelector
  , typeForContentsOfURL_errorSelector
  , typeFromFileExtensionSelector
  , urLsFromRunningOpenPanelSelector
  , validateUserInterfaceItemSelector
  , willPresentErrorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO (Id NSDocumentController)
init_ nsDocumentController =
  sendOwnedMessage nsDocumentController initSelector

-- | @- initWithCoder:@
initWithCoder :: (IsNSDocumentController nsDocumentController, IsNSCoder coder) => nsDocumentController -> coder -> IO (Id NSDocumentController)
initWithCoder nsDocumentController coder =
  sendOwnedMessage nsDocumentController initWithCoderSelector (toNSCoder coder)

-- | @- documentForURL:@
documentForURL :: (IsNSDocumentController nsDocumentController, IsNSURL url) => nsDocumentController -> url -> IO (Id NSDocument)
documentForURL nsDocumentController url =
  sendMessage nsDocumentController documentForURLSelector (toNSURL url)

-- | @- documentForWindow:@
documentForWindow :: (IsNSDocumentController nsDocumentController, IsNSWindow window) => nsDocumentController -> window -> IO (Id NSDocument)
documentForWindow nsDocumentController window =
  sendMessage nsDocumentController documentForWindowSelector (toNSWindow window)

-- | @- addDocument:@
addDocument :: (IsNSDocumentController nsDocumentController, IsNSDocument document) => nsDocumentController -> document -> IO ()
addDocument nsDocumentController document =
  sendMessage nsDocumentController addDocumentSelector (toNSDocument document)

-- | @- removeDocument:@
removeDocument :: (IsNSDocumentController nsDocumentController, IsNSDocument document) => nsDocumentController -> document -> IO ()
removeDocument nsDocumentController document =
  sendMessage nsDocumentController removeDocumentSelector (toNSDocument document)

-- | @- newDocument:@
newDocument :: IsNSDocumentController nsDocumentController => nsDocumentController -> RawId -> IO ()
newDocument nsDocumentController sender =
  sendOwnedMessage nsDocumentController newDocumentSelector sender

-- | @- openUntitledDocumentAndDisplay:error:@
openUntitledDocumentAndDisplay_error :: (IsNSDocumentController nsDocumentController, IsNSError outError) => nsDocumentController -> Bool -> outError -> IO (Id NSDocument)
openUntitledDocumentAndDisplay_error nsDocumentController displayDocument outError =
  sendMessage nsDocumentController openUntitledDocumentAndDisplay_errorSelector displayDocument (toNSError outError)

-- | @- makeUntitledDocumentOfType:error:@
makeUntitledDocumentOfType_error :: (IsNSDocumentController nsDocumentController, IsNSString typeName, IsNSError outError) => nsDocumentController -> typeName -> outError -> IO (Id NSDocument)
makeUntitledDocumentOfType_error nsDocumentController typeName outError =
  sendMessage nsDocumentController makeUntitledDocumentOfType_errorSelector (toNSString typeName) (toNSError outError)

-- | @- openDocument:@
openDocument :: IsNSDocumentController nsDocumentController => nsDocumentController -> RawId -> IO ()
openDocument nsDocumentController sender =
  sendMessage nsDocumentController openDocumentSelector sender

-- | @- URLsFromRunningOpenPanel@
urLsFromRunningOpenPanel :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO (Id NSArray)
urLsFromRunningOpenPanel nsDocumentController =
  sendMessage nsDocumentController urLsFromRunningOpenPanelSelector

-- | @- runModalOpenPanel:forTypes:@
runModalOpenPanel_forTypes :: (IsNSDocumentController nsDocumentController, IsNSOpenPanel openPanel, IsNSArray types) => nsDocumentController -> openPanel -> types -> IO CLong
runModalOpenPanel_forTypes nsDocumentController openPanel types =
  sendMessage nsDocumentController runModalOpenPanel_forTypesSelector (toNSOpenPanel openPanel) (toNSArray types)

-- | @- beginOpenPanel:forTypes:completionHandler:@
beginOpenPanel_forTypes_completionHandler :: (IsNSDocumentController nsDocumentController, IsNSOpenPanel openPanel, IsNSArray inTypes) => nsDocumentController -> openPanel -> inTypes -> Ptr () -> IO ()
beginOpenPanel_forTypes_completionHandler nsDocumentController openPanel inTypes completionHandler =
  sendMessage nsDocumentController beginOpenPanel_forTypes_completionHandlerSelector (toNSOpenPanel openPanel) (toNSArray inTypes) completionHandler

-- | @- openDocumentWithContentsOfURL:display:completionHandler:@
openDocumentWithContentsOfURL_display_completionHandler :: (IsNSDocumentController nsDocumentController, IsNSURL url) => nsDocumentController -> url -> Bool -> Ptr () -> IO ()
openDocumentWithContentsOfURL_display_completionHandler nsDocumentController url displayDocument completionHandler =
  sendMessage nsDocumentController openDocumentWithContentsOfURL_display_completionHandlerSelector (toNSURL url) displayDocument completionHandler

-- | @- makeDocumentWithContentsOfURL:ofType:error:@
makeDocumentWithContentsOfURL_ofType_error :: (IsNSDocumentController nsDocumentController, IsNSURL url, IsNSString typeName, IsNSError outError) => nsDocumentController -> url -> typeName -> outError -> IO (Id NSDocument)
makeDocumentWithContentsOfURL_ofType_error nsDocumentController url typeName outError =
  sendMessage nsDocumentController makeDocumentWithContentsOfURL_ofType_errorSelector (toNSURL url) (toNSString typeName) (toNSError outError)

-- | @- reopenDocumentForURL:withContentsOfURL:display:completionHandler:@
reopenDocumentForURL_withContentsOfURL_display_completionHandler :: (IsNSDocumentController nsDocumentController, IsNSURL urlOrNil, IsNSURL contentsURL) => nsDocumentController -> urlOrNil -> contentsURL -> Bool -> Ptr () -> IO ()
reopenDocumentForURL_withContentsOfURL_display_completionHandler nsDocumentController urlOrNil contentsURL displayDocument completionHandler =
  sendMessage nsDocumentController reopenDocumentForURL_withContentsOfURL_display_completionHandlerSelector (toNSURL urlOrNil) (toNSURL contentsURL) displayDocument completionHandler

-- | @- makeDocumentForURL:withContentsOfURL:ofType:error:@
makeDocumentForURL_withContentsOfURL_ofType_error :: (IsNSDocumentController nsDocumentController, IsNSURL urlOrNil, IsNSURL contentsURL, IsNSString typeName, IsNSError outError) => nsDocumentController -> urlOrNil -> contentsURL -> typeName -> outError -> IO (Id NSDocument)
makeDocumentForURL_withContentsOfURL_ofType_error nsDocumentController urlOrNil contentsURL typeName outError =
  sendMessage nsDocumentController makeDocumentForURL_withContentsOfURL_ofType_errorSelector (toNSURL urlOrNil) (toNSURL contentsURL) (toNSString typeName) (toNSError outError)

-- | @- saveAllDocuments:@
saveAllDocuments :: IsNSDocumentController nsDocumentController => nsDocumentController -> RawId -> IO ()
saveAllDocuments nsDocumentController sender =
  sendMessage nsDocumentController saveAllDocumentsSelector sender

-- | @- reviewUnsavedDocumentsWithAlertTitle:cancellable:delegate:didReviewAllSelector:contextInfo:@
reviewUnsavedDocumentsWithAlertTitle_cancellable_delegate_didReviewAllSelector_contextInfo :: (IsNSDocumentController nsDocumentController, IsNSString title) => nsDocumentController -> title -> Bool -> RawId -> Sel -> Ptr () -> IO ()
reviewUnsavedDocumentsWithAlertTitle_cancellable_delegate_didReviewAllSelector_contextInfo nsDocumentController title cancellable delegate didReviewAllSelector contextInfo =
  sendMessage nsDocumentController reviewUnsavedDocumentsWithAlertTitle_cancellable_delegate_didReviewAllSelector_contextInfoSelector (toNSString title) cancellable delegate didReviewAllSelector contextInfo

-- | @- closeAllDocumentsWithDelegate:didCloseAllSelector:contextInfo:@
closeAllDocumentsWithDelegate_didCloseAllSelector_contextInfo :: IsNSDocumentController nsDocumentController => nsDocumentController -> RawId -> Sel -> Ptr () -> IO ()
closeAllDocumentsWithDelegate_didCloseAllSelector_contextInfo nsDocumentController delegate didCloseAllSelector contextInfo =
  sendMessage nsDocumentController closeAllDocumentsWithDelegate_didCloseAllSelector_contextInfoSelector delegate didCloseAllSelector contextInfo

-- | @- duplicateDocumentWithContentsOfURL:copying:displayName:error:@
duplicateDocumentWithContentsOfURL_copying_displayName_error :: (IsNSDocumentController nsDocumentController, IsNSURL url, IsNSString displayNameOrNil, IsNSError outError) => nsDocumentController -> url -> Bool -> displayNameOrNil -> outError -> IO (Id NSDocument)
duplicateDocumentWithContentsOfURL_copying_displayName_error nsDocumentController url duplicateByCopying displayNameOrNil outError =
  sendMessage nsDocumentController duplicateDocumentWithContentsOfURL_copying_displayName_errorSelector (toNSURL url) duplicateByCopying (toNSString displayNameOrNil) (toNSError outError)

-- | @- standardShareMenuItem@
standardShareMenuItem :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO (Id NSMenuItem)
standardShareMenuItem nsDocumentController =
  sendMessage nsDocumentController standardShareMenuItemSelector

-- | @- presentError:modalForWindow:delegate:didPresentSelector:contextInfo:@
presentError_modalForWindow_delegate_didPresentSelector_contextInfo :: (IsNSDocumentController nsDocumentController, IsNSError error_, IsNSWindow window) => nsDocumentController -> error_ -> window -> RawId -> Sel -> Ptr () -> IO ()
presentError_modalForWindow_delegate_didPresentSelector_contextInfo nsDocumentController error_ window delegate didPresentSelector contextInfo =
  sendMessage nsDocumentController presentError_modalForWindow_delegate_didPresentSelector_contextInfoSelector (toNSError error_) (toNSWindow window) delegate didPresentSelector contextInfo

-- | @- presentError:@
presentError :: (IsNSDocumentController nsDocumentController, IsNSError error_) => nsDocumentController -> error_ -> IO Bool
presentError nsDocumentController error_ =
  sendMessage nsDocumentController presentErrorSelector (toNSError error_)

-- | @- willPresentError:@
willPresentError :: (IsNSDocumentController nsDocumentController, IsNSError error_) => nsDocumentController -> error_ -> IO (Id NSError)
willPresentError nsDocumentController error_ =
  sendMessage nsDocumentController willPresentErrorSelector (toNSError error_)

-- | @- clearRecentDocuments:@
clearRecentDocuments :: IsNSDocumentController nsDocumentController => nsDocumentController -> RawId -> IO ()
clearRecentDocuments nsDocumentController sender =
  sendMessage nsDocumentController clearRecentDocumentsSelector sender

-- | @- noteNewRecentDocument:@
noteNewRecentDocument :: (IsNSDocumentController nsDocumentController, IsNSDocument document) => nsDocumentController -> document -> IO ()
noteNewRecentDocument nsDocumentController document =
  sendMessage nsDocumentController noteNewRecentDocumentSelector (toNSDocument document)

-- | @- noteNewRecentDocumentURL:@
noteNewRecentDocumentURL :: (IsNSDocumentController nsDocumentController, IsNSURL url) => nsDocumentController -> url -> IO ()
noteNewRecentDocumentURL nsDocumentController url =
  sendMessage nsDocumentController noteNewRecentDocumentURLSelector (toNSURL url)

-- | @- typeForContentsOfURL:error:@
typeForContentsOfURL_error :: (IsNSDocumentController nsDocumentController, IsNSURL url, IsNSError outError) => nsDocumentController -> url -> outError -> IO (Id NSString)
typeForContentsOfURL_error nsDocumentController url outError =
  sendMessage nsDocumentController typeForContentsOfURL_errorSelector (toNSURL url) (toNSError outError)

-- | @- documentClassForType:@
documentClassForType :: (IsNSDocumentController nsDocumentController, IsNSString typeName) => nsDocumentController -> typeName -> IO Class
documentClassForType nsDocumentController typeName =
  sendMessage nsDocumentController documentClassForTypeSelector (toNSString typeName)

-- | @- displayNameForType:@
displayNameForType :: (IsNSDocumentController nsDocumentController, IsNSString typeName) => nsDocumentController -> typeName -> IO (Id NSString)
displayNameForType nsDocumentController typeName =
  sendMessage nsDocumentController displayNameForTypeSelector (toNSString typeName)

-- | @- validateUserInterfaceItem:@
validateUserInterfaceItem :: IsNSDocumentController nsDocumentController => nsDocumentController -> RawId -> IO Bool
validateUserInterfaceItem nsDocumentController item =
  sendMessage nsDocumentController validateUserInterfaceItemSelector item

-- | @- openDocumentWithContentsOfURL:display:error:@
openDocumentWithContentsOfURL_display_error :: (IsNSDocumentController nsDocumentController, IsNSURL url, IsNSError outError) => nsDocumentController -> url -> Bool -> outError -> IO RawId
openDocumentWithContentsOfURL_display_error nsDocumentController url displayDocument outError =
  sendMessage nsDocumentController openDocumentWithContentsOfURL_display_errorSelector (toNSURL url) displayDocument (toNSError outError)

-- | @- reopenDocumentForURL:withContentsOfURL:error:@
reopenDocumentForURL_withContentsOfURL_error :: (IsNSDocumentController nsDocumentController, IsNSURL url, IsNSURL contentsURL, IsNSError outError) => nsDocumentController -> url -> contentsURL -> outError -> IO Bool
reopenDocumentForURL_withContentsOfURL_error nsDocumentController url contentsURL outError =
  sendMessage nsDocumentController reopenDocumentForURL_withContentsOfURL_errorSelector (toNSURL url) (toNSURL contentsURL) (toNSError outError)

-- | @- fileExtensionsFromType:@
fileExtensionsFromType :: (IsNSDocumentController nsDocumentController, IsNSString typeName) => nsDocumentController -> typeName -> IO (Id NSArray)
fileExtensionsFromType nsDocumentController typeName =
  sendMessage nsDocumentController fileExtensionsFromTypeSelector (toNSString typeName)

-- | @- typeFromFileExtension:@
typeFromFileExtension :: (IsNSDocumentController nsDocumentController, IsNSString fileNameExtensionOrHFSFileType) => nsDocumentController -> fileNameExtensionOrHFSFileType -> IO (Id NSString)
typeFromFileExtension nsDocumentController fileNameExtensionOrHFSFileType =
  sendMessage nsDocumentController typeFromFileExtensionSelector (toNSString fileNameExtensionOrHFSFileType)

-- | @- documentForFileName:@
documentForFileName :: (IsNSDocumentController nsDocumentController, IsNSString fileName) => nsDocumentController -> fileName -> IO RawId
documentForFileName nsDocumentController fileName =
  sendMessage nsDocumentController documentForFileNameSelector (toNSString fileName)

-- | @- fileNamesFromRunningOpenPanel@
fileNamesFromRunningOpenPanel :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO (Id NSArray)
fileNamesFromRunningOpenPanel nsDocumentController =
  sendMessage nsDocumentController fileNamesFromRunningOpenPanelSelector

-- | @- makeDocumentWithContentsOfFile:ofType:@
makeDocumentWithContentsOfFile_ofType :: (IsNSDocumentController nsDocumentController, IsNSString fileName, IsNSString type_) => nsDocumentController -> fileName -> type_ -> IO RawId
makeDocumentWithContentsOfFile_ofType nsDocumentController fileName type_ =
  sendMessage nsDocumentController makeDocumentWithContentsOfFile_ofTypeSelector (toNSString fileName) (toNSString type_)

-- | @- makeDocumentWithContentsOfURL:ofType:@
makeDocumentWithContentsOfURL_ofType :: (IsNSDocumentController nsDocumentController, IsNSURL url, IsNSString type_) => nsDocumentController -> url -> type_ -> IO RawId
makeDocumentWithContentsOfURL_ofType nsDocumentController url type_ =
  sendMessage nsDocumentController makeDocumentWithContentsOfURL_ofTypeSelector (toNSURL url) (toNSString type_)

-- | @- makeUntitledDocumentOfType:@
makeUntitledDocumentOfType :: (IsNSDocumentController nsDocumentController, IsNSString type_) => nsDocumentController -> type_ -> IO RawId
makeUntitledDocumentOfType nsDocumentController type_ =
  sendMessage nsDocumentController makeUntitledDocumentOfTypeSelector (toNSString type_)

-- | @- openDocumentWithContentsOfFile:display:@
openDocumentWithContentsOfFile_display :: (IsNSDocumentController nsDocumentController, IsNSString fileName) => nsDocumentController -> fileName -> Bool -> IO RawId
openDocumentWithContentsOfFile_display nsDocumentController fileName display =
  sendMessage nsDocumentController openDocumentWithContentsOfFile_displaySelector (toNSString fileName) display

-- | @- openDocumentWithContentsOfURL:display:@
openDocumentWithContentsOfURL_display :: (IsNSDocumentController nsDocumentController, IsNSURL url) => nsDocumentController -> url -> Bool -> IO RawId
openDocumentWithContentsOfURL_display nsDocumentController url display =
  sendMessage nsDocumentController openDocumentWithContentsOfURL_displaySelector (toNSURL url) display

-- | @- openUntitledDocumentOfType:display:@
openUntitledDocumentOfType_display :: (IsNSDocumentController nsDocumentController, IsNSString type_) => nsDocumentController -> type_ -> Bool -> IO RawId
openUntitledDocumentOfType_display nsDocumentController type_ display =
  sendMessage nsDocumentController openUntitledDocumentOfType_displaySelector (toNSString type_) display

-- | @- setShouldCreateUI:@
setShouldCreateUI :: IsNSDocumentController nsDocumentController => nsDocumentController -> Bool -> IO ()
setShouldCreateUI nsDocumentController flag =
  sendMessage nsDocumentController setShouldCreateUISelector flag

-- | @- shouldCreateUI@
shouldCreateUI :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO Bool
shouldCreateUI nsDocumentController =
  sendMessage nsDocumentController shouldCreateUISelector

-- | @+ sharedDocumentController@
sharedDocumentController :: IO (Id NSDocumentController)
sharedDocumentController  =
  do
    cls' <- getRequiredClass "NSDocumentController"
    sendClassMessage cls' sharedDocumentControllerSelector

-- | @- documents@
documents :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO (Id NSArray)
documents nsDocumentController =
  sendMessage nsDocumentController documentsSelector

-- | @- currentDocument@
currentDocument :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO (Id NSDocument)
currentDocument nsDocumentController =
  sendMessage nsDocumentController currentDocumentSelector

-- | @- currentDirectory@
currentDirectory :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO (Id NSString)
currentDirectory nsDocumentController =
  sendMessage nsDocumentController currentDirectorySelector

-- | @- autosavingDelay@
autosavingDelay :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO CDouble
autosavingDelay nsDocumentController =
  sendMessage nsDocumentController autosavingDelaySelector

-- | @- setAutosavingDelay:@
setAutosavingDelay :: IsNSDocumentController nsDocumentController => nsDocumentController -> CDouble -> IO ()
setAutosavingDelay nsDocumentController value =
  sendMessage nsDocumentController setAutosavingDelaySelector value

-- | @- hasEditedDocuments@
hasEditedDocuments :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO Bool
hasEditedDocuments nsDocumentController =
  sendMessage nsDocumentController hasEditedDocumentsSelector

-- | @- allowsAutomaticShareMenu@
allowsAutomaticShareMenu :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO Bool
allowsAutomaticShareMenu nsDocumentController =
  sendMessage nsDocumentController allowsAutomaticShareMenuSelector

-- | @- maximumRecentDocumentCount@
maximumRecentDocumentCount :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO CULong
maximumRecentDocumentCount nsDocumentController =
  sendMessage nsDocumentController maximumRecentDocumentCountSelector

-- | @- recentDocumentURLs@
recentDocumentURLs :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO (Id NSArray)
recentDocumentURLs nsDocumentController =
  sendMessage nsDocumentController recentDocumentURLsSelector

-- | @- defaultType@
defaultType :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO (Id NSString)
defaultType nsDocumentController =
  sendMessage nsDocumentController defaultTypeSelector

-- | @- documentClassNames@
documentClassNames :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO (Id NSArray)
documentClassNames nsDocumentController =
  sendMessage nsDocumentController documentClassNamesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSDocumentController)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSDocumentController)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @documentForURL:@
documentForURLSelector :: Selector '[Id NSURL] (Id NSDocument)
documentForURLSelector = mkSelector "documentForURL:"

-- | @Selector@ for @documentForWindow:@
documentForWindowSelector :: Selector '[Id NSWindow] (Id NSDocument)
documentForWindowSelector = mkSelector "documentForWindow:"

-- | @Selector@ for @addDocument:@
addDocumentSelector :: Selector '[Id NSDocument] ()
addDocumentSelector = mkSelector "addDocument:"

-- | @Selector@ for @removeDocument:@
removeDocumentSelector :: Selector '[Id NSDocument] ()
removeDocumentSelector = mkSelector "removeDocument:"

-- | @Selector@ for @newDocument:@
newDocumentSelector :: Selector '[RawId] ()
newDocumentSelector = mkSelector "newDocument:"

-- | @Selector@ for @openUntitledDocumentAndDisplay:error:@
openUntitledDocumentAndDisplay_errorSelector :: Selector '[Bool, Id NSError] (Id NSDocument)
openUntitledDocumentAndDisplay_errorSelector = mkSelector "openUntitledDocumentAndDisplay:error:"

-- | @Selector@ for @makeUntitledDocumentOfType:error:@
makeUntitledDocumentOfType_errorSelector :: Selector '[Id NSString, Id NSError] (Id NSDocument)
makeUntitledDocumentOfType_errorSelector = mkSelector "makeUntitledDocumentOfType:error:"

-- | @Selector@ for @openDocument:@
openDocumentSelector :: Selector '[RawId] ()
openDocumentSelector = mkSelector "openDocument:"

-- | @Selector@ for @URLsFromRunningOpenPanel@
urLsFromRunningOpenPanelSelector :: Selector '[] (Id NSArray)
urLsFromRunningOpenPanelSelector = mkSelector "URLsFromRunningOpenPanel"

-- | @Selector@ for @runModalOpenPanel:forTypes:@
runModalOpenPanel_forTypesSelector :: Selector '[Id NSOpenPanel, Id NSArray] CLong
runModalOpenPanel_forTypesSelector = mkSelector "runModalOpenPanel:forTypes:"

-- | @Selector@ for @beginOpenPanel:forTypes:completionHandler:@
beginOpenPanel_forTypes_completionHandlerSelector :: Selector '[Id NSOpenPanel, Id NSArray, Ptr ()] ()
beginOpenPanel_forTypes_completionHandlerSelector = mkSelector "beginOpenPanel:forTypes:completionHandler:"

-- | @Selector@ for @openDocumentWithContentsOfURL:display:completionHandler:@
openDocumentWithContentsOfURL_display_completionHandlerSelector :: Selector '[Id NSURL, Bool, Ptr ()] ()
openDocumentWithContentsOfURL_display_completionHandlerSelector = mkSelector "openDocumentWithContentsOfURL:display:completionHandler:"

-- | @Selector@ for @makeDocumentWithContentsOfURL:ofType:error:@
makeDocumentWithContentsOfURL_ofType_errorSelector :: Selector '[Id NSURL, Id NSString, Id NSError] (Id NSDocument)
makeDocumentWithContentsOfURL_ofType_errorSelector = mkSelector "makeDocumentWithContentsOfURL:ofType:error:"

-- | @Selector@ for @reopenDocumentForURL:withContentsOfURL:display:completionHandler:@
reopenDocumentForURL_withContentsOfURL_display_completionHandlerSelector :: Selector '[Id NSURL, Id NSURL, Bool, Ptr ()] ()
reopenDocumentForURL_withContentsOfURL_display_completionHandlerSelector = mkSelector "reopenDocumentForURL:withContentsOfURL:display:completionHandler:"

-- | @Selector@ for @makeDocumentForURL:withContentsOfURL:ofType:error:@
makeDocumentForURL_withContentsOfURL_ofType_errorSelector :: Selector '[Id NSURL, Id NSURL, Id NSString, Id NSError] (Id NSDocument)
makeDocumentForURL_withContentsOfURL_ofType_errorSelector = mkSelector "makeDocumentForURL:withContentsOfURL:ofType:error:"

-- | @Selector@ for @saveAllDocuments:@
saveAllDocumentsSelector :: Selector '[RawId] ()
saveAllDocumentsSelector = mkSelector "saveAllDocuments:"

-- | @Selector@ for @reviewUnsavedDocumentsWithAlertTitle:cancellable:delegate:didReviewAllSelector:contextInfo:@
reviewUnsavedDocumentsWithAlertTitle_cancellable_delegate_didReviewAllSelector_contextInfoSelector :: Selector '[Id NSString, Bool, RawId, Sel, Ptr ()] ()
reviewUnsavedDocumentsWithAlertTitle_cancellable_delegate_didReviewAllSelector_contextInfoSelector = mkSelector "reviewUnsavedDocumentsWithAlertTitle:cancellable:delegate:didReviewAllSelector:contextInfo:"

-- | @Selector@ for @closeAllDocumentsWithDelegate:didCloseAllSelector:contextInfo:@
closeAllDocumentsWithDelegate_didCloseAllSelector_contextInfoSelector :: Selector '[RawId, Sel, Ptr ()] ()
closeAllDocumentsWithDelegate_didCloseAllSelector_contextInfoSelector = mkSelector "closeAllDocumentsWithDelegate:didCloseAllSelector:contextInfo:"

-- | @Selector@ for @duplicateDocumentWithContentsOfURL:copying:displayName:error:@
duplicateDocumentWithContentsOfURL_copying_displayName_errorSelector :: Selector '[Id NSURL, Bool, Id NSString, Id NSError] (Id NSDocument)
duplicateDocumentWithContentsOfURL_copying_displayName_errorSelector = mkSelector "duplicateDocumentWithContentsOfURL:copying:displayName:error:"

-- | @Selector@ for @standardShareMenuItem@
standardShareMenuItemSelector :: Selector '[] (Id NSMenuItem)
standardShareMenuItemSelector = mkSelector "standardShareMenuItem"

-- | @Selector@ for @presentError:modalForWindow:delegate:didPresentSelector:contextInfo:@
presentError_modalForWindow_delegate_didPresentSelector_contextInfoSelector :: Selector '[Id NSError, Id NSWindow, RawId, Sel, Ptr ()] ()
presentError_modalForWindow_delegate_didPresentSelector_contextInfoSelector = mkSelector "presentError:modalForWindow:delegate:didPresentSelector:contextInfo:"

-- | @Selector@ for @presentError:@
presentErrorSelector :: Selector '[Id NSError] Bool
presentErrorSelector = mkSelector "presentError:"

-- | @Selector@ for @willPresentError:@
willPresentErrorSelector :: Selector '[Id NSError] (Id NSError)
willPresentErrorSelector = mkSelector "willPresentError:"

-- | @Selector@ for @clearRecentDocuments:@
clearRecentDocumentsSelector :: Selector '[RawId] ()
clearRecentDocumentsSelector = mkSelector "clearRecentDocuments:"

-- | @Selector@ for @noteNewRecentDocument:@
noteNewRecentDocumentSelector :: Selector '[Id NSDocument] ()
noteNewRecentDocumentSelector = mkSelector "noteNewRecentDocument:"

-- | @Selector@ for @noteNewRecentDocumentURL:@
noteNewRecentDocumentURLSelector :: Selector '[Id NSURL] ()
noteNewRecentDocumentURLSelector = mkSelector "noteNewRecentDocumentURL:"

-- | @Selector@ for @typeForContentsOfURL:error:@
typeForContentsOfURL_errorSelector :: Selector '[Id NSURL, Id NSError] (Id NSString)
typeForContentsOfURL_errorSelector = mkSelector "typeForContentsOfURL:error:"

-- | @Selector@ for @documentClassForType:@
documentClassForTypeSelector :: Selector '[Id NSString] Class
documentClassForTypeSelector = mkSelector "documentClassForType:"

-- | @Selector@ for @displayNameForType:@
displayNameForTypeSelector :: Selector '[Id NSString] (Id NSString)
displayNameForTypeSelector = mkSelector "displayNameForType:"

-- | @Selector@ for @validateUserInterfaceItem:@
validateUserInterfaceItemSelector :: Selector '[RawId] Bool
validateUserInterfaceItemSelector = mkSelector "validateUserInterfaceItem:"

-- | @Selector@ for @openDocumentWithContentsOfURL:display:error:@
openDocumentWithContentsOfURL_display_errorSelector :: Selector '[Id NSURL, Bool, Id NSError] RawId
openDocumentWithContentsOfURL_display_errorSelector = mkSelector "openDocumentWithContentsOfURL:display:error:"

-- | @Selector@ for @reopenDocumentForURL:withContentsOfURL:error:@
reopenDocumentForURL_withContentsOfURL_errorSelector :: Selector '[Id NSURL, Id NSURL, Id NSError] Bool
reopenDocumentForURL_withContentsOfURL_errorSelector = mkSelector "reopenDocumentForURL:withContentsOfURL:error:"

-- | @Selector@ for @fileExtensionsFromType:@
fileExtensionsFromTypeSelector :: Selector '[Id NSString] (Id NSArray)
fileExtensionsFromTypeSelector = mkSelector "fileExtensionsFromType:"

-- | @Selector@ for @typeFromFileExtension:@
typeFromFileExtensionSelector :: Selector '[Id NSString] (Id NSString)
typeFromFileExtensionSelector = mkSelector "typeFromFileExtension:"

-- | @Selector@ for @documentForFileName:@
documentForFileNameSelector :: Selector '[Id NSString] RawId
documentForFileNameSelector = mkSelector "documentForFileName:"

-- | @Selector@ for @fileNamesFromRunningOpenPanel@
fileNamesFromRunningOpenPanelSelector :: Selector '[] (Id NSArray)
fileNamesFromRunningOpenPanelSelector = mkSelector "fileNamesFromRunningOpenPanel"

-- | @Selector@ for @makeDocumentWithContentsOfFile:ofType:@
makeDocumentWithContentsOfFile_ofTypeSelector :: Selector '[Id NSString, Id NSString] RawId
makeDocumentWithContentsOfFile_ofTypeSelector = mkSelector "makeDocumentWithContentsOfFile:ofType:"

-- | @Selector@ for @makeDocumentWithContentsOfURL:ofType:@
makeDocumentWithContentsOfURL_ofTypeSelector :: Selector '[Id NSURL, Id NSString] RawId
makeDocumentWithContentsOfURL_ofTypeSelector = mkSelector "makeDocumentWithContentsOfURL:ofType:"

-- | @Selector@ for @makeUntitledDocumentOfType:@
makeUntitledDocumentOfTypeSelector :: Selector '[Id NSString] RawId
makeUntitledDocumentOfTypeSelector = mkSelector "makeUntitledDocumentOfType:"

-- | @Selector@ for @openDocumentWithContentsOfFile:display:@
openDocumentWithContentsOfFile_displaySelector :: Selector '[Id NSString, Bool] RawId
openDocumentWithContentsOfFile_displaySelector = mkSelector "openDocumentWithContentsOfFile:display:"

-- | @Selector@ for @openDocumentWithContentsOfURL:display:@
openDocumentWithContentsOfURL_displaySelector :: Selector '[Id NSURL, Bool] RawId
openDocumentWithContentsOfURL_displaySelector = mkSelector "openDocumentWithContentsOfURL:display:"

-- | @Selector@ for @openUntitledDocumentOfType:display:@
openUntitledDocumentOfType_displaySelector :: Selector '[Id NSString, Bool] RawId
openUntitledDocumentOfType_displaySelector = mkSelector "openUntitledDocumentOfType:display:"

-- | @Selector@ for @setShouldCreateUI:@
setShouldCreateUISelector :: Selector '[Bool] ()
setShouldCreateUISelector = mkSelector "setShouldCreateUI:"

-- | @Selector@ for @shouldCreateUI@
shouldCreateUISelector :: Selector '[] Bool
shouldCreateUISelector = mkSelector "shouldCreateUI"

-- | @Selector@ for @sharedDocumentController@
sharedDocumentControllerSelector :: Selector '[] (Id NSDocumentController)
sharedDocumentControllerSelector = mkSelector "sharedDocumentController"

-- | @Selector@ for @documents@
documentsSelector :: Selector '[] (Id NSArray)
documentsSelector = mkSelector "documents"

-- | @Selector@ for @currentDocument@
currentDocumentSelector :: Selector '[] (Id NSDocument)
currentDocumentSelector = mkSelector "currentDocument"

-- | @Selector@ for @currentDirectory@
currentDirectorySelector :: Selector '[] (Id NSString)
currentDirectorySelector = mkSelector "currentDirectory"

-- | @Selector@ for @autosavingDelay@
autosavingDelaySelector :: Selector '[] CDouble
autosavingDelaySelector = mkSelector "autosavingDelay"

-- | @Selector@ for @setAutosavingDelay:@
setAutosavingDelaySelector :: Selector '[CDouble] ()
setAutosavingDelaySelector = mkSelector "setAutosavingDelay:"

-- | @Selector@ for @hasEditedDocuments@
hasEditedDocumentsSelector :: Selector '[] Bool
hasEditedDocumentsSelector = mkSelector "hasEditedDocuments"

-- | @Selector@ for @allowsAutomaticShareMenu@
allowsAutomaticShareMenuSelector :: Selector '[] Bool
allowsAutomaticShareMenuSelector = mkSelector "allowsAutomaticShareMenu"

-- | @Selector@ for @maximumRecentDocumentCount@
maximumRecentDocumentCountSelector :: Selector '[] CULong
maximumRecentDocumentCountSelector = mkSelector "maximumRecentDocumentCount"

-- | @Selector@ for @recentDocumentURLs@
recentDocumentURLsSelector :: Selector '[] (Id NSArray)
recentDocumentURLsSelector = mkSelector "recentDocumentURLs"

-- | @Selector@ for @defaultType@
defaultTypeSelector :: Selector '[] (Id NSString)
defaultTypeSelector = mkSelector "defaultType"

-- | @Selector@ for @documentClassNames@
documentClassNamesSelector :: Selector '[] (Id NSArray)
documentClassNamesSelector = mkSelector "documentClassNames"

