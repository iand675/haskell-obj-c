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
  , initSelector
  , initWithCoderSelector
  , documentForURLSelector
  , documentForWindowSelector
  , addDocumentSelector
  , removeDocumentSelector
  , newDocumentSelector
  , openUntitledDocumentAndDisplay_errorSelector
  , makeUntitledDocumentOfType_errorSelector
  , openDocumentSelector
  , urLsFromRunningOpenPanelSelector
  , runModalOpenPanel_forTypesSelector
  , beginOpenPanel_forTypes_completionHandlerSelector
  , openDocumentWithContentsOfURL_display_completionHandlerSelector
  , makeDocumentWithContentsOfURL_ofType_errorSelector
  , reopenDocumentForURL_withContentsOfURL_display_completionHandlerSelector
  , makeDocumentForURL_withContentsOfURL_ofType_errorSelector
  , saveAllDocumentsSelector
  , reviewUnsavedDocumentsWithAlertTitle_cancellable_delegate_didReviewAllSelector_contextInfoSelector
  , closeAllDocumentsWithDelegate_didCloseAllSelector_contextInfoSelector
  , duplicateDocumentWithContentsOfURL_copying_displayName_errorSelector
  , standardShareMenuItemSelector
  , presentError_modalForWindow_delegate_didPresentSelector_contextInfoSelector
  , presentErrorSelector
  , willPresentErrorSelector
  , clearRecentDocumentsSelector
  , noteNewRecentDocumentSelector
  , noteNewRecentDocumentURLSelector
  , typeForContentsOfURL_errorSelector
  , documentClassForTypeSelector
  , displayNameForTypeSelector
  , validateUserInterfaceItemSelector
  , openDocumentWithContentsOfURL_display_errorSelector
  , reopenDocumentForURL_withContentsOfURL_errorSelector
  , fileExtensionsFromTypeSelector
  , typeFromFileExtensionSelector
  , documentForFileNameSelector
  , fileNamesFromRunningOpenPanelSelector
  , makeDocumentWithContentsOfFile_ofTypeSelector
  , makeDocumentWithContentsOfURL_ofTypeSelector
  , makeUntitledDocumentOfTypeSelector
  , openDocumentWithContentsOfFile_displaySelector
  , openDocumentWithContentsOfURL_displaySelector
  , openUntitledDocumentOfType_displaySelector
  , setShouldCreateUISelector
  , shouldCreateUISelector
  , sharedDocumentControllerSelector
  , documentsSelector
  , currentDocumentSelector
  , currentDirectorySelector
  , autosavingDelaySelector
  , setAutosavingDelaySelector
  , hasEditedDocumentsSelector
  , allowsAutomaticShareMenuSelector
  , maximumRecentDocumentCountSelector
  , recentDocumentURLsSelector
  , defaultTypeSelector
  , documentClassNamesSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO (Id NSDocumentController)
init_ nsDocumentController  =
  sendMsg nsDocumentController (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSDocumentController nsDocumentController, IsNSCoder coder) => nsDocumentController -> coder -> IO (Id NSDocumentController)
initWithCoder nsDocumentController  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsDocumentController (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- documentForURL:@
documentForURL :: (IsNSDocumentController nsDocumentController, IsNSURL url) => nsDocumentController -> url -> IO (Id NSDocument)
documentForURL nsDocumentController  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsDocumentController (mkSelector "documentForURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @- documentForWindow:@
documentForWindow :: (IsNSDocumentController nsDocumentController, IsNSWindow window) => nsDocumentController -> window -> IO (Id NSDocument)
documentForWindow nsDocumentController  window =
withObjCPtr window $ \raw_window ->
    sendMsg nsDocumentController (mkSelector "documentForWindow:") (retPtr retVoid) [argPtr (castPtr raw_window :: Ptr ())] >>= retainedObject . castPtr

-- | @- addDocument:@
addDocument :: (IsNSDocumentController nsDocumentController, IsNSDocument document) => nsDocumentController -> document -> IO ()
addDocument nsDocumentController  document =
withObjCPtr document $ \raw_document ->
    sendMsg nsDocumentController (mkSelector "addDocument:") retVoid [argPtr (castPtr raw_document :: Ptr ())]

-- | @- removeDocument:@
removeDocument :: (IsNSDocumentController nsDocumentController, IsNSDocument document) => nsDocumentController -> document -> IO ()
removeDocument nsDocumentController  document =
withObjCPtr document $ \raw_document ->
    sendMsg nsDocumentController (mkSelector "removeDocument:") retVoid [argPtr (castPtr raw_document :: Ptr ())]

-- | @- newDocument:@
newDocument :: IsNSDocumentController nsDocumentController => nsDocumentController -> RawId -> IO ()
newDocument nsDocumentController  sender =
  sendMsg nsDocumentController (mkSelector "newDocument:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- openUntitledDocumentAndDisplay:error:@
openUntitledDocumentAndDisplay_error :: (IsNSDocumentController nsDocumentController, IsNSError outError) => nsDocumentController -> Bool -> outError -> IO (Id NSDocument)
openUntitledDocumentAndDisplay_error nsDocumentController  displayDocument outError =
withObjCPtr outError $ \raw_outError ->
    sendMsg nsDocumentController (mkSelector "openUntitledDocumentAndDisplay:error:") (retPtr retVoid) [argCULong (if displayDocument then 1 else 0), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | @- makeUntitledDocumentOfType:error:@
makeUntitledDocumentOfType_error :: (IsNSDocumentController nsDocumentController, IsNSString typeName, IsNSError outError) => nsDocumentController -> typeName -> outError -> IO (Id NSDocument)
makeUntitledDocumentOfType_error nsDocumentController  typeName outError =
withObjCPtr typeName $ \raw_typeName ->
  withObjCPtr outError $ \raw_outError ->
      sendMsg nsDocumentController (mkSelector "makeUntitledDocumentOfType:error:") (retPtr retVoid) [argPtr (castPtr raw_typeName :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | @- openDocument:@
openDocument :: IsNSDocumentController nsDocumentController => nsDocumentController -> RawId -> IO ()
openDocument nsDocumentController  sender =
  sendMsg nsDocumentController (mkSelector "openDocument:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- URLsFromRunningOpenPanel@
urLsFromRunningOpenPanel :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO (Id NSArray)
urLsFromRunningOpenPanel nsDocumentController  =
  sendMsg nsDocumentController (mkSelector "URLsFromRunningOpenPanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- runModalOpenPanel:forTypes:@
runModalOpenPanel_forTypes :: (IsNSDocumentController nsDocumentController, IsNSOpenPanel openPanel, IsNSArray types) => nsDocumentController -> openPanel -> types -> IO CLong
runModalOpenPanel_forTypes nsDocumentController  openPanel types =
withObjCPtr openPanel $ \raw_openPanel ->
  withObjCPtr types $ \raw_types ->
      sendMsg nsDocumentController (mkSelector "runModalOpenPanel:forTypes:") retCLong [argPtr (castPtr raw_openPanel :: Ptr ()), argPtr (castPtr raw_types :: Ptr ())]

-- | @- beginOpenPanel:forTypes:completionHandler:@
beginOpenPanel_forTypes_completionHandler :: (IsNSDocumentController nsDocumentController, IsNSOpenPanel openPanel, IsNSArray inTypes) => nsDocumentController -> openPanel -> inTypes -> Ptr () -> IO ()
beginOpenPanel_forTypes_completionHandler nsDocumentController  openPanel inTypes completionHandler =
withObjCPtr openPanel $ \raw_openPanel ->
  withObjCPtr inTypes $ \raw_inTypes ->
      sendMsg nsDocumentController (mkSelector "beginOpenPanel:forTypes:completionHandler:") retVoid [argPtr (castPtr raw_openPanel :: Ptr ()), argPtr (castPtr raw_inTypes :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- openDocumentWithContentsOfURL:display:completionHandler:@
openDocumentWithContentsOfURL_display_completionHandler :: (IsNSDocumentController nsDocumentController, IsNSURL url) => nsDocumentController -> url -> Bool -> Ptr () -> IO ()
openDocumentWithContentsOfURL_display_completionHandler nsDocumentController  url displayDocument completionHandler =
withObjCPtr url $ \raw_url ->
    sendMsg nsDocumentController (mkSelector "openDocumentWithContentsOfURL:display:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argCULong (if displayDocument then 1 else 0), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- makeDocumentWithContentsOfURL:ofType:error:@
makeDocumentWithContentsOfURL_ofType_error :: (IsNSDocumentController nsDocumentController, IsNSURL url, IsNSString typeName, IsNSError outError) => nsDocumentController -> url -> typeName -> outError -> IO (Id NSDocument)
makeDocumentWithContentsOfURL_ofType_error nsDocumentController  url typeName outError =
withObjCPtr url $ \raw_url ->
  withObjCPtr typeName $ \raw_typeName ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg nsDocumentController (mkSelector "makeDocumentWithContentsOfURL:ofType:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_typeName :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | @- reopenDocumentForURL:withContentsOfURL:display:completionHandler:@
reopenDocumentForURL_withContentsOfURL_display_completionHandler :: (IsNSDocumentController nsDocumentController, IsNSURL urlOrNil, IsNSURL contentsURL) => nsDocumentController -> urlOrNil -> contentsURL -> Bool -> Ptr () -> IO ()
reopenDocumentForURL_withContentsOfURL_display_completionHandler nsDocumentController  urlOrNil contentsURL displayDocument completionHandler =
withObjCPtr urlOrNil $ \raw_urlOrNil ->
  withObjCPtr contentsURL $ \raw_contentsURL ->
      sendMsg nsDocumentController (mkSelector "reopenDocumentForURL:withContentsOfURL:display:completionHandler:") retVoid [argPtr (castPtr raw_urlOrNil :: Ptr ()), argPtr (castPtr raw_contentsURL :: Ptr ()), argCULong (if displayDocument then 1 else 0), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- makeDocumentForURL:withContentsOfURL:ofType:error:@
makeDocumentForURL_withContentsOfURL_ofType_error :: (IsNSDocumentController nsDocumentController, IsNSURL urlOrNil, IsNSURL contentsURL, IsNSString typeName, IsNSError outError) => nsDocumentController -> urlOrNil -> contentsURL -> typeName -> outError -> IO (Id NSDocument)
makeDocumentForURL_withContentsOfURL_ofType_error nsDocumentController  urlOrNil contentsURL typeName outError =
withObjCPtr urlOrNil $ \raw_urlOrNil ->
  withObjCPtr contentsURL $ \raw_contentsURL ->
    withObjCPtr typeName $ \raw_typeName ->
      withObjCPtr outError $ \raw_outError ->
          sendMsg nsDocumentController (mkSelector "makeDocumentForURL:withContentsOfURL:ofType:error:") (retPtr retVoid) [argPtr (castPtr raw_urlOrNil :: Ptr ()), argPtr (castPtr raw_contentsURL :: Ptr ()), argPtr (castPtr raw_typeName :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | @- saveAllDocuments:@
saveAllDocuments :: IsNSDocumentController nsDocumentController => nsDocumentController -> RawId -> IO ()
saveAllDocuments nsDocumentController  sender =
  sendMsg nsDocumentController (mkSelector "saveAllDocuments:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- reviewUnsavedDocumentsWithAlertTitle:cancellable:delegate:didReviewAllSelector:contextInfo:@
reviewUnsavedDocumentsWithAlertTitle_cancellable_delegate_didReviewAllSelector_contextInfo :: (IsNSDocumentController nsDocumentController, IsNSString title) => nsDocumentController -> title -> Bool -> RawId -> Selector -> Ptr () -> IO ()
reviewUnsavedDocumentsWithAlertTitle_cancellable_delegate_didReviewAllSelector_contextInfo nsDocumentController  title cancellable delegate didReviewAllSelector contextInfo =
withObjCPtr title $ \raw_title ->
    sendMsg nsDocumentController (mkSelector "reviewUnsavedDocumentsWithAlertTitle:cancellable:delegate:didReviewAllSelector:contextInfo:") retVoid [argPtr (castPtr raw_title :: Ptr ()), argCULong (if cancellable then 1 else 0), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didReviewAllSelector), argPtr contextInfo]

-- | @- closeAllDocumentsWithDelegate:didCloseAllSelector:contextInfo:@
closeAllDocumentsWithDelegate_didCloseAllSelector_contextInfo :: IsNSDocumentController nsDocumentController => nsDocumentController -> RawId -> Selector -> Ptr () -> IO ()
closeAllDocumentsWithDelegate_didCloseAllSelector_contextInfo nsDocumentController  delegate didCloseAllSelector contextInfo =
  sendMsg nsDocumentController (mkSelector "closeAllDocumentsWithDelegate:didCloseAllSelector:contextInfo:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didCloseAllSelector), argPtr contextInfo]

-- | @- duplicateDocumentWithContentsOfURL:copying:displayName:error:@
duplicateDocumentWithContentsOfURL_copying_displayName_error :: (IsNSDocumentController nsDocumentController, IsNSURL url, IsNSString displayNameOrNil, IsNSError outError) => nsDocumentController -> url -> Bool -> displayNameOrNil -> outError -> IO (Id NSDocument)
duplicateDocumentWithContentsOfURL_copying_displayName_error nsDocumentController  url duplicateByCopying displayNameOrNil outError =
withObjCPtr url $ \raw_url ->
  withObjCPtr displayNameOrNil $ \raw_displayNameOrNil ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg nsDocumentController (mkSelector "duplicateDocumentWithContentsOfURL:copying:displayName:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (if duplicateByCopying then 1 else 0), argPtr (castPtr raw_displayNameOrNil :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | @- standardShareMenuItem@
standardShareMenuItem :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO (Id NSMenuItem)
standardShareMenuItem nsDocumentController  =
  sendMsg nsDocumentController (mkSelector "standardShareMenuItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- presentError:modalForWindow:delegate:didPresentSelector:contextInfo:@
presentError_modalForWindow_delegate_didPresentSelector_contextInfo :: (IsNSDocumentController nsDocumentController, IsNSError error_, IsNSWindow window) => nsDocumentController -> error_ -> window -> RawId -> Selector -> Ptr () -> IO ()
presentError_modalForWindow_delegate_didPresentSelector_contextInfo nsDocumentController  error_ window delegate didPresentSelector contextInfo =
withObjCPtr error_ $ \raw_error_ ->
  withObjCPtr window $ \raw_window ->
      sendMsg nsDocumentController (mkSelector "presentError:modalForWindow:delegate:didPresentSelector:contextInfo:") retVoid [argPtr (castPtr raw_error_ :: Ptr ()), argPtr (castPtr raw_window :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didPresentSelector), argPtr contextInfo]

-- | @- presentError:@
presentError :: (IsNSDocumentController nsDocumentController, IsNSError error_) => nsDocumentController -> error_ -> IO Bool
presentError nsDocumentController  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocumentController (mkSelector "presentError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- willPresentError:@
willPresentError :: (IsNSDocumentController nsDocumentController, IsNSError error_) => nsDocumentController -> error_ -> IO (Id NSError)
willPresentError nsDocumentController  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg nsDocumentController (mkSelector "willPresentError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- clearRecentDocuments:@
clearRecentDocuments :: IsNSDocumentController nsDocumentController => nsDocumentController -> RawId -> IO ()
clearRecentDocuments nsDocumentController  sender =
  sendMsg nsDocumentController (mkSelector "clearRecentDocuments:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- noteNewRecentDocument:@
noteNewRecentDocument :: (IsNSDocumentController nsDocumentController, IsNSDocument document) => nsDocumentController -> document -> IO ()
noteNewRecentDocument nsDocumentController  document =
withObjCPtr document $ \raw_document ->
    sendMsg nsDocumentController (mkSelector "noteNewRecentDocument:") retVoid [argPtr (castPtr raw_document :: Ptr ())]

-- | @- noteNewRecentDocumentURL:@
noteNewRecentDocumentURL :: (IsNSDocumentController nsDocumentController, IsNSURL url) => nsDocumentController -> url -> IO ()
noteNewRecentDocumentURL nsDocumentController  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsDocumentController (mkSelector "noteNewRecentDocumentURL:") retVoid [argPtr (castPtr raw_url :: Ptr ())]

-- | @- typeForContentsOfURL:error:@
typeForContentsOfURL_error :: (IsNSDocumentController nsDocumentController, IsNSURL url, IsNSError outError) => nsDocumentController -> url -> outError -> IO (Id NSString)
typeForContentsOfURL_error nsDocumentController  url outError =
withObjCPtr url $ \raw_url ->
  withObjCPtr outError $ \raw_outError ->
      sendMsg nsDocumentController (mkSelector "typeForContentsOfURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | @- documentClassForType:@
documentClassForType :: (IsNSDocumentController nsDocumentController, IsNSString typeName) => nsDocumentController -> typeName -> IO Class
documentClassForType nsDocumentController  typeName =
withObjCPtr typeName $ \raw_typeName ->
    fmap (Class . castPtr) $ sendMsg nsDocumentController (mkSelector "documentClassForType:") (retPtr retVoid) [argPtr (castPtr raw_typeName :: Ptr ())]

-- | @- displayNameForType:@
displayNameForType :: (IsNSDocumentController nsDocumentController, IsNSString typeName) => nsDocumentController -> typeName -> IO (Id NSString)
displayNameForType nsDocumentController  typeName =
withObjCPtr typeName $ \raw_typeName ->
    sendMsg nsDocumentController (mkSelector "displayNameForType:") (retPtr retVoid) [argPtr (castPtr raw_typeName :: Ptr ())] >>= retainedObject . castPtr

-- | @- validateUserInterfaceItem:@
validateUserInterfaceItem :: IsNSDocumentController nsDocumentController => nsDocumentController -> RawId -> IO Bool
validateUserInterfaceItem nsDocumentController  item =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocumentController (mkSelector "validateUserInterfaceItem:") retCULong [argPtr (castPtr (unRawId item) :: Ptr ())]

-- | @- openDocumentWithContentsOfURL:display:error:@
openDocumentWithContentsOfURL_display_error :: (IsNSDocumentController nsDocumentController, IsNSURL url, IsNSError outError) => nsDocumentController -> url -> Bool -> outError -> IO RawId
openDocumentWithContentsOfURL_display_error nsDocumentController  url displayDocument outError =
withObjCPtr url $ \raw_url ->
  withObjCPtr outError $ \raw_outError ->
      fmap (RawId . castPtr) $ sendMsg nsDocumentController (mkSelector "openDocumentWithContentsOfURL:display:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (if displayDocument then 1 else 0), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- reopenDocumentForURL:withContentsOfURL:error:@
reopenDocumentForURL_withContentsOfURL_error :: (IsNSDocumentController nsDocumentController, IsNSURL url, IsNSURL contentsURL, IsNSError outError) => nsDocumentController -> url -> contentsURL -> outError -> IO Bool
reopenDocumentForURL_withContentsOfURL_error nsDocumentController  url contentsURL outError =
withObjCPtr url $ \raw_url ->
  withObjCPtr contentsURL $ \raw_contentsURL ->
    withObjCPtr outError $ \raw_outError ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocumentController (mkSelector "reopenDocumentForURL:withContentsOfURL:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_contentsURL :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- fileExtensionsFromType:@
fileExtensionsFromType :: (IsNSDocumentController nsDocumentController, IsNSString typeName) => nsDocumentController -> typeName -> IO (Id NSArray)
fileExtensionsFromType nsDocumentController  typeName =
withObjCPtr typeName $ \raw_typeName ->
    sendMsg nsDocumentController (mkSelector "fileExtensionsFromType:") (retPtr retVoid) [argPtr (castPtr raw_typeName :: Ptr ())] >>= retainedObject . castPtr

-- | @- typeFromFileExtension:@
typeFromFileExtension :: (IsNSDocumentController nsDocumentController, IsNSString fileNameExtensionOrHFSFileType) => nsDocumentController -> fileNameExtensionOrHFSFileType -> IO (Id NSString)
typeFromFileExtension nsDocumentController  fileNameExtensionOrHFSFileType =
withObjCPtr fileNameExtensionOrHFSFileType $ \raw_fileNameExtensionOrHFSFileType ->
    sendMsg nsDocumentController (mkSelector "typeFromFileExtension:") (retPtr retVoid) [argPtr (castPtr raw_fileNameExtensionOrHFSFileType :: Ptr ())] >>= retainedObject . castPtr

-- | @- documentForFileName:@
documentForFileName :: (IsNSDocumentController nsDocumentController, IsNSString fileName) => nsDocumentController -> fileName -> IO RawId
documentForFileName nsDocumentController  fileName =
withObjCPtr fileName $ \raw_fileName ->
    fmap (RawId . castPtr) $ sendMsg nsDocumentController (mkSelector "documentForFileName:") (retPtr retVoid) [argPtr (castPtr raw_fileName :: Ptr ())]

-- | @- fileNamesFromRunningOpenPanel@
fileNamesFromRunningOpenPanel :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO (Id NSArray)
fileNamesFromRunningOpenPanel nsDocumentController  =
  sendMsg nsDocumentController (mkSelector "fileNamesFromRunningOpenPanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- makeDocumentWithContentsOfFile:ofType:@
makeDocumentWithContentsOfFile_ofType :: (IsNSDocumentController nsDocumentController, IsNSString fileName, IsNSString type_) => nsDocumentController -> fileName -> type_ -> IO RawId
makeDocumentWithContentsOfFile_ofType nsDocumentController  fileName type_ =
withObjCPtr fileName $ \raw_fileName ->
  withObjCPtr type_ $ \raw_type_ ->
      fmap (RawId . castPtr) $ sendMsg nsDocumentController (mkSelector "makeDocumentWithContentsOfFile:ofType:") (retPtr retVoid) [argPtr (castPtr raw_fileName :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- makeDocumentWithContentsOfURL:ofType:@
makeDocumentWithContentsOfURL_ofType :: (IsNSDocumentController nsDocumentController, IsNSURL url, IsNSString type_) => nsDocumentController -> url -> type_ -> IO RawId
makeDocumentWithContentsOfURL_ofType nsDocumentController  url type_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr type_ $ \raw_type_ ->
      fmap (RawId . castPtr) $ sendMsg nsDocumentController (mkSelector "makeDocumentWithContentsOfURL:ofType:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- makeUntitledDocumentOfType:@
makeUntitledDocumentOfType :: (IsNSDocumentController nsDocumentController, IsNSString type_) => nsDocumentController -> type_ -> IO RawId
makeUntitledDocumentOfType nsDocumentController  type_ =
withObjCPtr type_ $ \raw_type_ ->
    fmap (RawId . castPtr) $ sendMsg nsDocumentController (mkSelector "makeUntitledDocumentOfType:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- openDocumentWithContentsOfFile:display:@
openDocumentWithContentsOfFile_display :: (IsNSDocumentController nsDocumentController, IsNSString fileName) => nsDocumentController -> fileName -> Bool -> IO RawId
openDocumentWithContentsOfFile_display nsDocumentController  fileName display =
withObjCPtr fileName $ \raw_fileName ->
    fmap (RawId . castPtr) $ sendMsg nsDocumentController (mkSelector "openDocumentWithContentsOfFile:display:") (retPtr retVoid) [argPtr (castPtr raw_fileName :: Ptr ()), argCULong (if display then 1 else 0)]

-- | @- openDocumentWithContentsOfURL:display:@
openDocumentWithContentsOfURL_display :: (IsNSDocumentController nsDocumentController, IsNSURL url) => nsDocumentController -> url -> Bool -> IO RawId
openDocumentWithContentsOfURL_display nsDocumentController  url display =
withObjCPtr url $ \raw_url ->
    fmap (RawId . castPtr) $ sendMsg nsDocumentController (mkSelector "openDocumentWithContentsOfURL:display:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (if display then 1 else 0)]

-- | @- openUntitledDocumentOfType:display:@
openUntitledDocumentOfType_display :: (IsNSDocumentController nsDocumentController, IsNSString type_) => nsDocumentController -> type_ -> Bool -> IO RawId
openUntitledDocumentOfType_display nsDocumentController  type_ display =
withObjCPtr type_ $ \raw_type_ ->
    fmap (RawId . castPtr) $ sendMsg nsDocumentController (mkSelector "openUntitledDocumentOfType:display:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ()), argCULong (if display then 1 else 0)]

-- | @- setShouldCreateUI:@
setShouldCreateUI :: IsNSDocumentController nsDocumentController => nsDocumentController -> Bool -> IO ()
setShouldCreateUI nsDocumentController  flag =
  sendMsg nsDocumentController (mkSelector "setShouldCreateUI:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- shouldCreateUI@
shouldCreateUI :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO Bool
shouldCreateUI nsDocumentController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocumentController (mkSelector "shouldCreateUI") retCULong []

-- | @+ sharedDocumentController@
sharedDocumentController :: IO (Id NSDocumentController)
sharedDocumentController  =
  do
    cls' <- getRequiredClass "NSDocumentController"
    sendClassMsg cls' (mkSelector "sharedDocumentController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- documents@
documents :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO (Id NSArray)
documents nsDocumentController  =
  sendMsg nsDocumentController (mkSelector "documents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currentDocument@
currentDocument :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO (Id NSDocument)
currentDocument nsDocumentController  =
  sendMsg nsDocumentController (mkSelector "currentDocument") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currentDirectory@
currentDirectory :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO (Id NSString)
currentDirectory nsDocumentController  =
  sendMsg nsDocumentController (mkSelector "currentDirectory") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- autosavingDelay@
autosavingDelay :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO CDouble
autosavingDelay nsDocumentController  =
  sendMsg nsDocumentController (mkSelector "autosavingDelay") retCDouble []

-- | @- setAutosavingDelay:@
setAutosavingDelay :: IsNSDocumentController nsDocumentController => nsDocumentController -> CDouble -> IO ()
setAutosavingDelay nsDocumentController  value =
  sendMsg nsDocumentController (mkSelector "setAutosavingDelay:") retVoid [argCDouble (fromIntegral value)]

-- | @- hasEditedDocuments@
hasEditedDocuments :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO Bool
hasEditedDocuments nsDocumentController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocumentController (mkSelector "hasEditedDocuments") retCULong []

-- | @- allowsAutomaticShareMenu@
allowsAutomaticShareMenu :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO Bool
allowsAutomaticShareMenu nsDocumentController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDocumentController (mkSelector "allowsAutomaticShareMenu") retCULong []

-- | @- maximumRecentDocumentCount@
maximumRecentDocumentCount :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO CULong
maximumRecentDocumentCount nsDocumentController  =
  sendMsg nsDocumentController (mkSelector "maximumRecentDocumentCount") retCULong []

-- | @- recentDocumentURLs@
recentDocumentURLs :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO (Id NSArray)
recentDocumentURLs nsDocumentController  =
  sendMsg nsDocumentController (mkSelector "recentDocumentURLs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- defaultType@
defaultType :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO (Id NSString)
defaultType nsDocumentController  =
  sendMsg nsDocumentController (mkSelector "defaultType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- documentClassNames@
documentClassNames :: IsNSDocumentController nsDocumentController => nsDocumentController -> IO (Id NSArray)
documentClassNames nsDocumentController  =
  sendMsg nsDocumentController (mkSelector "documentClassNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @documentForURL:@
documentForURLSelector :: Selector
documentForURLSelector = mkSelector "documentForURL:"

-- | @Selector@ for @documentForWindow:@
documentForWindowSelector :: Selector
documentForWindowSelector = mkSelector "documentForWindow:"

-- | @Selector@ for @addDocument:@
addDocumentSelector :: Selector
addDocumentSelector = mkSelector "addDocument:"

-- | @Selector@ for @removeDocument:@
removeDocumentSelector :: Selector
removeDocumentSelector = mkSelector "removeDocument:"

-- | @Selector@ for @newDocument:@
newDocumentSelector :: Selector
newDocumentSelector = mkSelector "newDocument:"

-- | @Selector@ for @openUntitledDocumentAndDisplay:error:@
openUntitledDocumentAndDisplay_errorSelector :: Selector
openUntitledDocumentAndDisplay_errorSelector = mkSelector "openUntitledDocumentAndDisplay:error:"

-- | @Selector@ for @makeUntitledDocumentOfType:error:@
makeUntitledDocumentOfType_errorSelector :: Selector
makeUntitledDocumentOfType_errorSelector = mkSelector "makeUntitledDocumentOfType:error:"

-- | @Selector@ for @openDocument:@
openDocumentSelector :: Selector
openDocumentSelector = mkSelector "openDocument:"

-- | @Selector@ for @URLsFromRunningOpenPanel@
urLsFromRunningOpenPanelSelector :: Selector
urLsFromRunningOpenPanelSelector = mkSelector "URLsFromRunningOpenPanel"

-- | @Selector@ for @runModalOpenPanel:forTypes:@
runModalOpenPanel_forTypesSelector :: Selector
runModalOpenPanel_forTypesSelector = mkSelector "runModalOpenPanel:forTypes:"

-- | @Selector@ for @beginOpenPanel:forTypes:completionHandler:@
beginOpenPanel_forTypes_completionHandlerSelector :: Selector
beginOpenPanel_forTypes_completionHandlerSelector = mkSelector "beginOpenPanel:forTypes:completionHandler:"

-- | @Selector@ for @openDocumentWithContentsOfURL:display:completionHandler:@
openDocumentWithContentsOfURL_display_completionHandlerSelector :: Selector
openDocumentWithContentsOfURL_display_completionHandlerSelector = mkSelector "openDocumentWithContentsOfURL:display:completionHandler:"

-- | @Selector@ for @makeDocumentWithContentsOfURL:ofType:error:@
makeDocumentWithContentsOfURL_ofType_errorSelector :: Selector
makeDocumentWithContentsOfURL_ofType_errorSelector = mkSelector "makeDocumentWithContentsOfURL:ofType:error:"

-- | @Selector@ for @reopenDocumentForURL:withContentsOfURL:display:completionHandler:@
reopenDocumentForURL_withContentsOfURL_display_completionHandlerSelector :: Selector
reopenDocumentForURL_withContentsOfURL_display_completionHandlerSelector = mkSelector "reopenDocumentForURL:withContentsOfURL:display:completionHandler:"

-- | @Selector@ for @makeDocumentForURL:withContentsOfURL:ofType:error:@
makeDocumentForURL_withContentsOfURL_ofType_errorSelector :: Selector
makeDocumentForURL_withContentsOfURL_ofType_errorSelector = mkSelector "makeDocumentForURL:withContentsOfURL:ofType:error:"

-- | @Selector@ for @saveAllDocuments:@
saveAllDocumentsSelector :: Selector
saveAllDocumentsSelector = mkSelector "saveAllDocuments:"

-- | @Selector@ for @reviewUnsavedDocumentsWithAlertTitle:cancellable:delegate:didReviewAllSelector:contextInfo:@
reviewUnsavedDocumentsWithAlertTitle_cancellable_delegate_didReviewAllSelector_contextInfoSelector :: Selector
reviewUnsavedDocumentsWithAlertTitle_cancellable_delegate_didReviewAllSelector_contextInfoSelector = mkSelector "reviewUnsavedDocumentsWithAlertTitle:cancellable:delegate:didReviewAllSelector:contextInfo:"

-- | @Selector@ for @closeAllDocumentsWithDelegate:didCloseAllSelector:contextInfo:@
closeAllDocumentsWithDelegate_didCloseAllSelector_contextInfoSelector :: Selector
closeAllDocumentsWithDelegate_didCloseAllSelector_contextInfoSelector = mkSelector "closeAllDocumentsWithDelegate:didCloseAllSelector:contextInfo:"

-- | @Selector@ for @duplicateDocumentWithContentsOfURL:copying:displayName:error:@
duplicateDocumentWithContentsOfURL_copying_displayName_errorSelector :: Selector
duplicateDocumentWithContentsOfURL_copying_displayName_errorSelector = mkSelector "duplicateDocumentWithContentsOfURL:copying:displayName:error:"

-- | @Selector@ for @standardShareMenuItem@
standardShareMenuItemSelector :: Selector
standardShareMenuItemSelector = mkSelector "standardShareMenuItem"

-- | @Selector@ for @presentError:modalForWindow:delegate:didPresentSelector:contextInfo:@
presentError_modalForWindow_delegate_didPresentSelector_contextInfoSelector :: Selector
presentError_modalForWindow_delegate_didPresentSelector_contextInfoSelector = mkSelector "presentError:modalForWindow:delegate:didPresentSelector:contextInfo:"

-- | @Selector@ for @presentError:@
presentErrorSelector :: Selector
presentErrorSelector = mkSelector "presentError:"

-- | @Selector@ for @willPresentError:@
willPresentErrorSelector :: Selector
willPresentErrorSelector = mkSelector "willPresentError:"

-- | @Selector@ for @clearRecentDocuments:@
clearRecentDocumentsSelector :: Selector
clearRecentDocumentsSelector = mkSelector "clearRecentDocuments:"

-- | @Selector@ for @noteNewRecentDocument:@
noteNewRecentDocumentSelector :: Selector
noteNewRecentDocumentSelector = mkSelector "noteNewRecentDocument:"

-- | @Selector@ for @noteNewRecentDocumentURL:@
noteNewRecentDocumentURLSelector :: Selector
noteNewRecentDocumentURLSelector = mkSelector "noteNewRecentDocumentURL:"

-- | @Selector@ for @typeForContentsOfURL:error:@
typeForContentsOfURL_errorSelector :: Selector
typeForContentsOfURL_errorSelector = mkSelector "typeForContentsOfURL:error:"

-- | @Selector@ for @documentClassForType:@
documentClassForTypeSelector :: Selector
documentClassForTypeSelector = mkSelector "documentClassForType:"

-- | @Selector@ for @displayNameForType:@
displayNameForTypeSelector :: Selector
displayNameForTypeSelector = mkSelector "displayNameForType:"

-- | @Selector@ for @validateUserInterfaceItem:@
validateUserInterfaceItemSelector :: Selector
validateUserInterfaceItemSelector = mkSelector "validateUserInterfaceItem:"

-- | @Selector@ for @openDocumentWithContentsOfURL:display:error:@
openDocumentWithContentsOfURL_display_errorSelector :: Selector
openDocumentWithContentsOfURL_display_errorSelector = mkSelector "openDocumentWithContentsOfURL:display:error:"

-- | @Selector@ for @reopenDocumentForURL:withContentsOfURL:error:@
reopenDocumentForURL_withContentsOfURL_errorSelector :: Selector
reopenDocumentForURL_withContentsOfURL_errorSelector = mkSelector "reopenDocumentForURL:withContentsOfURL:error:"

-- | @Selector@ for @fileExtensionsFromType:@
fileExtensionsFromTypeSelector :: Selector
fileExtensionsFromTypeSelector = mkSelector "fileExtensionsFromType:"

-- | @Selector@ for @typeFromFileExtension:@
typeFromFileExtensionSelector :: Selector
typeFromFileExtensionSelector = mkSelector "typeFromFileExtension:"

-- | @Selector@ for @documentForFileName:@
documentForFileNameSelector :: Selector
documentForFileNameSelector = mkSelector "documentForFileName:"

-- | @Selector@ for @fileNamesFromRunningOpenPanel@
fileNamesFromRunningOpenPanelSelector :: Selector
fileNamesFromRunningOpenPanelSelector = mkSelector "fileNamesFromRunningOpenPanel"

-- | @Selector@ for @makeDocumentWithContentsOfFile:ofType:@
makeDocumentWithContentsOfFile_ofTypeSelector :: Selector
makeDocumentWithContentsOfFile_ofTypeSelector = mkSelector "makeDocumentWithContentsOfFile:ofType:"

-- | @Selector@ for @makeDocumentWithContentsOfURL:ofType:@
makeDocumentWithContentsOfURL_ofTypeSelector :: Selector
makeDocumentWithContentsOfURL_ofTypeSelector = mkSelector "makeDocumentWithContentsOfURL:ofType:"

-- | @Selector@ for @makeUntitledDocumentOfType:@
makeUntitledDocumentOfTypeSelector :: Selector
makeUntitledDocumentOfTypeSelector = mkSelector "makeUntitledDocumentOfType:"

-- | @Selector@ for @openDocumentWithContentsOfFile:display:@
openDocumentWithContentsOfFile_displaySelector :: Selector
openDocumentWithContentsOfFile_displaySelector = mkSelector "openDocumentWithContentsOfFile:display:"

-- | @Selector@ for @openDocumentWithContentsOfURL:display:@
openDocumentWithContentsOfURL_displaySelector :: Selector
openDocumentWithContentsOfURL_displaySelector = mkSelector "openDocumentWithContentsOfURL:display:"

-- | @Selector@ for @openUntitledDocumentOfType:display:@
openUntitledDocumentOfType_displaySelector :: Selector
openUntitledDocumentOfType_displaySelector = mkSelector "openUntitledDocumentOfType:display:"

-- | @Selector@ for @setShouldCreateUI:@
setShouldCreateUISelector :: Selector
setShouldCreateUISelector = mkSelector "setShouldCreateUI:"

-- | @Selector@ for @shouldCreateUI@
shouldCreateUISelector :: Selector
shouldCreateUISelector = mkSelector "shouldCreateUI"

-- | @Selector@ for @sharedDocumentController@
sharedDocumentControllerSelector :: Selector
sharedDocumentControllerSelector = mkSelector "sharedDocumentController"

-- | @Selector@ for @documents@
documentsSelector :: Selector
documentsSelector = mkSelector "documents"

-- | @Selector@ for @currentDocument@
currentDocumentSelector :: Selector
currentDocumentSelector = mkSelector "currentDocument"

-- | @Selector@ for @currentDirectory@
currentDirectorySelector :: Selector
currentDirectorySelector = mkSelector "currentDirectory"

-- | @Selector@ for @autosavingDelay@
autosavingDelaySelector :: Selector
autosavingDelaySelector = mkSelector "autosavingDelay"

-- | @Selector@ for @setAutosavingDelay:@
setAutosavingDelaySelector :: Selector
setAutosavingDelaySelector = mkSelector "setAutosavingDelay:"

-- | @Selector@ for @hasEditedDocuments@
hasEditedDocumentsSelector :: Selector
hasEditedDocumentsSelector = mkSelector "hasEditedDocuments"

-- | @Selector@ for @allowsAutomaticShareMenu@
allowsAutomaticShareMenuSelector :: Selector
allowsAutomaticShareMenuSelector = mkSelector "allowsAutomaticShareMenu"

-- | @Selector@ for @maximumRecentDocumentCount@
maximumRecentDocumentCountSelector :: Selector
maximumRecentDocumentCountSelector = mkSelector "maximumRecentDocumentCount"

-- | @Selector@ for @recentDocumentURLs@
recentDocumentURLsSelector :: Selector
recentDocumentURLsSelector = mkSelector "recentDocumentURLs"

-- | @Selector@ for @defaultType@
defaultTypeSelector :: Selector
defaultTypeSelector = mkSelector "defaultType"

-- | @Selector@ for @documentClassNames@
documentClassNamesSelector :: Selector
documentClassNamesSelector = mkSelector "documentClassNames"

