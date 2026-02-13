{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFileCoordinator@.
module ObjC.Foundation.NSFileCoordinator
  ( NSFileCoordinator
  , IsNSFileCoordinator(..)
  , addFilePresenter
  , removeFilePresenter
  , initWithFilePresenter
  , coordinateAccessWithIntents_queue_byAccessor
  , coordinateReadingItemAtURL_options_error_byAccessor
  , coordinateWritingItemAtURL_options_error_byAccessor
  , coordinateReadingItemAtURL_options_writingItemAtURL_options_error_byAccessor
  , coordinateWritingItemAtURL_options_writingItemAtURL_options_error_byAccessor
  , prepareForReadingItemsAtURLs_options_writingItemsAtURLs_options_error_byAccessor
  , itemAtURL_willMoveToURL
  , itemAtURL_didMoveToURL
  , itemAtURL_didChangeUbiquityAttributes
  , cancel
  , filePresenters
  , purposeIdentifier
  , setPurposeIdentifier
  , addFilePresenterSelector
  , cancelSelector
  , coordinateAccessWithIntents_queue_byAccessorSelector
  , coordinateReadingItemAtURL_options_error_byAccessorSelector
  , coordinateReadingItemAtURL_options_writingItemAtURL_options_error_byAccessorSelector
  , coordinateWritingItemAtURL_options_error_byAccessorSelector
  , coordinateWritingItemAtURL_options_writingItemAtURL_options_error_byAccessorSelector
  , filePresentersSelector
  , initWithFilePresenterSelector
  , itemAtURL_didChangeUbiquityAttributesSelector
  , itemAtURL_didMoveToURLSelector
  , itemAtURL_willMoveToURLSelector
  , prepareForReadingItemsAtURLs_options_writingItemsAtURLs_options_error_byAccessorSelector
  , purposeIdentifierSelector
  , removeFilePresenterSelector
  , setPurposeIdentifierSelector

  -- * Enum types
  , NSFileCoordinatorReadingOptions(NSFileCoordinatorReadingOptions)
  , pattern NSFileCoordinatorReadingWithoutChanges
  , pattern NSFileCoordinatorReadingResolvesSymbolicLink
  , pattern NSFileCoordinatorReadingImmediatelyAvailableMetadataOnly
  , pattern NSFileCoordinatorReadingForUploading
  , NSFileCoordinatorWritingOptions(NSFileCoordinatorWritingOptions)
  , pattern NSFileCoordinatorWritingForDeleting
  , pattern NSFileCoordinatorWritingForMoving
  , pattern NSFileCoordinatorWritingForMerging
  , pattern NSFileCoordinatorWritingForReplacing
  , pattern NSFileCoordinatorWritingContentIndependentMetadataOnly

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @+ addFilePresenter:@
addFilePresenter :: RawId -> IO ()
addFilePresenter filePresenter =
  do
    cls' <- getRequiredClass "NSFileCoordinator"
    sendClassMessage cls' addFilePresenterSelector filePresenter

-- | @+ removeFilePresenter:@
removeFilePresenter :: RawId -> IO ()
removeFilePresenter filePresenter =
  do
    cls' <- getRequiredClass "NSFileCoordinator"
    sendClassMessage cls' removeFilePresenterSelector filePresenter

-- | @- initWithFilePresenter:@
initWithFilePresenter :: IsNSFileCoordinator nsFileCoordinator => nsFileCoordinator -> RawId -> IO (Id NSFileCoordinator)
initWithFilePresenter nsFileCoordinator filePresenterOrNil =
  sendOwnedMessage nsFileCoordinator initWithFilePresenterSelector filePresenterOrNil

-- | @- coordinateAccessWithIntents:queue:byAccessor:@
coordinateAccessWithIntents_queue_byAccessor :: (IsNSFileCoordinator nsFileCoordinator, IsNSArray intents, IsNSOperationQueue queue) => nsFileCoordinator -> intents -> queue -> Ptr () -> IO ()
coordinateAccessWithIntents_queue_byAccessor nsFileCoordinator intents queue accessor =
  sendMessage nsFileCoordinator coordinateAccessWithIntents_queue_byAccessorSelector (toNSArray intents) (toNSOperationQueue queue) accessor

-- | @- coordinateReadingItemAtURL:options:error:byAccessor:@
coordinateReadingItemAtURL_options_error_byAccessor :: (IsNSFileCoordinator nsFileCoordinator, IsNSURL url, IsNSError outError) => nsFileCoordinator -> url -> NSFileCoordinatorReadingOptions -> outError -> Ptr () -> IO ()
coordinateReadingItemAtURL_options_error_byAccessor nsFileCoordinator url options outError reader =
  sendMessage nsFileCoordinator coordinateReadingItemAtURL_options_error_byAccessorSelector (toNSURL url) options (toNSError outError) reader

-- | @- coordinateWritingItemAtURL:options:error:byAccessor:@
coordinateWritingItemAtURL_options_error_byAccessor :: (IsNSFileCoordinator nsFileCoordinator, IsNSURL url, IsNSError outError) => nsFileCoordinator -> url -> NSFileCoordinatorWritingOptions -> outError -> Ptr () -> IO ()
coordinateWritingItemAtURL_options_error_byAccessor nsFileCoordinator url options outError writer =
  sendMessage nsFileCoordinator coordinateWritingItemAtURL_options_error_byAccessorSelector (toNSURL url) options (toNSError outError) writer

-- | @- coordinateReadingItemAtURL:options:writingItemAtURL:options:error:byAccessor:@
coordinateReadingItemAtURL_options_writingItemAtURL_options_error_byAccessor :: (IsNSFileCoordinator nsFileCoordinator, IsNSURL readingURL, IsNSURL writingURL, IsNSError outError) => nsFileCoordinator -> readingURL -> NSFileCoordinatorReadingOptions -> writingURL -> NSFileCoordinatorWritingOptions -> outError -> Ptr () -> IO ()
coordinateReadingItemAtURL_options_writingItemAtURL_options_error_byAccessor nsFileCoordinator readingURL readingOptions writingURL writingOptions outError readerWriter =
  sendMessage nsFileCoordinator coordinateReadingItemAtURL_options_writingItemAtURL_options_error_byAccessorSelector (toNSURL readingURL) readingOptions (toNSURL writingURL) writingOptions (toNSError outError) readerWriter

-- | @- coordinateWritingItemAtURL:options:writingItemAtURL:options:error:byAccessor:@
coordinateWritingItemAtURL_options_writingItemAtURL_options_error_byAccessor :: (IsNSFileCoordinator nsFileCoordinator, IsNSURL url1, IsNSURL url2, IsNSError outError) => nsFileCoordinator -> url1 -> NSFileCoordinatorWritingOptions -> url2 -> NSFileCoordinatorWritingOptions -> outError -> Ptr () -> IO ()
coordinateWritingItemAtURL_options_writingItemAtURL_options_error_byAccessor nsFileCoordinator url1 options1 url2 options2 outError writer =
  sendMessage nsFileCoordinator coordinateWritingItemAtURL_options_writingItemAtURL_options_error_byAccessorSelector (toNSURL url1) options1 (toNSURL url2) options2 (toNSError outError) writer

-- | @- prepareForReadingItemsAtURLs:options:writingItemsAtURLs:options:error:byAccessor:@
prepareForReadingItemsAtURLs_options_writingItemsAtURLs_options_error_byAccessor :: (IsNSFileCoordinator nsFileCoordinator, IsNSArray readingURLs, IsNSArray writingURLs, IsNSError outError) => nsFileCoordinator -> readingURLs -> NSFileCoordinatorReadingOptions -> writingURLs -> NSFileCoordinatorWritingOptions -> outError -> Ptr () -> IO ()
prepareForReadingItemsAtURLs_options_writingItemsAtURLs_options_error_byAccessor nsFileCoordinator readingURLs readingOptions writingURLs writingOptions outError batchAccessor =
  sendMessage nsFileCoordinator prepareForReadingItemsAtURLs_options_writingItemsAtURLs_options_error_byAccessorSelector (toNSArray readingURLs) readingOptions (toNSArray writingURLs) writingOptions (toNSError outError) batchAccessor

-- | @- itemAtURL:willMoveToURL:@
itemAtURL_willMoveToURL :: (IsNSFileCoordinator nsFileCoordinator, IsNSURL oldURL, IsNSURL newURL) => nsFileCoordinator -> oldURL -> newURL -> IO ()
itemAtURL_willMoveToURL nsFileCoordinator oldURL newURL =
  sendMessage nsFileCoordinator itemAtURL_willMoveToURLSelector (toNSURL oldURL) (toNSURL newURL)

-- | @- itemAtURL:didMoveToURL:@
itemAtURL_didMoveToURL :: (IsNSFileCoordinator nsFileCoordinator, IsNSURL oldURL, IsNSURL newURL) => nsFileCoordinator -> oldURL -> newURL -> IO ()
itemAtURL_didMoveToURL nsFileCoordinator oldURL newURL =
  sendMessage nsFileCoordinator itemAtURL_didMoveToURLSelector (toNSURL oldURL) (toNSURL newURL)

-- | @- itemAtURL:didChangeUbiquityAttributes:@
itemAtURL_didChangeUbiquityAttributes :: (IsNSFileCoordinator nsFileCoordinator, IsNSURL url, IsNSSet attributes) => nsFileCoordinator -> url -> attributes -> IO ()
itemAtURL_didChangeUbiquityAttributes nsFileCoordinator url attributes =
  sendMessage nsFileCoordinator itemAtURL_didChangeUbiquityAttributesSelector (toNSURL url) (toNSSet attributes)

-- | @- cancel@
cancel :: IsNSFileCoordinator nsFileCoordinator => nsFileCoordinator -> IO ()
cancel nsFileCoordinator =
  sendMessage nsFileCoordinator cancelSelector

-- | @+ filePresenters@
filePresenters :: IO (Id NSArray)
filePresenters  =
  do
    cls' <- getRequiredClass "NSFileCoordinator"
    sendClassMessage cls' filePresentersSelector

-- | @- purposeIdentifier@
purposeIdentifier :: IsNSFileCoordinator nsFileCoordinator => nsFileCoordinator -> IO (Id NSString)
purposeIdentifier nsFileCoordinator =
  sendMessage nsFileCoordinator purposeIdentifierSelector

-- | @- setPurposeIdentifier:@
setPurposeIdentifier :: (IsNSFileCoordinator nsFileCoordinator, IsNSString value) => nsFileCoordinator -> value -> IO ()
setPurposeIdentifier nsFileCoordinator value =
  sendMessage nsFileCoordinator setPurposeIdentifierSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addFilePresenter:@
addFilePresenterSelector :: Selector '[RawId] ()
addFilePresenterSelector = mkSelector "addFilePresenter:"

-- | @Selector@ for @removeFilePresenter:@
removeFilePresenterSelector :: Selector '[RawId] ()
removeFilePresenterSelector = mkSelector "removeFilePresenter:"

-- | @Selector@ for @initWithFilePresenter:@
initWithFilePresenterSelector :: Selector '[RawId] (Id NSFileCoordinator)
initWithFilePresenterSelector = mkSelector "initWithFilePresenter:"

-- | @Selector@ for @coordinateAccessWithIntents:queue:byAccessor:@
coordinateAccessWithIntents_queue_byAccessorSelector :: Selector '[Id NSArray, Id NSOperationQueue, Ptr ()] ()
coordinateAccessWithIntents_queue_byAccessorSelector = mkSelector "coordinateAccessWithIntents:queue:byAccessor:"

-- | @Selector@ for @coordinateReadingItemAtURL:options:error:byAccessor:@
coordinateReadingItemAtURL_options_error_byAccessorSelector :: Selector '[Id NSURL, NSFileCoordinatorReadingOptions, Id NSError, Ptr ()] ()
coordinateReadingItemAtURL_options_error_byAccessorSelector = mkSelector "coordinateReadingItemAtURL:options:error:byAccessor:"

-- | @Selector@ for @coordinateWritingItemAtURL:options:error:byAccessor:@
coordinateWritingItemAtURL_options_error_byAccessorSelector :: Selector '[Id NSURL, NSFileCoordinatorWritingOptions, Id NSError, Ptr ()] ()
coordinateWritingItemAtURL_options_error_byAccessorSelector = mkSelector "coordinateWritingItemAtURL:options:error:byAccessor:"

-- | @Selector@ for @coordinateReadingItemAtURL:options:writingItemAtURL:options:error:byAccessor:@
coordinateReadingItemAtURL_options_writingItemAtURL_options_error_byAccessorSelector :: Selector '[Id NSURL, NSFileCoordinatorReadingOptions, Id NSURL, NSFileCoordinatorWritingOptions, Id NSError, Ptr ()] ()
coordinateReadingItemAtURL_options_writingItemAtURL_options_error_byAccessorSelector = mkSelector "coordinateReadingItemAtURL:options:writingItemAtURL:options:error:byAccessor:"

-- | @Selector@ for @coordinateWritingItemAtURL:options:writingItemAtURL:options:error:byAccessor:@
coordinateWritingItemAtURL_options_writingItemAtURL_options_error_byAccessorSelector :: Selector '[Id NSURL, NSFileCoordinatorWritingOptions, Id NSURL, NSFileCoordinatorWritingOptions, Id NSError, Ptr ()] ()
coordinateWritingItemAtURL_options_writingItemAtURL_options_error_byAccessorSelector = mkSelector "coordinateWritingItemAtURL:options:writingItemAtURL:options:error:byAccessor:"

-- | @Selector@ for @prepareForReadingItemsAtURLs:options:writingItemsAtURLs:options:error:byAccessor:@
prepareForReadingItemsAtURLs_options_writingItemsAtURLs_options_error_byAccessorSelector :: Selector '[Id NSArray, NSFileCoordinatorReadingOptions, Id NSArray, NSFileCoordinatorWritingOptions, Id NSError, Ptr ()] ()
prepareForReadingItemsAtURLs_options_writingItemsAtURLs_options_error_byAccessorSelector = mkSelector "prepareForReadingItemsAtURLs:options:writingItemsAtURLs:options:error:byAccessor:"

-- | @Selector@ for @itemAtURL:willMoveToURL:@
itemAtURL_willMoveToURLSelector :: Selector '[Id NSURL, Id NSURL] ()
itemAtURL_willMoveToURLSelector = mkSelector "itemAtURL:willMoveToURL:"

-- | @Selector@ for @itemAtURL:didMoveToURL:@
itemAtURL_didMoveToURLSelector :: Selector '[Id NSURL, Id NSURL] ()
itemAtURL_didMoveToURLSelector = mkSelector "itemAtURL:didMoveToURL:"

-- | @Selector@ for @itemAtURL:didChangeUbiquityAttributes:@
itemAtURL_didChangeUbiquityAttributesSelector :: Selector '[Id NSURL, Id NSSet] ()
itemAtURL_didChangeUbiquityAttributesSelector = mkSelector "itemAtURL:didChangeUbiquityAttributes:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @filePresenters@
filePresentersSelector :: Selector '[] (Id NSArray)
filePresentersSelector = mkSelector "filePresenters"

-- | @Selector@ for @purposeIdentifier@
purposeIdentifierSelector :: Selector '[] (Id NSString)
purposeIdentifierSelector = mkSelector "purposeIdentifier"

-- | @Selector@ for @setPurposeIdentifier:@
setPurposeIdentifierSelector :: Selector '[Id NSString] ()
setPurposeIdentifierSelector = mkSelector "setPurposeIdentifier:"

