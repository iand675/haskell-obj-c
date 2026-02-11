{-# LANGUAGE PatternSynonyms #-}
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
  , removeFilePresenterSelector
  , initWithFilePresenterSelector
  , coordinateAccessWithIntents_queue_byAccessorSelector
  , coordinateReadingItemAtURL_options_error_byAccessorSelector
  , coordinateWritingItemAtURL_options_error_byAccessorSelector
  , coordinateReadingItemAtURL_options_writingItemAtURL_options_error_byAccessorSelector
  , coordinateWritingItemAtURL_options_writingItemAtURL_options_error_byAccessorSelector
  , prepareForReadingItemsAtURLs_options_writingItemsAtURLs_options_error_byAccessorSelector
  , itemAtURL_willMoveToURLSelector
  , itemAtURL_didMoveToURLSelector
  , itemAtURL_didChangeUbiquityAttributesSelector
  , cancelSelector
  , filePresentersSelector
  , purposeIdentifierSelector
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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @+ addFilePresenter:@
addFilePresenter :: RawId -> IO ()
addFilePresenter filePresenter =
  do
    cls' <- getRequiredClass "NSFileCoordinator"
    sendClassMsg cls' (mkSelector "addFilePresenter:") retVoid [argPtr (castPtr (unRawId filePresenter) :: Ptr ())]

-- | @+ removeFilePresenter:@
removeFilePresenter :: RawId -> IO ()
removeFilePresenter filePresenter =
  do
    cls' <- getRequiredClass "NSFileCoordinator"
    sendClassMsg cls' (mkSelector "removeFilePresenter:") retVoid [argPtr (castPtr (unRawId filePresenter) :: Ptr ())]

-- | @- initWithFilePresenter:@
initWithFilePresenter :: IsNSFileCoordinator nsFileCoordinator => nsFileCoordinator -> RawId -> IO (Id NSFileCoordinator)
initWithFilePresenter nsFileCoordinator  filePresenterOrNil =
    sendMsg nsFileCoordinator (mkSelector "initWithFilePresenter:") (retPtr retVoid) [argPtr (castPtr (unRawId filePresenterOrNil) :: Ptr ())] >>= ownedObject . castPtr

-- | @- coordinateAccessWithIntents:queue:byAccessor:@
coordinateAccessWithIntents_queue_byAccessor :: (IsNSFileCoordinator nsFileCoordinator, IsNSArray intents, IsNSOperationQueue queue) => nsFileCoordinator -> intents -> queue -> Ptr () -> IO ()
coordinateAccessWithIntents_queue_byAccessor nsFileCoordinator  intents queue accessor =
  withObjCPtr intents $ \raw_intents ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg nsFileCoordinator (mkSelector "coordinateAccessWithIntents:queue:byAccessor:") retVoid [argPtr (castPtr raw_intents :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr accessor :: Ptr ())]

-- | @- coordinateReadingItemAtURL:options:error:byAccessor:@
coordinateReadingItemAtURL_options_error_byAccessor :: (IsNSFileCoordinator nsFileCoordinator, IsNSURL url, IsNSError outError) => nsFileCoordinator -> url -> NSFileCoordinatorReadingOptions -> outError -> Ptr () -> IO ()
coordinateReadingItemAtURL_options_error_byAccessor nsFileCoordinator  url options outError reader =
  withObjCPtr url $ \raw_url ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg nsFileCoordinator (mkSelector "coordinateReadingItemAtURL:options:error:byAccessor:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_outError :: Ptr ()), argPtr (castPtr reader :: Ptr ())]

-- | @- coordinateWritingItemAtURL:options:error:byAccessor:@
coordinateWritingItemAtURL_options_error_byAccessor :: (IsNSFileCoordinator nsFileCoordinator, IsNSURL url, IsNSError outError) => nsFileCoordinator -> url -> NSFileCoordinatorWritingOptions -> outError -> Ptr () -> IO ()
coordinateWritingItemAtURL_options_error_byAccessor nsFileCoordinator  url options outError writer =
  withObjCPtr url $ \raw_url ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg nsFileCoordinator (mkSelector "coordinateWritingItemAtURL:options:error:byAccessor:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_outError :: Ptr ()), argPtr (castPtr writer :: Ptr ())]

-- | @- coordinateReadingItemAtURL:options:writingItemAtURL:options:error:byAccessor:@
coordinateReadingItemAtURL_options_writingItemAtURL_options_error_byAccessor :: (IsNSFileCoordinator nsFileCoordinator, IsNSURL readingURL, IsNSURL writingURL, IsNSError outError) => nsFileCoordinator -> readingURL -> NSFileCoordinatorReadingOptions -> writingURL -> NSFileCoordinatorWritingOptions -> outError -> Ptr () -> IO ()
coordinateReadingItemAtURL_options_writingItemAtURL_options_error_byAccessor nsFileCoordinator  readingURL readingOptions writingURL writingOptions outError readerWriter =
  withObjCPtr readingURL $ \raw_readingURL ->
    withObjCPtr writingURL $ \raw_writingURL ->
      withObjCPtr outError $ \raw_outError ->
          sendMsg nsFileCoordinator (mkSelector "coordinateReadingItemAtURL:options:writingItemAtURL:options:error:byAccessor:") retVoid [argPtr (castPtr raw_readingURL :: Ptr ()), argCULong (coerce readingOptions), argPtr (castPtr raw_writingURL :: Ptr ()), argCULong (coerce writingOptions), argPtr (castPtr raw_outError :: Ptr ()), argPtr (castPtr readerWriter :: Ptr ())]

-- | @- coordinateWritingItemAtURL:options:writingItemAtURL:options:error:byAccessor:@
coordinateWritingItemAtURL_options_writingItemAtURL_options_error_byAccessor :: (IsNSFileCoordinator nsFileCoordinator, IsNSURL url1, IsNSURL url2, IsNSError outError) => nsFileCoordinator -> url1 -> NSFileCoordinatorWritingOptions -> url2 -> NSFileCoordinatorWritingOptions -> outError -> Ptr () -> IO ()
coordinateWritingItemAtURL_options_writingItemAtURL_options_error_byAccessor nsFileCoordinator  url1 options1 url2 options2 outError writer =
  withObjCPtr url1 $ \raw_url1 ->
    withObjCPtr url2 $ \raw_url2 ->
      withObjCPtr outError $ \raw_outError ->
          sendMsg nsFileCoordinator (mkSelector "coordinateWritingItemAtURL:options:writingItemAtURL:options:error:byAccessor:") retVoid [argPtr (castPtr raw_url1 :: Ptr ()), argCULong (coerce options1), argPtr (castPtr raw_url2 :: Ptr ()), argCULong (coerce options2), argPtr (castPtr raw_outError :: Ptr ()), argPtr (castPtr writer :: Ptr ())]

-- | @- prepareForReadingItemsAtURLs:options:writingItemsAtURLs:options:error:byAccessor:@
prepareForReadingItemsAtURLs_options_writingItemsAtURLs_options_error_byAccessor :: (IsNSFileCoordinator nsFileCoordinator, IsNSArray readingURLs, IsNSArray writingURLs, IsNSError outError) => nsFileCoordinator -> readingURLs -> NSFileCoordinatorReadingOptions -> writingURLs -> NSFileCoordinatorWritingOptions -> outError -> Ptr () -> IO ()
prepareForReadingItemsAtURLs_options_writingItemsAtURLs_options_error_byAccessor nsFileCoordinator  readingURLs readingOptions writingURLs writingOptions outError batchAccessor =
  withObjCPtr readingURLs $ \raw_readingURLs ->
    withObjCPtr writingURLs $ \raw_writingURLs ->
      withObjCPtr outError $ \raw_outError ->
          sendMsg nsFileCoordinator (mkSelector "prepareForReadingItemsAtURLs:options:writingItemsAtURLs:options:error:byAccessor:") retVoid [argPtr (castPtr raw_readingURLs :: Ptr ()), argCULong (coerce readingOptions), argPtr (castPtr raw_writingURLs :: Ptr ()), argCULong (coerce writingOptions), argPtr (castPtr raw_outError :: Ptr ()), argPtr (castPtr batchAccessor :: Ptr ())]

-- | @- itemAtURL:willMoveToURL:@
itemAtURL_willMoveToURL :: (IsNSFileCoordinator nsFileCoordinator, IsNSURL oldURL, IsNSURL newURL) => nsFileCoordinator -> oldURL -> newURL -> IO ()
itemAtURL_willMoveToURL nsFileCoordinator  oldURL newURL =
  withObjCPtr oldURL $ \raw_oldURL ->
    withObjCPtr newURL $ \raw_newURL ->
        sendMsg nsFileCoordinator (mkSelector "itemAtURL:willMoveToURL:") retVoid [argPtr (castPtr raw_oldURL :: Ptr ()), argPtr (castPtr raw_newURL :: Ptr ())]

-- | @- itemAtURL:didMoveToURL:@
itemAtURL_didMoveToURL :: (IsNSFileCoordinator nsFileCoordinator, IsNSURL oldURL, IsNSURL newURL) => nsFileCoordinator -> oldURL -> newURL -> IO ()
itemAtURL_didMoveToURL nsFileCoordinator  oldURL newURL =
  withObjCPtr oldURL $ \raw_oldURL ->
    withObjCPtr newURL $ \raw_newURL ->
        sendMsg nsFileCoordinator (mkSelector "itemAtURL:didMoveToURL:") retVoid [argPtr (castPtr raw_oldURL :: Ptr ()), argPtr (castPtr raw_newURL :: Ptr ())]

-- | @- itemAtURL:didChangeUbiquityAttributes:@
itemAtURL_didChangeUbiquityAttributes :: (IsNSFileCoordinator nsFileCoordinator, IsNSURL url, IsNSSet attributes) => nsFileCoordinator -> url -> attributes -> IO ()
itemAtURL_didChangeUbiquityAttributes nsFileCoordinator  url attributes =
  withObjCPtr url $ \raw_url ->
    withObjCPtr attributes $ \raw_attributes ->
        sendMsg nsFileCoordinator (mkSelector "itemAtURL:didChangeUbiquityAttributes:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_attributes :: Ptr ())]

-- | @- cancel@
cancel :: IsNSFileCoordinator nsFileCoordinator => nsFileCoordinator -> IO ()
cancel nsFileCoordinator  =
    sendMsg nsFileCoordinator (mkSelector "cancel") retVoid []

-- | @+ filePresenters@
filePresenters :: IO (Id NSArray)
filePresenters  =
  do
    cls' <- getRequiredClass "NSFileCoordinator"
    sendClassMsg cls' (mkSelector "filePresenters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- purposeIdentifier@
purposeIdentifier :: IsNSFileCoordinator nsFileCoordinator => nsFileCoordinator -> IO (Id NSString)
purposeIdentifier nsFileCoordinator  =
    sendMsg nsFileCoordinator (mkSelector "purposeIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPurposeIdentifier:@
setPurposeIdentifier :: (IsNSFileCoordinator nsFileCoordinator, IsNSString value) => nsFileCoordinator -> value -> IO ()
setPurposeIdentifier nsFileCoordinator  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsFileCoordinator (mkSelector "setPurposeIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addFilePresenter:@
addFilePresenterSelector :: Selector
addFilePresenterSelector = mkSelector "addFilePresenter:"

-- | @Selector@ for @removeFilePresenter:@
removeFilePresenterSelector :: Selector
removeFilePresenterSelector = mkSelector "removeFilePresenter:"

-- | @Selector@ for @initWithFilePresenter:@
initWithFilePresenterSelector :: Selector
initWithFilePresenterSelector = mkSelector "initWithFilePresenter:"

-- | @Selector@ for @coordinateAccessWithIntents:queue:byAccessor:@
coordinateAccessWithIntents_queue_byAccessorSelector :: Selector
coordinateAccessWithIntents_queue_byAccessorSelector = mkSelector "coordinateAccessWithIntents:queue:byAccessor:"

-- | @Selector@ for @coordinateReadingItemAtURL:options:error:byAccessor:@
coordinateReadingItemAtURL_options_error_byAccessorSelector :: Selector
coordinateReadingItemAtURL_options_error_byAccessorSelector = mkSelector "coordinateReadingItemAtURL:options:error:byAccessor:"

-- | @Selector@ for @coordinateWritingItemAtURL:options:error:byAccessor:@
coordinateWritingItemAtURL_options_error_byAccessorSelector :: Selector
coordinateWritingItemAtURL_options_error_byAccessorSelector = mkSelector "coordinateWritingItemAtURL:options:error:byAccessor:"

-- | @Selector@ for @coordinateReadingItemAtURL:options:writingItemAtURL:options:error:byAccessor:@
coordinateReadingItemAtURL_options_writingItemAtURL_options_error_byAccessorSelector :: Selector
coordinateReadingItemAtURL_options_writingItemAtURL_options_error_byAccessorSelector = mkSelector "coordinateReadingItemAtURL:options:writingItemAtURL:options:error:byAccessor:"

-- | @Selector@ for @coordinateWritingItemAtURL:options:writingItemAtURL:options:error:byAccessor:@
coordinateWritingItemAtURL_options_writingItemAtURL_options_error_byAccessorSelector :: Selector
coordinateWritingItemAtURL_options_writingItemAtURL_options_error_byAccessorSelector = mkSelector "coordinateWritingItemAtURL:options:writingItemAtURL:options:error:byAccessor:"

-- | @Selector@ for @prepareForReadingItemsAtURLs:options:writingItemsAtURLs:options:error:byAccessor:@
prepareForReadingItemsAtURLs_options_writingItemsAtURLs_options_error_byAccessorSelector :: Selector
prepareForReadingItemsAtURLs_options_writingItemsAtURLs_options_error_byAccessorSelector = mkSelector "prepareForReadingItemsAtURLs:options:writingItemsAtURLs:options:error:byAccessor:"

-- | @Selector@ for @itemAtURL:willMoveToURL:@
itemAtURL_willMoveToURLSelector :: Selector
itemAtURL_willMoveToURLSelector = mkSelector "itemAtURL:willMoveToURL:"

-- | @Selector@ for @itemAtURL:didMoveToURL:@
itemAtURL_didMoveToURLSelector :: Selector
itemAtURL_didMoveToURLSelector = mkSelector "itemAtURL:didMoveToURL:"

-- | @Selector@ for @itemAtURL:didChangeUbiquityAttributes:@
itemAtURL_didChangeUbiquityAttributesSelector :: Selector
itemAtURL_didChangeUbiquityAttributesSelector = mkSelector "itemAtURL:didChangeUbiquityAttributes:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @filePresenters@
filePresentersSelector :: Selector
filePresentersSelector = mkSelector "filePresenters"

-- | @Selector@ for @purposeIdentifier@
purposeIdentifierSelector :: Selector
purposeIdentifierSelector = mkSelector "purposeIdentifier"

-- | @Selector@ for @setPurposeIdentifier:@
setPurposeIdentifierSelector :: Selector
setPurposeIdentifierSelector = mkSelector "setPurposeIdentifier:"

