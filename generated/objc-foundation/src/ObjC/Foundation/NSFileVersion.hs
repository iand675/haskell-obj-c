{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFileVersion@.
module ObjC.Foundation.NSFileVersion
  ( NSFileVersion
  , IsNSFileVersion(..)
  , currentVersionOfItemAtURL
  , otherVersionsOfItemAtURL
  , unresolvedConflictVersionsOfItemAtURL
  , versionOfItemAtURL_forPersistentIdentifier
  , addVersionOfItemAtURL_withContentsOfURL_options_error
  , temporaryDirectoryURLForNewVersionOfItemAtURL
  , replaceItemAtURL_options_error
  , removeAndReturnError
  , removeOtherVersionsOfItemAtURL_error
  , url
  , localizedName
  , localizedNameOfSavingComputer
  , originatorNameComponents
  , modificationDate
  , conflict
  , resolved
  , setResolved
  , discardable
  , setDiscardable
  , hasLocalContents
  , hasThumbnail
  , currentVersionOfItemAtURLSelector
  , otherVersionsOfItemAtURLSelector
  , unresolvedConflictVersionsOfItemAtURLSelector
  , versionOfItemAtURL_forPersistentIdentifierSelector
  , addVersionOfItemAtURL_withContentsOfURL_options_errorSelector
  , temporaryDirectoryURLForNewVersionOfItemAtURLSelector
  , replaceItemAtURL_options_errorSelector
  , removeAndReturnErrorSelector
  , removeOtherVersionsOfItemAtURL_errorSelector
  , urlSelector
  , localizedNameSelector
  , localizedNameOfSavingComputerSelector
  , originatorNameComponentsSelector
  , modificationDateSelector
  , conflictSelector
  , resolvedSelector
  , setResolvedSelector
  , discardableSelector
  , setDiscardableSelector
  , hasLocalContentsSelector
  , hasThumbnailSelector

  -- * Enum types
  , NSFileVersionAddingOptions(NSFileVersionAddingOptions)
  , pattern NSFileVersionAddingByMoving
  , NSFileVersionReplacingOptions(NSFileVersionReplacingOptions)
  , pattern NSFileVersionReplacingByMoving

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

-- | @+ currentVersionOfItemAtURL:@
currentVersionOfItemAtURL :: IsNSURL url => url -> IO (Id NSFileVersion)
currentVersionOfItemAtURL url =
  do
    cls' <- getRequiredClass "NSFileVersion"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "currentVersionOfItemAtURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @+ otherVersionsOfItemAtURL:@
otherVersionsOfItemAtURL :: IsNSURL url => url -> IO (Id NSArray)
otherVersionsOfItemAtURL url =
  do
    cls' <- getRequiredClass "NSFileVersion"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "otherVersionsOfItemAtURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @+ unresolvedConflictVersionsOfItemAtURL:@
unresolvedConflictVersionsOfItemAtURL :: IsNSURL url => url -> IO (Id NSArray)
unresolvedConflictVersionsOfItemAtURL url =
  do
    cls' <- getRequiredClass "NSFileVersion"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "unresolvedConflictVersionsOfItemAtURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @+ versionOfItemAtURL:forPersistentIdentifier:@
versionOfItemAtURL_forPersistentIdentifier :: IsNSURL url => url -> RawId -> IO (Id NSFileVersion)
versionOfItemAtURL_forPersistentIdentifier url persistentIdentifier =
  do
    cls' <- getRequiredClass "NSFileVersion"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "versionOfItemAtURL:forPersistentIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr (unRawId persistentIdentifier) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ addVersionOfItemAtURL:withContentsOfURL:options:error:@
addVersionOfItemAtURL_withContentsOfURL_options_error :: (IsNSURL url, IsNSURL contentsURL, IsNSError outError) => url -> contentsURL -> NSFileVersionAddingOptions -> outError -> IO (Id NSFileVersion)
addVersionOfItemAtURL_withContentsOfURL_options_error url contentsURL options outError =
  do
    cls' <- getRequiredClass "NSFileVersion"
    withObjCPtr url $ \raw_url ->
      withObjCPtr contentsURL $ \raw_contentsURL ->
        withObjCPtr outError $ \raw_outError ->
          sendClassMsg cls' (mkSelector "addVersionOfItemAtURL:withContentsOfURL:options:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_contentsURL :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | @+ temporaryDirectoryURLForNewVersionOfItemAtURL:@
temporaryDirectoryURLForNewVersionOfItemAtURL :: IsNSURL url => url -> IO (Id NSURL)
temporaryDirectoryURLForNewVersionOfItemAtURL url =
  do
    cls' <- getRequiredClass "NSFileVersion"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "temporaryDirectoryURLForNewVersionOfItemAtURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @- replaceItemAtURL:options:error:@
replaceItemAtURL_options_error :: (IsNSFileVersion nsFileVersion, IsNSURL url, IsNSError error_) => nsFileVersion -> url -> NSFileVersionReplacingOptions -> error_ -> IO (Id NSURL)
replaceItemAtURL_options_error nsFileVersion  url options error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsFileVersion (mkSelector "replaceItemAtURL:options:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- removeAndReturnError:@
removeAndReturnError :: (IsNSFileVersion nsFileVersion, IsNSError outError) => nsFileVersion -> outError -> IO Bool
removeAndReturnError nsFileVersion  outError =
withObjCPtr outError $ \raw_outError ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileVersion (mkSelector "removeAndReturnError:") retCULong [argPtr (castPtr raw_outError :: Ptr ())]

-- | @+ removeOtherVersionsOfItemAtURL:error:@
removeOtherVersionsOfItemAtURL_error :: (IsNSURL url, IsNSError outError) => url -> outError -> IO Bool
removeOtherVersionsOfItemAtURL_error url outError =
  do
    cls' <- getRequiredClass "NSFileVersion"
    withObjCPtr url $ \raw_url ->
      withObjCPtr outError $ \raw_outError ->
        fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "removeOtherVersionsOfItemAtURL:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- URL@
url :: IsNSFileVersion nsFileVersion => nsFileVersion -> IO (Id NSURL)
url nsFileVersion  =
  sendMsg nsFileVersion (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localizedName@
localizedName :: IsNSFileVersion nsFileVersion => nsFileVersion -> IO (Id NSString)
localizedName nsFileVersion  =
  sendMsg nsFileVersion (mkSelector "localizedName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localizedNameOfSavingComputer@
localizedNameOfSavingComputer :: IsNSFileVersion nsFileVersion => nsFileVersion -> IO (Id NSString)
localizedNameOfSavingComputer nsFileVersion  =
  sendMsg nsFileVersion (mkSelector "localizedNameOfSavingComputer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- originatorNameComponents@
originatorNameComponents :: IsNSFileVersion nsFileVersion => nsFileVersion -> IO (Id NSPersonNameComponents)
originatorNameComponents nsFileVersion  =
  sendMsg nsFileVersion (mkSelector "originatorNameComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- modificationDate@
modificationDate :: IsNSFileVersion nsFileVersion => nsFileVersion -> IO (Id NSDate)
modificationDate nsFileVersion  =
  sendMsg nsFileVersion (mkSelector "modificationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- conflict@
conflict :: IsNSFileVersion nsFileVersion => nsFileVersion -> IO Bool
conflict nsFileVersion  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileVersion (mkSelector "conflict") retCULong []

-- | @- resolved@
resolved :: IsNSFileVersion nsFileVersion => nsFileVersion -> IO Bool
resolved nsFileVersion  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileVersion (mkSelector "resolved") retCULong []

-- | @- setResolved:@
setResolved :: IsNSFileVersion nsFileVersion => nsFileVersion -> Bool -> IO ()
setResolved nsFileVersion  value =
  sendMsg nsFileVersion (mkSelector "setResolved:") retVoid [argCULong (if value then 1 else 0)]

-- | @- discardable@
discardable :: IsNSFileVersion nsFileVersion => nsFileVersion -> IO Bool
discardable nsFileVersion  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileVersion (mkSelector "discardable") retCULong []

-- | @- setDiscardable:@
setDiscardable :: IsNSFileVersion nsFileVersion => nsFileVersion -> Bool -> IO ()
setDiscardable nsFileVersion  value =
  sendMsg nsFileVersion (mkSelector "setDiscardable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- hasLocalContents@
hasLocalContents :: IsNSFileVersion nsFileVersion => nsFileVersion -> IO Bool
hasLocalContents nsFileVersion  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileVersion (mkSelector "hasLocalContents") retCULong []

-- | @- hasThumbnail@
hasThumbnail :: IsNSFileVersion nsFileVersion => nsFileVersion -> IO Bool
hasThumbnail nsFileVersion  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileVersion (mkSelector "hasThumbnail") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentVersionOfItemAtURL:@
currentVersionOfItemAtURLSelector :: Selector
currentVersionOfItemAtURLSelector = mkSelector "currentVersionOfItemAtURL:"

-- | @Selector@ for @otherVersionsOfItemAtURL:@
otherVersionsOfItemAtURLSelector :: Selector
otherVersionsOfItemAtURLSelector = mkSelector "otherVersionsOfItemAtURL:"

-- | @Selector@ for @unresolvedConflictVersionsOfItemAtURL:@
unresolvedConflictVersionsOfItemAtURLSelector :: Selector
unresolvedConflictVersionsOfItemAtURLSelector = mkSelector "unresolvedConflictVersionsOfItemAtURL:"

-- | @Selector@ for @versionOfItemAtURL:forPersistentIdentifier:@
versionOfItemAtURL_forPersistentIdentifierSelector :: Selector
versionOfItemAtURL_forPersistentIdentifierSelector = mkSelector "versionOfItemAtURL:forPersistentIdentifier:"

-- | @Selector@ for @addVersionOfItemAtURL:withContentsOfURL:options:error:@
addVersionOfItemAtURL_withContentsOfURL_options_errorSelector :: Selector
addVersionOfItemAtURL_withContentsOfURL_options_errorSelector = mkSelector "addVersionOfItemAtURL:withContentsOfURL:options:error:"

-- | @Selector@ for @temporaryDirectoryURLForNewVersionOfItemAtURL:@
temporaryDirectoryURLForNewVersionOfItemAtURLSelector :: Selector
temporaryDirectoryURLForNewVersionOfItemAtURLSelector = mkSelector "temporaryDirectoryURLForNewVersionOfItemAtURL:"

-- | @Selector@ for @replaceItemAtURL:options:error:@
replaceItemAtURL_options_errorSelector :: Selector
replaceItemAtURL_options_errorSelector = mkSelector "replaceItemAtURL:options:error:"

-- | @Selector@ for @removeAndReturnError:@
removeAndReturnErrorSelector :: Selector
removeAndReturnErrorSelector = mkSelector "removeAndReturnError:"

-- | @Selector@ for @removeOtherVersionsOfItemAtURL:error:@
removeOtherVersionsOfItemAtURL_errorSelector :: Selector
removeOtherVersionsOfItemAtURL_errorSelector = mkSelector "removeOtherVersionsOfItemAtURL:error:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @localizedNameOfSavingComputer@
localizedNameOfSavingComputerSelector :: Selector
localizedNameOfSavingComputerSelector = mkSelector "localizedNameOfSavingComputer"

-- | @Selector@ for @originatorNameComponents@
originatorNameComponentsSelector :: Selector
originatorNameComponentsSelector = mkSelector "originatorNameComponents"

-- | @Selector@ for @modificationDate@
modificationDateSelector :: Selector
modificationDateSelector = mkSelector "modificationDate"

-- | @Selector@ for @conflict@
conflictSelector :: Selector
conflictSelector = mkSelector "conflict"

-- | @Selector@ for @resolved@
resolvedSelector :: Selector
resolvedSelector = mkSelector "resolved"

-- | @Selector@ for @setResolved:@
setResolvedSelector :: Selector
setResolvedSelector = mkSelector "setResolved:"

-- | @Selector@ for @discardable@
discardableSelector :: Selector
discardableSelector = mkSelector "discardable"

-- | @Selector@ for @setDiscardable:@
setDiscardableSelector :: Selector
setDiscardableSelector = mkSelector "setDiscardable:"

-- | @Selector@ for @hasLocalContents@
hasLocalContentsSelector :: Selector
hasLocalContentsSelector = mkSelector "hasLocalContents"

-- | @Selector@ for @hasThumbnail@
hasThumbnailSelector :: Selector
hasThumbnailSelector = mkSelector "hasThumbnail"

