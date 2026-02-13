{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , persistentIdentifier
  , conflict
  , resolved
  , setResolved
  , discardable
  , setDiscardable
  , hasLocalContents
  , hasThumbnail
  , addVersionOfItemAtURL_withContentsOfURL_options_errorSelector
  , conflictSelector
  , currentVersionOfItemAtURLSelector
  , discardableSelector
  , hasLocalContentsSelector
  , hasThumbnailSelector
  , localizedNameOfSavingComputerSelector
  , localizedNameSelector
  , modificationDateSelector
  , originatorNameComponentsSelector
  , otherVersionsOfItemAtURLSelector
  , persistentIdentifierSelector
  , removeAndReturnErrorSelector
  , removeOtherVersionsOfItemAtURL_errorSelector
  , replaceItemAtURL_options_errorSelector
  , resolvedSelector
  , setDiscardableSelector
  , setResolvedSelector
  , temporaryDirectoryURLForNewVersionOfItemAtURLSelector
  , unresolvedConflictVersionsOfItemAtURLSelector
  , urlSelector
  , versionOfItemAtURL_forPersistentIdentifierSelector

  -- * Enum types
  , NSFileVersionAddingOptions(NSFileVersionAddingOptions)
  , pattern NSFileVersionAddingByMoving
  , NSFileVersionReplacingOptions(NSFileVersionReplacingOptions)
  , pattern NSFileVersionReplacingByMoving

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @+ currentVersionOfItemAtURL:@
currentVersionOfItemAtURL :: IsNSURL url => url -> IO (Id NSFileVersion)
currentVersionOfItemAtURL url =
  do
    cls' <- getRequiredClass "NSFileVersion"
    sendClassMessage cls' currentVersionOfItemAtURLSelector (toNSURL url)

-- | @+ otherVersionsOfItemAtURL:@
otherVersionsOfItemAtURL :: IsNSURL url => url -> IO (Id NSArray)
otherVersionsOfItemAtURL url =
  do
    cls' <- getRequiredClass "NSFileVersion"
    sendClassMessage cls' otherVersionsOfItemAtURLSelector (toNSURL url)

-- | @+ unresolvedConflictVersionsOfItemAtURL:@
unresolvedConflictVersionsOfItemAtURL :: IsNSURL url => url -> IO (Id NSArray)
unresolvedConflictVersionsOfItemAtURL url =
  do
    cls' <- getRequiredClass "NSFileVersion"
    sendClassMessage cls' unresolvedConflictVersionsOfItemAtURLSelector (toNSURL url)

-- | @+ versionOfItemAtURL:forPersistentIdentifier:@
versionOfItemAtURL_forPersistentIdentifier :: IsNSURL url => url -> RawId -> IO (Id NSFileVersion)
versionOfItemAtURL_forPersistentIdentifier url persistentIdentifier =
  do
    cls' <- getRequiredClass "NSFileVersion"
    sendClassMessage cls' versionOfItemAtURL_forPersistentIdentifierSelector (toNSURL url) persistentIdentifier

-- | @+ addVersionOfItemAtURL:withContentsOfURL:options:error:@
addVersionOfItemAtURL_withContentsOfURL_options_error :: (IsNSURL url, IsNSURL contentsURL, IsNSError outError) => url -> contentsURL -> NSFileVersionAddingOptions -> outError -> IO (Id NSFileVersion)
addVersionOfItemAtURL_withContentsOfURL_options_error url contentsURL options outError =
  do
    cls' <- getRequiredClass "NSFileVersion"
    sendClassMessage cls' addVersionOfItemAtURL_withContentsOfURL_options_errorSelector (toNSURL url) (toNSURL contentsURL) options (toNSError outError)

-- | @+ temporaryDirectoryURLForNewVersionOfItemAtURL:@
temporaryDirectoryURLForNewVersionOfItemAtURL :: IsNSURL url => url -> IO (Id NSURL)
temporaryDirectoryURLForNewVersionOfItemAtURL url =
  do
    cls' <- getRequiredClass "NSFileVersion"
    sendClassMessage cls' temporaryDirectoryURLForNewVersionOfItemAtURLSelector (toNSURL url)

-- | @- replaceItemAtURL:options:error:@
replaceItemAtURL_options_error :: (IsNSFileVersion nsFileVersion, IsNSURL url, IsNSError error_) => nsFileVersion -> url -> NSFileVersionReplacingOptions -> error_ -> IO (Id NSURL)
replaceItemAtURL_options_error nsFileVersion url options error_ =
  sendMessage nsFileVersion replaceItemAtURL_options_errorSelector (toNSURL url) options (toNSError error_)

-- | @- removeAndReturnError:@
removeAndReturnError :: (IsNSFileVersion nsFileVersion, IsNSError outError) => nsFileVersion -> outError -> IO Bool
removeAndReturnError nsFileVersion outError =
  sendMessage nsFileVersion removeAndReturnErrorSelector (toNSError outError)

-- | @+ removeOtherVersionsOfItemAtURL:error:@
removeOtherVersionsOfItemAtURL_error :: (IsNSURL url, IsNSError outError) => url -> outError -> IO Bool
removeOtherVersionsOfItemAtURL_error url outError =
  do
    cls' <- getRequiredClass "NSFileVersion"
    sendClassMessage cls' removeOtherVersionsOfItemAtURL_errorSelector (toNSURL url) (toNSError outError)

-- | @- URL@
url :: IsNSFileVersion nsFileVersion => nsFileVersion -> IO (Id NSURL)
url nsFileVersion =
  sendMessage nsFileVersion urlSelector

-- | @- localizedName@
localizedName :: IsNSFileVersion nsFileVersion => nsFileVersion -> IO (Id NSString)
localizedName nsFileVersion =
  sendMessage nsFileVersion localizedNameSelector

-- | @- localizedNameOfSavingComputer@
localizedNameOfSavingComputer :: IsNSFileVersion nsFileVersion => nsFileVersion -> IO (Id NSString)
localizedNameOfSavingComputer nsFileVersion =
  sendMessage nsFileVersion localizedNameOfSavingComputerSelector

-- | @- originatorNameComponents@
originatorNameComponents :: IsNSFileVersion nsFileVersion => nsFileVersion -> IO (Id NSPersonNameComponents)
originatorNameComponents nsFileVersion =
  sendMessage nsFileVersion originatorNameComponentsSelector

-- | @- modificationDate@
modificationDate :: IsNSFileVersion nsFileVersion => nsFileVersion -> IO (Id NSDate)
modificationDate nsFileVersion =
  sendMessage nsFileVersion modificationDateSelector

-- | @- persistentIdentifier@
persistentIdentifier :: IsNSFileVersion nsFileVersion => nsFileVersion -> IO RawId
persistentIdentifier nsFileVersion =
  sendMessage nsFileVersion persistentIdentifierSelector

-- | @- conflict@
conflict :: IsNSFileVersion nsFileVersion => nsFileVersion -> IO Bool
conflict nsFileVersion =
  sendMessage nsFileVersion conflictSelector

-- | @- resolved@
resolved :: IsNSFileVersion nsFileVersion => nsFileVersion -> IO Bool
resolved nsFileVersion =
  sendMessage nsFileVersion resolvedSelector

-- | @- setResolved:@
setResolved :: IsNSFileVersion nsFileVersion => nsFileVersion -> Bool -> IO ()
setResolved nsFileVersion value =
  sendMessage nsFileVersion setResolvedSelector value

-- | @- discardable@
discardable :: IsNSFileVersion nsFileVersion => nsFileVersion -> IO Bool
discardable nsFileVersion =
  sendMessage nsFileVersion discardableSelector

-- | @- setDiscardable:@
setDiscardable :: IsNSFileVersion nsFileVersion => nsFileVersion -> Bool -> IO ()
setDiscardable nsFileVersion value =
  sendMessage nsFileVersion setDiscardableSelector value

-- | @- hasLocalContents@
hasLocalContents :: IsNSFileVersion nsFileVersion => nsFileVersion -> IO Bool
hasLocalContents nsFileVersion =
  sendMessage nsFileVersion hasLocalContentsSelector

-- | @- hasThumbnail@
hasThumbnail :: IsNSFileVersion nsFileVersion => nsFileVersion -> IO Bool
hasThumbnail nsFileVersion =
  sendMessage nsFileVersion hasThumbnailSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentVersionOfItemAtURL:@
currentVersionOfItemAtURLSelector :: Selector '[Id NSURL] (Id NSFileVersion)
currentVersionOfItemAtURLSelector = mkSelector "currentVersionOfItemAtURL:"

-- | @Selector@ for @otherVersionsOfItemAtURL:@
otherVersionsOfItemAtURLSelector :: Selector '[Id NSURL] (Id NSArray)
otherVersionsOfItemAtURLSelector = mkSelector "otherVersionsOfItemAtURL:"

-- | @Selector@ for @unresolvedConflictVersionsOfItemAtURL:@
unresolvedConflictVersionsOfItemAtURLSelector :: Selector '[Id NSURL] (Id NSArray)
unresolvedConflictVersionsOfItemAtURLSelector = mkSelector "unresolvedConflictVersionsOfItemAtURL:"

-- | @Selector@ for @versionOfItemAtURL:forPersistentIdentifier:@
versionOfItemAtURL_forPersistentIdentifierSelector :: Selector '[Id NSURL, RawId] (Id NSFileVersion)
versionOfItemAtURL_forPersistentIdentifierSelector = mkSelector "versionOfItemAtURL:forPersistentIdentifier:"

-- | @Selector@ for @addVersionOfItemAtURL:withContentsOfURL:options:error:@
addVersionOfItemAtURL_withContentsOfURL_options_errorSelector :: Selector '[Id NSURL, Id NSURL, NSFileVersionAddingOptions, Id NSError] (Id NSFileVersion)
addVersionOfItemAtURL_withContentsOfURL_options_errorSelector = mkSelector "addVersionOfItemAtURL:withContentsOfURL:options:error:"

-- | @Selector@ for @temporaryDirectoryURLForNewVersionOfItemAtURL:@
temporaryDirectoryURLForNewVersionOfItemAtURLSelector :: Selector '[Id NSURL] (Id NSURL)
temporaryDirectoryURLForNewVersionOfItemAtURLSelector = mkSelector "temporaryDirectoryURLForNewVersionOfItemAtURL:"

-- | @Selector@ for @replaceItemAtURL:options:error:@
replaceItemAtURL_options_errorSelector :: Selector '[Id NSURL, NSFileVersionReplacingOptions, Id NSError] (Id NSURL)
replaceItemAtURL_options_errorSelector = mkSelector "replaceItemAtURL:options:error:"

-- | @Selector@ for @removeAndReturnError:@
removeAndReturnErrorSelector :: Selector '[Id NSError] Bool
removeAndReturnErrorSelector = mkSelector "removeAndReturnError:"

-- | @Selector@ for @removeOtherVersionsOfItemAtURL:error:@
removeOtherVersionsOfItemAtURL_errorSelector :: Selector '[Id NSURL, Id NSError] Bool
removeOtherVersionsOfItemAtURL_errorSelector = mkSelector "removeOtherVersionsOfItemAtURL:error:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector '[] (Id NSString)
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @localizedNameOfSavingComputer@
localizedNameOfSavingComputerSelector :: Selector '[] (Id NSString)
localizedNameOfSavingComputerSelector = mkSelector "localizedNameOfSavingComputer"

-- | @Selector@ for @originatorNameComponents@
originatorNameComponentsSelector :: Selector '[] (Id NSPersonNameComponents)
originatorNameComponentsSelector = mkSelector "originatorNameComponents"

-- | @Selector@ for @modificationDate@
modificationDateSelector :: Selector '[] (Id NSDate)
modificationDateSelector = mkSelector "modificationDate"

-- | @Selector@ for @persistentIdentifier@
persistentIdentifierSelector :: Selector '[] RawId
persistentIdentifierSelector = mkSelector "persistentIdentifier"

-- | @Selector@ for @conflict@
conflictSelector :: Selector '[] Bool
conflictSelector = mkSelector "conflict"

-- | @Selector@ for @resolved@
resolvedSelector :: Selector '[] Bool
resolvedSelector = mkSelector "resolved"

-- | @Selector@ for @setResolved:@
setResolvedSelector :: Selector '[Bool] ()
setResolvedSelector = mkSelector "setResolved:"

-- | @Selector@ for @discardable@
discardableSelector :: Selector '[] Bool
discardableSelector = mkSelector "discardable"

-- | @Selector@ for @setDiscardable:@
setDiscardableSelector :: Selector '[Bool] ()
setDiscardableSelector = mkSelector "setDiscardable:"

-- | @Selector@ for @hasLocalContents@
hasLocalContentsSelector :: Selector '[] Bool
hasLocalContentsSelector = mkSelector "hasLocalContents"

-- | @Selector@ for @hasThumbnail@
hasThumbnailSelector :: Selector '[] Bool
hasThumbnailSelector = mkSelector "hasThumbnail"

