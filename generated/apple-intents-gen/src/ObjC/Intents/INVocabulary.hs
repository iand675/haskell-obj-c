{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INVocabulary@.
module ObjC.Intents.INVocabulary
  ( INVocabulary
  , IsINVocabulary(..)
  , sharedVocabulary
  , setVocabularyStrings_ofType
  , setVocabulary_ofType
  , removeAllVocabularyStrings
  , removeAllVocabularyStringsSelector
  , setVocabularyStrings_ofTypeSelector
  , setVocabulary_ofTypeSelector
  , sharedVocabularySelector

  -- * Enum types
  , INVocabularyStringType(INVocabularyStringType)
  , pattern INVocabularyStringTypeContactName
  , pattern INVocabularyStringTypeContactGroupName
  , pattern INVocabularyStringTypePhotoTag
  , pattern INVocabularyStringTypePhotoAlbumName
  , pattern INVocabularyStringTypeWorkoutActivityName
  , pattern INVocabularyStringTypeCarProfileName
  , pattern INVocabularyStringTypeCarName
  , pattern INVocabularyStringTypePaymentsOrganizationName
  , pattern INVocabularyStringTypePaymentsAccountNickname
  , pattern INVocabularyStringTypeNotebookItemTitle
  , pattern INVocabularyStringTypeNotebookItemGroupName
  , pattern INVocabularyStringTypeMediaPlaylistTitle
  , pattern INVocabularyStringTypeMediaMusicArtistName
  , pattern INVocabularyStringTypeMediaAudiobookTitle
  , pattern INVocabularyStringTypeMediaAudiobookAuthorName
  , pattern INVocabularyStringTypeMediaShowTitle

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ sharedVocabulary@
sharedVocabulary :: IO (Id INVocabulary)
sharedVocabulary  =
  do
    cls' <- getRequiredClass "INVocabulary"
    sendClassMessage cls' sharedVocabularySelector

-- | Replaces all vocabulary strings already set for the given type. Strings should be sorted by their expected importance to the user, in descending order. There is no guarantee that every provided string will be used, but preference is given to strings at the beginning of the set. Any strings larger than 1024 bytes when encoded as UTF-16 (roughly 500 characters) will be discarded.
--
-- ObjC selector: @- setVocabularyStrings:ofType:@
setVocabularyStrings_ofType :: (IsINVocabulary inVocabulary, IsNSOrderedSet vocabulary) => inVocabulary -> vocabulary -> INVocabularyStringType -> IO ()
setVocabularyStrings_ofType inVocabulary vocabulary type_ =
  sendMessage inVocabulary setVocabularyStrings_ofTypeSelector (toNSOrderedSet vocabulary) type_

-- | @- setVocabulary:ofType:@
setVocabulary_ofType :: (IsINVocabulary inVocabulary, IsNSOrderedSet vocabulary) => inVocabulary -> vocabulary -> INVocabularyStringType -> IO ()
setVocabulary_ofType inVocabulary vocabulary type_ =
  sendMessage inVocabulary setVocabulary_ofTypeSelector (toNSOrderedSet vocabulary) type_

-- | Removes all vocabulary strings for every INVocabularyStringType the calling app has previously registered.
--
-- ObjC selector: @- removeAllVocabularyStrings@
removeAllVocabularyStrings :: IsINVocabulary inVocabulary => inVocabulary -> IO ()
removeAllVocabularyStrings inVocabulary =
  sendMessage inVocabulary removeAllVocabularyStringsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedVocabulary@
sharedVocabularySelector :: Selector '[] (Id INVocabulary)
sharedVocabularySelector = mkSelector "sharedVocabulary"

-- | @Selector@ for @setVocabularyStrings:ofType:@
setVocabularyStrings_ofTypeSelector :: Selector '[Id NSOrderedSet, INVocabularyStringType] ()
setVocabularyStrings_ofTypeSelector = mkSelector "setVocabularyStrings:ofType:"

-- | @Selector@ for @setVocabulary:ofType:@
setVocabulary_ofTypeSelector :: Selector '[Id NSOrderedSet, INVocabularyStringType] ()
setVocabulary_ofTypeSelector = mkSelector "setVocabulary:ofType:"

-- | @Selector@ for @removeAllVocabularyStrings@
removeAllVocabularyStringsSelector :: Selector '[] ()
removeAllVocabularyStringsSelector = mkSelector "removeAllVocabularyStrings"

