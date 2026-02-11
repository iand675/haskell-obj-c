{-# LANGUAGE PatternSynonyms #-}
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
  , sharedVocabularySelector
  , setVocabularyStrings_ofTypeSelector
  , setVocabulary_ofTypeSelector
  , removeAllVocabularyStringsSelector

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

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ sharedVocabulary@
sharedVocabulary :: IO (Id INVocabulary)
sharedVocabulary  =
  do
    cls' <- getRequiredClass "INVocabulary"
    sendClassMsg cls' (mkSelector "sharedVocabulary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Replaces all vocabulary strings already set for the given type. Strings should be sorted by their expected importance to the user, in descending order. There is no guarantee that every provided string will be used, but preference is given to strings at the beginning of the set. Any strings larger than 1024 bytes when encoded as UTF-16 (roughly 500 characters) will be discarded.
--
-- ObjC selector: @- setVocabularyStrings:ofType:@
setVocabularyStrings_ofType :: (IsINVocabulary inVocabulary, IsNSOrderedSet vocabulary) => inVocabulary -> vocabulary -> INVocabularyStringType -> IO ()
setVocabularyStrings_ofType inVocabulary  vocabulary type_ =
withObjCPtr vocabulary $ \raw_vocabulary ->
    sendMsg inVocabulary (mkSelector "setVocabularyStrings:ofType:") retVoid [argPtr (castPtr raw_vocabulary :: Ptr ()), argCLong (coerce type_)]

-- | @- setVocabulary:ofType:@
setVocabulary_ofType :: (IsINVocabulary inVocabulary, IsNSOrderedSet vocabulary) => inVocabulary -> vocabulary -> INVocabularyStringType -> IO ()
setVocabulary_ofType inVocabulary  vocabulary type_ =
withObjCPtr vocabulary $ \raw_vocabulary ->
    sendMsg inVocabulary (mkSelector "setVocabulary:ofType:") retVoid [argPtr (castPtr raw_vocabulary :: Ptr ()), argCLong (coerce type_)]

-- | Removes all vocabulary strings for every INVocabularyStringType the calling app has previously registered.
--
-- ObjC selector: @- removeAllVocabularyStrings@
removeAllVocabularyStrings :: IsINVocabulary inVocabulary => inVocabulary -> IO ()
removeAllVocabularyStrings inVocabulary  =
  sendMsg inVocabulary (mkSelector "removeAllVocabularyStrings") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedVocabulary@
sharedVocabularySelector :: Selector
sharedVocabularySelector = mkSelector "sharedVocabulary"

-- | @Selector@ for @setVocabularyStrings:ofType:@
setVocabularyStrings_ofTypeSelector :: Selector
setVocabularyStrings_ofTypeSelector = mkSelector "setVocabularyStrings:ofType:"

-- | @Selector@ for @setVocabulary:ofType:@
setVocabulary_ofTypeSelector :: Selector
setVocabulary_ofTypeSelector = mkSelector "setVocabulary:ofType:"

-- | @Selector@ for @removeAllVocabularyStrings@
removeAllVocabularyStringsSelector :: Selector
removeAllVocabularyStringsSelector = mkSelector "removeAllVocabularyStrings"

