{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFileAccessIntent@.
module ObjC.Foundation.NSFileAccessIntent
  ( NSFileAccessIntent
  , IsNSFileAccessIntent(..)
  , readingIntentWithURL_options
  , writingIntentWithURL_options
  , url
  , readingIntentWithURL_optionsSelector
  , urlSelector
  , writingIntentWithURL_optionsSelector

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

-- | @+ readingIntentWithURL:options:@
readingIntentWithURL_options :: IsNSURL url => url -> NSFileCoordinatorReadingOptions -> IO (Id NSFileAccessIntent)
readingIntentWithURL_options url options =
  do
    cls' <- getRequiredClass "NSFileAccessIntent"
    sendClassMessage cls' readingIntentWithURL_optionsSelector (toNSURL url) options

-- | @+ writingIntentWithURL:options:@
writingIntentWithURL_options :: IsNSURL url => url -> NSFileCoordinatorWritingOptions -> IO (Id NSFileAccessIntent)
writingIntentWithURL_options url options =
  do
    cls' <- getRequiredClass "NSFileAccessIntent"
    sendClassMessage cls' writingIntentWithURL_optionsSelector (toNSURL url) options

-- | @- URL@
url :: IsNSFileAccessIntent nsFileAccessIntent => nsFileAccessIntent -> IO (Id NSURL)
url nsFileAccessIntent =
  sendMessage nsFileAccessIntent urlSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readingIntentWithURL:options:@
readingIntentWithURL_optionsSelector :: Selector '[Id NSURL, NSFileCoordinatorReadingOptions] (Id NSFileAccessIntent)
readingIntentWithURL_optionsSelector = mkSelector "readingIntentWithURL:options:"

-- | @Selector@ for @writingIntentWithURL:options:@
writingIntentWithURL_optionsSelector :: Selector '[Id NSURL, NSFileCoordinatorWritingOptions] (Id NSFileAccessIntent)
writingIntentWithURL_optionsSelector = mkSelector "writingIntentWithURL:options:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

