{-# LANGUAGE PatternSynonyms #-}
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
  , writingIntentWithURL_optionsSelector
  , urlSelector

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

-- | @+ readingIntentWithURL:options:@
readingIntentWithURL_options :: IsNSURL url => url -> NSFileCoordinatorReadingOptions -> IO (Id NSFileAccessIntent)
readingIntentWithURL_options url options =
  do
    cls' <- getRequiredClass "NSFileAccessIntent"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "readingIntentWithURL:options:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (coerce options)] >>= retainedObject . castPtr

-- | @+ writingIntentWithURL:options:@
writingIntentWithURL_options :: IsNSURL url => url -> NSFileCoordinatorWritingOptions -> IO (Id NSFileAccessIntent)
writingIntentWithURL_options url options =
  do
    cls' <- getRequiredClass "NSFileAccessIntent"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "writingIntentWithURL:options:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (coerce options)] >>= retainedObject . castPtr

-- | @- URL@
url :: IsNSFileAccessIntent nsFileAccessIntent => nsFileAccessIntent -> IO (Id NSURL)
url nsFileAccessIntent  =
  sendMsg nsFileAccessIntent (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readingIntentWithURL:options:@
readingIntentWithURL_optionsSelector :: Selector
readingIntentWithURL_optionsSelector = mkSelector "readingIntentWithURL:options:"

-- | @Selector@ for @writingIntentWithURL:options:@
writingIntentWithURL_optionsSelector :: Selector
writingIntentWithURL_optionsSelector = mkSelector "writingIntentWithURL:options:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

