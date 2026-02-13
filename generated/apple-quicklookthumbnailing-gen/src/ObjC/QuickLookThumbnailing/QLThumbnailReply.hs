{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | To provide a thumbnail for a request, you have to return a QLThumbnailReply object.
--
-- To provide a thumbnail, you have two options: 1. Draw the thumbnail, by providing a QLThumbnailReply created with a drawing block. 2. Pass the thumbnail file URL, by providing a QLThumbnailReply created with a file URL.
--
-- Generated bindings for @QLThumbnailReply@.
module ObjC.QuickLookThumbnailing.QLThumbnailReply
  ( QLThumbnailReply
  , IsQLThumbnailReply(..)
  , init_
  , replyWithImageFileURL
  , extensionBadge
  , setExtensionBadge
  , extensionBadgeSelector
  , initSelector
  , replyWithImageFileURLSelector
  , setExtensionBadgeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuickLookThumbnailing.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsQLThumbnailReply qlThumbnailReply => qlThumbnailReply -> IO (Id QLThumbnailReply)
init_ qlThumbnailReply =
  sendOwnedMessage qlThumbnailReply initSelector

-- | You can create a reply object with a file URL of an image that will be used as the thumbnail. The image will be downscaled to fit the size of the QLFileThumbnailRequest if necessary.
--
-- ObjC selector: @+ replyWithImageFileURL:@
replyWithImageFileURL :: IsNSURL fileURL => fileURL -> IO (Id QLThumbnailReply)
replyWithImageFileURL fileURL =
  do
    cls' <- getRequiredClass "QLThumbnailReply"
    sendClassMessage cls' replyWithImageFileURLSelector (toNSURL fileURL)

-- | The extensionBadge is a short string identifying the file type used as a badge when producing an icon.
--
-- ObjC selector: @- extensionBadge@
extensionBadge :: IsQLThumbnailReply qlThumbnailReply => qlThumbnailReply -> IO (Id NSString)
extensionBadge qlThumbnailReply =
  sendMessage qlThumbnailReply extensionBadgeSelector

-- | The extensionBadge is a short string identifying the file type used as a badge when producing an icon.
--
-- ObjC selector: @- setExtensionBadge:@
setExtensionBadge :: (IsQLThumbnailReply qlThumbnailReply, IsNSString value) => qlThumbnailReply -> value -> IO ()
setExtensionBadge qlThumbnailReply value =
  sendMessage qlThumbnailReply setExtensionBadgeSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id QLThumbnailReply)
initSelector = mkSelector "init"

-- | @Selector@ for @replyWithImageFileURL:@
replyWithImageFileURLSelector :: Selector '[Id NSURL] (Id QLThumbnailReply)
replyWithImageFileURLSelector = mkSelector "replyWithImageFileURL:"

-- | @Selector@ for @extensionBadge@
extensionBadgeSelector :: Selector '[] (Id NSString)
extensionBadgeSelector = mkSelector "extensionBadge"

-- | @Selector@ for @setExtensionBadge:@
setExtensionBadgeSelector :: Selector '[Id NSString] ()
setExtensionBadgeSelector = mkSelector "setExtensionBadge:"

