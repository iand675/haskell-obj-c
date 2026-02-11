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
  , initSelector
  , replyWithImageFileURLSelector
  , extensionBadgeSelector
  , setExtensionBadgeSelector


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

import ObjC.QuickLookThumbnailing.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsQLThumbnailReply qlThumbnailReply => qlThumbnailReply -> IO (Id QLThumbnailReply)
init_ qlThumbnailReply  =
  sendMsg qlThumbnailReply (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | You can create a reply object with a file URL of an image that will be used as the thumbnail. The image will be downscaled to fit the size of the QLFileThumbnailRequest if necessary.
--
-- ObjC selector: @+ replyWithImageFileURL:@
replyWithImageFileURL :: IsNSURL fileURL => fileURL -> IO (Id QLThumbnailReply)
replyWithImageFileURL fileURL =
  do
    cls' <- getRequiredClass "QLThumbnailReply"
    withObjCPtr fileURL $ \raw_fileURL ->
      sendClassMsg cls' (mkSelector "replyWithImageFileURL:") (retPtr retVoid) [argPtr (castPtr raw_fileURL :: Ptr ())] >>= retainedObject . castPtr

-- | The extensionBadge is a short string identifying the file type used as a badge when producing an icon.
--
-- ObjC selector: @- extensionBadge@
extensionBadge :: IsQLThumbnailReply qlThumbnailReply => qlThumbnailReply -> IO (Id NSString)
extensionBadge qlThumbnailReply  =
  sendMsg qlThumbnailReply (mkSelector "extensionBadge") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The extensionBadge is a short string identifying the file type used as a badge when producing an icon.
--
-- ObjC selector: @- setExtensionBadge:@
setExtensionBadge :: (IsQLThumbnailReply qlThumbnailReply, IsNSString value) => qlThumbnailReply -> value -> IO ()
setExtensionBadge qlThumbnailReply  value =
withObjCPtr value $ \raw_value ->
    sendMsg qlThumbnailReply (mkSelector "setExtensionBadge:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @replyWithImageFileURL:@
replyWithImageFileURLSelector :: Selector
replyWithImageFileURLSelector = mkSelector "replyWithImageFileURL:"

-- | @Selector@ for @extensionBadge@
extensionBadgeSelector :: Selector
extensionBadgeSelector = mkSelector "extensionBadge"

-- | @Selector@ for @setExtensionBadge:@
setExtensionBadgeSelector :: Selector
setExtensionBadgeSelector = mkSelector "setExtensionBadge:"

