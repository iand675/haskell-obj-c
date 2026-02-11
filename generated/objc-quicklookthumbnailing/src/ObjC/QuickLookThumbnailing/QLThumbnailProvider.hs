{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @QLThumbnailProvider@.
module ObjC.QuickLookThumbnailing.QLThumbnailProvider
  ( QLThumbnailProvider
  , IsQLThumbnailProvider(..)
  , provideThumbnailForFileRequest_completionHandler
  , provideThumbnailForFileRequest_completionHandlerSelector


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

-- | Subclass this method to provide a QLThumbnailReply that either contains a drawing block or an image file URL.
--
-- @request@ — An object which contains information about the thumbnail that should be provided. It contains the URL of the file to provide a thumbnail for.
--
-- @handler@ — Call the completion handler with a QLThumbnailReply if you can provide a thumbnail, or with an NSError if you cannot.                If an error is passed or reply is nil, no thumbnail will be drawn.                The handler can be called asynchronously after the method has returned.
--
-- ObjC selector: @- provideThumbnailForFileRequest:completionHandler:@
provideThumbnailForFileRequest_completionHandler :: (IsQLThumbnailProvider qlThumbnailProvider, IsQLFileThumbnailRequest request) => qlThumbnailProvider -> request -> Ptr () -> IO ()
provideThumbnailForFileRequest_completionHandler qlThumbnailProvider  request handler =
withObjCPtr request $ \raw_request ->
    sendMsg qlThumbnailProvider (mkSelector "provideThumbnailForFileRequest:completionHandler:") retVoid [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @provideThumbnailForFileRequest:completionHandler:@
provideThumbnailForFileRequest_completionHandlerSelector :: Selector
provideThumbnailForFileRequest_completionHandlerSelector = mkSelector "provideThumbnailForFileRequest:completionHandler:"

