{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
provideThumbnailForFileRequest_completionHandler qlThumbnailProvider request handler =
  sendMessage qlThumbnailProvider provideThumbnailForFileRequest_completionHandlerSelector (toQLFileThumbnailRequest request) handler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @provideThumbnailForFileRequest:completionHandler:@
provideThumbnailForFileRequest_completionHandlerSelector :: Selector '[Id QLFileThumbnailRequest, Ptr ()] ()
provideThumbnailForFileRequest_completionHandlerSelector = mkSelector "provideThumbnailForFileRequest:completionHandler:"

