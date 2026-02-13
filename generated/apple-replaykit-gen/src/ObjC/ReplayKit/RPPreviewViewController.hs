{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @RPPreviewViewController@.
module ObjC.ReplayKit.RPPreviewViewController
  ( RPPreviewViewController
  , IsRPPreviewViewController(..)
  , previewControllerDelegate
  , setPreviewControllerDelegate
  , previewControllerDelegateSelector
  , setPreviewControllerDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ReplayKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- previewControllerDelegate@
previewControllerDelegate :: IsRPPreviewViewController rpPreviewViewController => rpPreviewViewController -> IO RawId
previewControllerDelegate rpPreviewViewController =
  sendMessage rpPreviewViewController previewControllerDelegateSelector

-- | @- setPreviewControllerDelegate:@
setPreviewControllerDelegate :: IsRPPreviewViewController rpPreviewViewController => rpPreviewViewController -> RawId -> IO ()
setPreviewControllerDelegate rpPreviewViewController value =
  sendMessage rpPreviewViewController setPreviewControllerDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previewControllerDelegate@
previewControllerDelegateSelector :: Selector '[] RawId
previewControllerDelegateSelector = mkSelector "previewControllerDelegate"

-- | @Selector@ for @setPreviewControllerDelegate:@
setPreviewControllerDelegateSelector :: Selector '[RawId] ()
setPreviewControllerDelegateSelector = mkSelector "setPreviewControllerDelegate:"

