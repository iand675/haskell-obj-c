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

import ObjC.ReplayKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- previewControllerDelegate@
previewControllerDelegate :: IsRPPreviewViewController rpPreviewViewController => rpPreviewViewController -> IO RawId
previewControllerDelegate rpPreviewViewController  =
    fmap (RawId . castPtr) $ sendMsg rpPreviewViewController (mkSelector "previewControllerDelegate") (retPtr retVoid) []

-- | @- setPreviewControllerDelegate:@
setPreviewControllerDelegate :: IsRPPreviewViewController rpPreviewViewController => rpPreviewViewController -> RawId -> IO ()
setPreviewControllerDelegate rpPreviewViewController  value =
    sendMsg rpPreviewViewController (mkSelector "setPreviewControllerDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previewControllerDelegate@
previewControllerDelegateSelector :: Selector
previewControllerDelegateSelector = mkSelector "previewControllerDelegate"

-- | @Selector@ for @setPreviewControllerDelegate:@
setPreviewControllerDelegateSelector :: Selector
setPreviewControllerDelegateSelector = mkSelector "setPreviewControllerDelegate:"

