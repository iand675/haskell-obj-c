{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPasteboardItem@.
module ObjC.SharedWithYou.NSPasteboardItem
  ( NSPasteboardItem
  , IsNSPasteboardItem(..)
  , collaborationMetadata
  , setCollaborationMetadata
  , collaborationMetadataSelector
  , setCollaborationMetadataSelector


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

import ObjC.SharedWithYou.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.SharedWithYouCore.Internal.Classes

-- | Sets the collaboration metadata on the pasteboard item.
--
-- ObjC selector: @- collaborationMetadata@
collaborationMetadata :: IsNSPasteboardItem nsPasteboardItem => nsPasteboardItem -> IO (Id SWCollaborationMetadata)
collaborationMetadata nsPasteboardItem  =
  sendMsg nsPasteboardItem (mkSelector "collaborationMetadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets the collaboration metadata on the pasteboard item.
--
-- ObjC selector: @- setCollaborationMetadata:@
setCollaborationMetadata :: (IsNSPasteboardItem nsPasteboardItem, IsSWCollaborationMetadata value) => nsPasteboardItem -> value -> IO ()
setCollaborationMetadata nsPasteboardItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPasteboardItem (mkSelector "setCollaborationMetadata:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @collaborationMetadata@
collaborationMetadataSelector :: Selector
collaborationMetadataSelector = mkSelector "collaborationMetadata"

-- | @Selector@ for @setCollaborationMetadata:@
setCollaborationMetadataSelector :: Selector
setCollaborationMetadataSelector = mkSelector "setCollaborationMetadata:"

