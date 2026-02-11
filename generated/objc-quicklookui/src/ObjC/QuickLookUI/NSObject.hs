{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSObject@.
module ObjC.QuickLookUI.NSObject
  ( NSObject
  , IsNSObject(..)
  , acceptsPreviewPanelControl
  , beginPreviewPanelControl
  , endPreviewPanelControl
  , acceptsPreviewPanelControlSelector
  , beginPreviewPanelControlSelector
  , endPreviewPanelControlSelector


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

import ObjC.QuickLookUI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Sent to each object in the responder chain to find a controller.
--
-- @panel@ — The Preview Panel looking for a controller.
--
-- Returns: YES if the receiver accepts to control the panel. You should never call this method directly.
--
-- ObjC selector: @- acceptsPreviewPanelControl:@
acceptsPreviewPanelControl :: (IsNSObject nsObject, IsQLPreviewPanel panel) => nsObject -> panel -> IO Bool
acceptsPreviewPanelControl nsObject  panel =
withObjCPtr panel $ \raw_panel ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "acceptsPreviewPanelControl:") retCULong [argPtr (castPtr raw_panel :: Ptr ())]

-- | Sent to the object taking control of the Preview Panel.
--
-- @panel@ — The Preview Panel the receiver will control.
--
-- The receiver should setup the preview panel (data source, delegate, binding, etc.) here. You should never call this method directly.
--
-- ObjC selector: @- beginPreviewPanelControl:@
beginPreviewPanelControl :: (IsNSObject nsObject, IsQLPreviewPanel panel) => nsObject -> panel -> IO ()
beginPreviewPanelControl nsObject  panel =
withObjCPtr panel $ \raw_panel ->
    sendMsg nsObject (mkSelector "beginPreviewPanelControl:") retVoid [argPtr (castPtr raw_panel :: Ptr ())]

-- | Sent to the object in control of the Preview Panel just before stopping its control.
--
-- @panel@ — The Preview Panel that the receiver will stop controlling.
--
-- The receiver should unsetup the preview panel (data source, delegate, binding, etc.) here. You should never call this method directly.
--
-- ObjC selector: @- endPreviewPanelControl:@
endPreviewPanelControl :: (IsNSObject nsObject, IsQLPreviewPanel panel) => nsObject -> panel -> IO ()
endPreviewPanelControl nsObject  panel =
withObjCPtr panel $ \raw_panel ->
    sendMsg nsObject (mkSelector "endPreviewPanelControl:") retVoid [argPtr (castPtr raw_panel :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @acceptsPreviewPanelControl:@
acceptsPreviewPanelControlSelector :: Selector
acceptsPreviewPanelControlSelector = mkSelector "acceptsPreviewPanelControl:"

-- | @Selector@ for @beginPreviewPanelControl:@
beginPreviewPanelControlSelector :: Selector
beginPreviewPanelControlSelector = mkSelector "beginPreviewPanelControl:"

-- | @Selector@ for @endPreviewPanelControl:@
endPreviewPanelControlSelector :: Selector
endPreviewPanelControlSelector = mkSelector "endPreviewPanelControl:"

