{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSharingServicePicker@.
module ObjC.AppKit.NSSharingServicePicker
  ( NSSharingServicePicker
  , IsNSSharingServicePicker(..)
  , initWithItems
  , init_
  , showRelativeToRect_ofView_preferredEdge
  , close
  , delegate
  , setDelegate
  , standardShareMenuItem
  , initWithItemsSelector
  , initSelector
  , showRelativeToRect_ofView_preferredEdgeSelector
  , closeSelector
  , delegateSelector
  , setDelegateSelector
  , standardShareMenuItemSelector

  -- * Enum types
  , NSRectEdge(NSRectEdge)
  , pattern NSRectEdgeMinX
  , pattern NSRectEdgeMinY
  , pattern NSRectEdgeMaxX
  , pattern NSRectEdgeMaxY
  , pattern NSMinXEdge
  , pattern NSMinYEdge
  , pattern NSMaxXEdge
  , pattern NSMaxYEdge

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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Returns a new picker. The items represent the objects to be shared and must conform to the <NSPasteboardWriting> protocol or be an NSItemProvider or an NSDocument. (e.g. NSString, NSImage, NSURL, etc.)
--
-- ObjC selector: @- initWithItems:@
initWithItems :: (IsNSSharingServicePicker nsSharingServicePicker, IsNSArray items) => nsSharingServicePicker -> items -> IO (Id NSSharingServicePicker)
initWithItems nsSharingServicePicker  items =
  withObjCPtr items $ \raw_items ->
      sendMsg nsSharingServicePicker (mkSelector "initWithItems:") (retPtr retVoid) [argPtr (castPtr raw_items :: Ptr ())] >>= ownedObject . castPtr

-- | Use initWithItems: instead.
--
-- ObjC selector: @- init@
init_ :: IsNSSharingServicePicker nsSharingServicePicker => nsSharingServicePicker -> IO (Id NSSharingServicePicker)
init_ nsSharingServicePicker  =
    sendMsg nsSharingServicePicker (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Shows the picker, populated with sharing services related to the instance items. When the user selects one of the sharing services, the sharing service will be performed. Note that this method must be called on mouseDown.
--
-- ObjC selector: @- showRelativeToRect:ofView:preferredEdge:@
showRelativeToRect_ofView_preferredEdge :: (IsNSSharingServicePicker nsSharingServicePicker, IsNSView view) => nsSharingServicePicker -> NSRect -> view -> NSRectEdge -> IO ()
showRelativeToRect_ofView_preferredEdge nsSharingServicePicker  rect view preferredEdge =
  withObjCPtr view $ \raw_view ->
      sendMsg nsSharingServicePicker (mkSelector "showRelativeToRect:ofView:preferredEdge:") retVoid [argNSRect rect, argPtr (castPtr raw_view :: Ptr ()), argCULong (coerce preferredEdge)]

-- | Closes the picker UI. @-[NSSharingServicePickerDelegate sharingServicePicker:didChooseSharingService:]@ will be invoked if @delegate@ is set, with a @nil@ service.
--
-- ObjC selector: @- close@
close :: IsNSSharingServicePicker nsSharingServicePicker => nsSharingServicePicker -> IO ()
close nsSharingServicePicker  =
    sendMsg nsSharingServicePicker (mkSelector "close") retVoid []

-- | @- delegate@
delegate :: IsNSSharingServicePicker nsSharingServicePicker => nsSharingServicePicker -> IO RawId
delegate nsSharingServicePicker  =
    fmap (RawId . castPtr) $ sendMsg nsSharingServicePicker (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSSharingServicePicker nsSharingServicePicker => nsSharingServicePicker -> RawId -> IO ()
setDelegate nsSharingServicePicker  value =
    sendMsg nsSharingServicePicker (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Returns a menu item suitable to display the picker for the given items.
--
-- ObjC selector: @- standardShareMenuItem@
standardShareMenuItem :: IsNSSharingServicePicker nsSharingServicePicker => nsSharingServicePicker -> IO (Id NSMenuItem)
standardShareMenuItem nsSharingServicePicker  =
    sendMsg nsSharingServicePicker (mkSelector "standardShareMenuItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithItems:@
initWithItemsSelector :: Selector
initWithItemsSelector = mkSelector "initWithItems:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @showRelativeToRect:ofView:preferredEdge:@
showRelativeToRect_ofView_preferredEdgeSelector :: Selector
showRelativeToRect_ofView_preferredEdgeSelector = mkSelector "showRelativeToRect:ofView:preferredEdge:"

-- | @Selector@ for @close@
closeSelector :: Selector
closeSelector = mkSelector "close"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @standardShareMenuItem@
standardShareMenuItemSelector :: Selector
standardShareMenuItemSelector = mkSelector "standardShareMenuItem"

