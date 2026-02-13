{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , closeSelector
  , delegateSelector
  , initSelector
  , initWithItemsSelector
  , setDelegateSelector
  , showRelativeToRect_ofView_preferredEdgeSelector
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithItems nsSharingServicePicker items =
  sendOwnedMessage nsSharingServicePicker initWithItemsSelector (toNSArray items)

-- | Use initWithItems: instead.
--
-- ObjC selector: @- init@
init_ :: IsNSSharingServicePicker nsSharingServicePicker => nsSharingServicePicker -> IO (Id NSSharingServicePicker)
init_ nsSharingServicePicker =
  sendOwnedMessage nsSharingServicePicker initSelector

-- | Shows the picker, populated with sharing services related to the instance items. When the user selects one of the sharing services, the sharing service will be performed. Note that this method must be called on mouseDown.
--
-- ObjC selector: @- showRelativeToRect:ofView:preferredEdge:@
showRelativeToRect_ofView_preferredEdge :: (IsNSSharingServicePicker nsSharingServicePicker, IsNSView view) => nsSharingServicePicker -> NSRect -> view -> NSRectEdge -> IO ()
showRelativeToRect_ofView_preferredEdge nsSharingServicePicker rect view preferredEdge =
  sendMessage nsSharingServicePicker showRelativeToRect_ofView_preferredEdgeSelector rect (toNSView view) preferredEdge

-- | Closes the picker UI. @-[NSSharingServicePickerDelegate sharingServicePicker:didChooseSharingService:]@ will be invoked if @delegate@ is set, with a @nil@ service.
--
-- ObjC selector: @- close@
close :: IsNSSharingServicePicker nsSharingServicePicker => nsSharingServicePicker -> IO ()
close nsSharingServicePicker =
  sendMessage nsSharingServicePicker closeSelector

-- | @- delegate@
delegate :: IsNSSharingServicePicker nsSharingServicePicker => nsSharingServicePicker -> IO RawId
delegate nsSharingServicePicker =
  sendMessage nsSharingServicePicker delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSSharingServicePicker nsSharingServicePicker => nsSharingServicePicker -> RawId -> IO ()
setDelegate nsSharingServicePicker value =
  sendMessage nsSharingServicePicker setDelegateSelector value

-- | Returns a menu item suitable to display the picker for the given items.
--
-- ObjC selector: @- standardShareMenuItem@
standardShareMenuItem :: IsNSSharingServicePicker nsSharingServicePicker => nsSharingServicePicker -> IO (Id NSMenuItem)
standardShareMenuItem nsSharingServicePicker =
  sendMessage nsSharingServicePicker standardShareMenuItemSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithItems:@
initWithItemsSelector :: Selector '[Id NSArray] (Id NSSharingServicePicker)
initWithItemsSelector = mkSelector "initWithItems:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSSharingServicePicker)
initSelector = mkSelector "init"

-- | @Selector@ for @showRelativeToRect:ofView:preferredEdge:@
showRelativeToRect_ofView_preferredEdgeSelector :: Selector '[NSRect, Id NSView, NSRectEdge] ()
showRelativeToRect_ofView_preferredEdgeSelector = mkSelector "showRelativeToRect:ofView:preferredEdge:"

-- | @Selector@ for @close@
closeSelector :: Selector '[] ()
closeSelector = mkSelector "close"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @standardShareMenuItem@
standardShareMenuItemSelector :: Selector '[] (Id NSMenuItem)
standardShareMenuItemSelector = mkSelector "standardShareMenuItem"

