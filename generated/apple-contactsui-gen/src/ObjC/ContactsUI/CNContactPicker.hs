{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CNContactPicker is a popover-based contact picker for choosing a contact or a contact's value, such as a phone number or email address.
--
-- Generated bindings for @CNContactPicker@.
module ObjC.ContactsUI.CNContactPicker
  ( CNContactPicker
  , IsCNContactPicker(..)
  , showRelativeToRect_ofView_preferredEdge
  , close
  , displayedKeys
  , setDisplayedKeys
  , delegate
  , setDelegate
  , closeSelector
  , delegateSelector
  , displayedKeysSelector
  , setDelegateSelector
  , setDisplayedKeysSelector
  , showRelativeToRect_ofView_preferredEdgeSelector

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

import ObjC.ContactsUI.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Shows the picker popover relative to a positioning rect for a view with a preferred edge. See NSPopover for more information.
--
-- ObjC selector: @- showRelativeToRect:ofView:preferredEdge:@
showRelativeToRect_ofView_preferredEdge :: (IsCNContactPicker cnContactPicker, IsNSView positioningView) => cnContactPicker -> NSRect -> positioningView -> NSRectEdge -> IO ()
showRelativeToRect_ofView_preferredEdge cnContactPicker positioningRect positioningView preferredEdge =
  sendMessage cnContactPicker showRelativeToRect_ofView_preferredEdgeSelector positioningRect (toNSView positioningView) preferredEdge

-- | Closes the popover.
--
-- ObjC selector: @- close@
close :: IsCNContactPicker cnContactPicker => cnContactPicker -> IO ()
close cnContactPicker =
  sendMessage cnContactPicker closeSelector

-- | The CNContact keys to display when a contact is expanded.
--
-- If no keys are provided, the picker will select contacts instead of values.
--
-- ObjC selector: @- displayedKeys@
displayedKeys :: IsCNContactPicker cnContactPicker => cnContactPicker -> IO (Id NSArray)
displayedKeys cnContactPicker =
  sendMessage cnContactPicker displayedKeysSelector

-- | The CNContact keys to display when a contact is expanded.
--
-- If no keys are provided, the picker will select contacts instead of values.
--
-- ObjC selector: @- setDisplayedKeys:@
setDisplayedKeys :: (IsCNContactPicker cnContactPicker, IsNSArray value) => cnContactPicker -> value -> IO ()
setDisplayedKeys cnContactPicker value =
  sendMessage cnContactPicker setDisplayedKeysSelector (toNSArray value)

-- | The picker delegate to be notified when the user chooses a contact or value.
--
-- ObjC selector: @- delegate@
delegate :: IsCNContactPicker cnContactPicker => cnContactPicker -> IO RawId
delegate cnContactPicker =
  sendMessage cnContactPicker delegateSelector

-- | The picker delegate to be notified when the user chooses a contact or value.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsCNContactPicker cnContactPicker => cnContactPicker -> RawId -> IO ()
setDelegate cnContactPicker value =
  sendMessage cnContactPicker setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @showRelativeToRect:ofView:preferredEdge:@
showRelativeToRect_ofView_preferredEdgeSelector :: Selector '[NSRect, Id NSView, NSRectEdge] ()
showRelativeToRect_ofView_preferredEdgeSelector = mkSelector "showRelativeToRect:ofView:preferredEdge:"

-- | @Selector@ for @close@
closeSelector :: Selector '[] ()
closeSelector = mkSelector "close"

-- | @Selector@ for @displayedKeys@
displayedKeysSelector :: Selector '[] (Id NSArray)
displayedKeysSelector = mkSelector "displayedKeys"

-- | @Selector@ for @setDisplayedKeys:@
setDisplayedKeysSelector :: Selector '[Id NSArray] ()
setDisplayedKeysSelector = mkSelector "setDisplayedKeys:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

