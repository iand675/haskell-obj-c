{-# LANGUAGE PatternSynonyms #-}
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
  , showRelativeToRect_ofView_preferredEdgeSelector
  , closeSelector
  , displayedKeysSelector
  , setDisplayedKeysSelector

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

import ObjC.ContactsUI.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Shows the picker popover relative to a positioning rect for a view with a preferred edge. See NSPopover for more information.
--
-- ObjC selector: @- showRelativeToRect:ofView:preferredEdge:@
showRelativeToRect_ofView_preferredEdge :: (IsCNContactPicker cnContactPicker, IsNSView positioningView) => cnContactPicker -> NSRect -> positioningView -> NSRectEdge -> IO ()
showRelativeToRect_ofView_preferredEdge cnContactPicker  positioningRect positioningView preferredEdge =
withObjCPtr positioningView $ \raw_positioningView ->
    sendMsg cnContactPicker (mkSelector "showRelativeToRect:ofView:preferredEdge:") retVoid [argNSRect positioningRect, argPtr (castPtr raw_positioningView :: Ptr ()), argCULong (coerce preferredEdge)]

-- | Closes the popover.
--
-- ObjC selector: @- close@
close :: IsCNContactPicker cnContactPicker => cnContactPicker -> IO ()
close cnContactPicker  =
  sendMsg cnContactPicker (mkSelector "close") retVoid []

-- | The CNContact keys to display when a contact is expanded.
--
-- If no keys are provided, the picker will select contacts instead of values.
--
-- ObjC selector: @- displayedKeys@
displayedKeys :: IsCNContactPicker cnContactPicker => cnContactPicker -> IO (Id NSArray)
displayedKeys cnContactPicker  =
  sendMsg cnContactPicker (mkSelector "displayedKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The CNContact keys to display when a contact is expanded.
--
-- If no keys are provided, the picker will select contacts instead of values.
--
-- ObjC selector: @- setDisplayedKeys:@
setDisplayedKeys :: (IsCNContactPicker cnContactPicker, IsNSArray value) => cnContactPicker -> value -> IO ()
setDisplayedKeys cnContactPicker  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnContactPicker (mkSelector "setDisplayedKeys:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @showRelativeToRect:ofView:preferredEdge:@
showRelativeToRect_ofView_preferredEdgeSelector :: Selector
showRelativeToRect_ofView_preferredEdgeSelector = mkSelector "showRelativeToRect:ofView:preferredEdge:"

-- | @Selector@ for @close@
closeSelector :: Selector
closeSelector = mkSelector "close"

-- | @Selector@ for @displayedKeys@
displayedKeysSelector :: Selector
displayedKeysSelector = mkSelector "displayedKeys"

-- | @Selector@ for @setDisplayedKeys:@
setDisplayedKeysSelector :: Selector
setDisplayedKeysSelector = mkSelector "setDisplayedKeys:"

