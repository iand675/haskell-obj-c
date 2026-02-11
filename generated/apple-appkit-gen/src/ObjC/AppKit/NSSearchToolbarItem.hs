{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | @NSSearchToolbarItem@ provides the standard UI behavior for integrating a search field into the toolbar.
--
-- Generated bindings for @NSSearchToolbarItem@.
module ObjC.AppKit.NSSearchToolbarItem
  ( NSSearchToolbarItem
  , IsNSSearchToolbarItem(..)
  , beginSearchInteraction
  , endSearchInteraction
  , searchField
  , setSearchField
  , view
  , setView
  , resignsFirstResponderWithCancel
  , setResignsFirstResponderWithCancel
  , preferredWidthForSearchField
  , setPreferredWidthForSearchField
  , beginSearchInteractionSelector
  , endSearchInteractionSelector
  , searchFieldSelector
  , setSearchFieldSelector
  , viewSelector
  , setViewSelector
  , resignsFirstResponderWithCancelSelector
  , setResignsFirstResponderWithCancelSelector
  , preferredWidthForSearchFieldSelector
  , setPreferredWidthForSearchFieldSelector


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
import ObjC.Foundation.Internal.Classes

-- | Starts a search interaction. If necessary, expands to the preferred width and moves the keyboard focus to the search field.
--
-- ObjC selector: @- beginSearchInteraction@
beginSearchInteraction :: IsNSSearchToolbarItem nsSearchToolbarItem => nsSearchToolbarItem -> IO ()
beginSearchInteraction nsSearchToolbarItem  =
    sendMsg nsSearchToolbarItem (mkSelector "beginSearchInteraction") retVoid []

-- | Ends a search interaction. Gives up the first responder by calling @-endEditing:@ to the search field. Adjusts to the natural available width for the toolbar item if necessary.
--
-- ObjC selector: @- endSearchInteraction@
endSearchInteraction :: IsNSSearchToolbarItem nsSearchToolbarItem => nsSearchToolbarItem -> IO ()
endSearchInteraction nsSearchToolbarItem  =
    sendMsg nsSearchToolbarItem (mkSelector "endSearchInteraction") retVoid []

-- | An @NSSearchField@ displayed in the toolbar item. While inside the toolbar item, the field properties and layout constraints are managed by the item. The field should be configured before assigned. The width constraint for the field could be updated after assigned. When set to nil, will reset to a search field with the default configuration.
--
-- ObjC selector: @- searchField@
searchField :: IsNSSearchToolbarItem nsSearchToolbarItem => nsSearchToolbarItem -> IO (Id NSSearchField)
searchField nsSearchToolbarItem  =
    sendMsg nsSearchToolbarItem (mkSelector "searchField") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An @NSSearchField@ displayed in the toolbar item. While inside the toolbar item, the field properties and layout constraints are managed by the item. The field should be configured before assigned. The width constraint for the field could be updated after assigned. When set to nil, will reset to a search field with the default configuration.
--
-- ObjC selector: @- setSearchField:@
setSearchField :: (IsNSSearchToolbarItem nsSearchToolbarItem, IsNSSearchField value) => nsSearchToolbarItem -> value -> IO ()
setSearchField nsSearchToolbarItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSearchToolbarItem (mkSelector "setSearchField:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The base view property is owned by the toolbar item and not available for customization.
--
-- ObjC selector: @- view@
view :: IsNSSearchToolbarItem nsSearchToolbarItem => nsSearchToolbarItem -> IO RawId
view nsSearchToolbarItem  =
    fmap (RawId . castPtr) $ sendMsg nsSearchToolbarItem (mkSelector "view") (retPtr retVoid) []

-- | The base view property is owned by the toolbar item and not available for customization.
--
-- ObjC selector: @- setView:@
setView :: IsNSSearchToolbarItem nsSearchToolbarItem => nsSearchToolbarItem -> RawId -> IO ()
setView nsSearchToolbarItem  value =
    sendMsg nsSearchToolbarItem (mkSelector "setView:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | When YES, the cancel button in the field resigns the first responder status of the search field as clearing the contents. The default is YES.
--
-- ObjC selector: @- resignsFirstResponderWithCancel@
resignsFirstResponderWithCancel :: IsNSSearchToolbarItem nsSearchToolbarItem => nsSearchToolbarItem -> IO Bool
resignsFirstResponderWithCancel nsSearchToolbarItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSearchToolbarItem (mkSelector "resignsFirstResponderWithCancel") retCULong []

-- | When YES, the cancel button in the field resigns the first responder status of the search field as clearing the contents. The default is YES.
--
-- ObjC selector: @- setResignsFirstResponderWithCancel:@
setResignsFirstResponderWithCancel :: IsNSSearchToolbarItem nsSearchToolbarItem => nsSearchToolbarItem -> Bool -> IO ()
setResignsFirstResponderWithCancel nsSearchToolbarItem  value =
    sendMsg nsSearchToolbarItem (mkSelector "setResignsFirstResponderWithCancel:") retVoid [argCULong (if value then 1 else 0)]

-- | The preferred width for the search field. This value is used to configure the search field width whenever it gets the keyboard focus. If specifying custom width constraints to the search field, they should not conflict with this value.
--
-- ObjC selector: @- preferredWidthForSearchField@
preferredWidthForSearchField :: IsNSSearchToolbarItem nsSearchToolbarItem => nsSearchToolbarItem -> IO CDouble
preferredWidthForSearchField nsSearchToolbarItem  =
    sendMsg nsSearchToolbarItem (mkSelector "preferredWidthForSearchField") retCDouble []

-- | The preferred width for the search field. This value is used to configure the search field width whenever it gets the keyboard focus. If specifying custom width constraints to the search field, they should not conflict with this value.
--
-- ObjC selector: @- setPreferredWidthForSearchField:@
setPreferredWidthForSearchField :: IsNSSearchToolbarItem nsSearchToolbarItem => nsSearchToolbarItem -> CDouble -> IO ()
setPreferredWidthForSearchField nsSearchToolbarItem  value =
    sendMsg nsSearchToolbarItem (mkSelector "setPreferredWidthForSearchField:") retVoid [argCDouble value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @beginSearchInteraction@
beginSearchInteractionSelector :: Selector
beginSearchInteractionSelector = mkSelector "beginSearchInteraction"

-- | @Selector@ for @endSearchInteraction@
endSearchInteractionSelector :: Selector
endSearchInteractionSelector = mkSelector "endSearchInteraction"

-- | @Selector@ for @searchField@
searchFieldSelector :: Selector
searchFieldSelector = mkSelector "searchField"

-- | @Selector@ for @setSearchField:@
setSearchFieldSelector :: Selector
setSearchFieldSelector = mkSelector "setSearchField:"

-- | @Selector@ for @view@
viewSelector :: Selector
viewSelector = mkSelector "view"

-- | @Selector@ for @setView:@
setViewSelector :: Selector
setViewSelector = mkSelector "setView:"

-- | @Selector@ for @resignsFirstResponderWithCancel@
resignsFirstResponderWithCancelSelector :: Selector
resignsFirstResponderWithCancelSelector = mkSelector "resignsFirstResponderWithCancel"

-- | @Selector@ for @setResignsFirstResponderWithCancel:@
setResignsFirstResponderWithCancelSelector :: Selector
setResignsFirstResponderWithCancelSelector = mkSelector "setResignsFirstResponderWithCancel:"

-- | @Selector@ for @preferredWidthForSearchField@
preferredWidthForSearchFieldSelector :: Selector
preferredWidthForSearchFieldSelector = mkSelector "preferredWidthForSearchField"

-- | @Selector@ for @setPreferredWidthForSearchField:@
setPreferredWidthForSearchFieldSelector :: Selector
setPreferredWidthForSearchFieldSelector = mkSelector "setPreferredWidthForSearchField:"

