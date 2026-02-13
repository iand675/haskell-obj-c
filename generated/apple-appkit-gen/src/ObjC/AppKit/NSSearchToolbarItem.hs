{-# LANGUAGE DataKinds #-}
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
  , preferredWidthForSearchFieldSelector
  , resignsFirstResponderWithCancelSelector
  , searchFieldSelector
  , setPreferredWidthForSearchFieldSelector
  , setResignsFirstResponderWithCancelSelector
  , setSearchFieldSelector
  , setViewSelector
  , viewSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Starts a search interaction. If necessary, expands to the preferred width and moves the keyboard focus to the search field.
--
-- ObjC selector: @- beginSearchInteraction@
beginSearchInteraction :: IsNSSearchToolbarItem nsSearchToolbarItem => nsSearchToolbarItem -> IO ()
beginSearchInteraction nsSearchToolbarItem =
  sendMessage nsSearchToolbarItem beginSearchInteractionSelector

-- | Ends a search interaction. Gives up the first responder by calling @-endEditing:@ to the search field. Adjusts to the natural available width for the toolbar item if necessary.
--
-- ObjC selector: @- endSearchInteraction@
endSearchInteraction :: IsNSSearchToolbarItem nsSearchToolbarItem => nsSearchToolbarItem -> IO ()
endSearchInteraction nsSearchToolbarItem =
  sendMessage nsSearchToolbarItem endSearchInteractionSelector

-- | An @NSSearchField@ displayed in the toolbar item. While inside the toolbar item, the field properties and layout constraints are managed by the item. The field should be configured before assigned. The width constraint for the field could be updated after assigned. When set to nil, will reset to a search field with the default configuration.
--
-- ObjC selector: @- searchField@
searchField :: IsNSSearchToolbarItem nsSearchToolbarItem => nsSearchToolbarItem -> IO (Id NSSearchField)
searchField nsSearchToolbarItem =
  sendMessage nsSearchToolbarItem searchFieldSelector

-- | An @NSSearchField@ displayed in the toolbar item. While inside the toolbar item, the field properties and layout constraints are managed by the item. The field should be configured before assigned. The width constraint for the field could be updated after assigned. When set to nil, will reset to a search field with the default configuration.
--
-- ObjC selector: @- setSearchField:@
setSearchField :: (IsNSSearchToolbarItem nsSearchToolbarItem, IsNSSearchField value) => nsSearchToolbarItem -> value -> IO ()
setSearchField nsSearchToolbarItem value =
  sendMessage nsSearchToolbarItem setSearchFieldSelector (toNSSearchField value)

-- | The base view property is owned by the toolbar item and not available for customization.
--
-- ObjC selector: @- view@
view :: IsNSSearchToolbarItem nsSearchToolbarItem => nsSearchToolbarItem -> IO RawId
view nsSearchToolbarItem =
  sendMessage nsSearchToolbarItem viewSelector

-- | The base view property is owned by the toolbar item and not available for customization.
--
-- ObjC selector: @- setView:@
setView :: IsNSSearchToolbarItem nsSearchToolbarItem => nsSearchToolbarItem -> RawId -> IO ()
setView nsSearchToolbarItem value =
  sendMessage nsSearchToolbarItem setViewSelector value

-- | When YES, the cancel button in the field resigns the first responder status of the search field as clearing the contents. The default is YES.
--
-- ObjC selector: @- resignsFirstResponderWithCancel@
resignsFirstResponderWithCancel :: IsNSSearchToolbarItem nsSearchToolbarItem => nsSearchToolbarItem -> IO Bool
resignsFirstResponderWithCancel nsSearchToolbarItem =
  sendMessage nsSearchToolbarItem resignsFirstResponderWithCancelSelector

-- | When YES, the cancel button in the field resigns the first responder status of the search field as clearing the contents. The default is YES.
--
-- ObjC selector: @- setResignsFirstResponderWithCancel:@
setResignsFirstResponderWithCancel :: IsNSSearchToolbarItem nsSearchToolbarItem => nsSearchToolbarItem -> Bool -> IO ()
setResignsFirstResponderWithCancel nsSearchToolbarItem value =
  sendMessage nsSearchToolbarItem setResignsFirstResponderWithCancelSelector value

-- | The preferred width for the search field. This value is used to configure the search field width whenever it gets the keyboard focus. If specifying custom width constraints to the search field, they should not conflict with this value.
--
-- ObjC selector: @- preferredWidthForSearchField@
preferredWidthForSearchField :: IsNSSearchToolbarItem nsSearchToolbarItem => nsSearchToolbarItem -> IO CDouble
preferredWidthForSearchField nsSearchToolbarItem =
  sendMessage nsSearchToolbarItem preferredWidthForSearchFieldSelector

-- | The preferred width for the search field. This value is used to configure the search field width whenever it gets the keyboard focus. If specifying custom width constraints to the search field, they should not conflict with this value.
--
-- ObjC selector: @- setPreferredWidthForSearchField:@
setPreferredWidthForSearchField :: IsNSSearchToolbarItem nsSearchToolbarItem => nsSearchToolbarItem -> CDouble -> IO ()
setPreferredWidthForSearchField nsSearchToolbarItem value =
  sendMessage nsSearchToolbarItem setPreferredWidthForSearchFieldSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @beginSearchInteraction@
beginSearchInteractionSelector :: Selector '[] ()
beginSearchInteractionSelector = mkSelector "beginSearchInteraction"

-- | @Selector@ for @endSearchInteraction@
endSearchInteractionSelector :: Selector '[] ()
endSearchInteractionSelector = mkSelector "endSearchInteraction"

-- | @Selector@ for @searchField@
searchFieldSelector :: Selector '[] (Id NSSearchField)
searchFieldSelector = mkSelector "searchField"

-- | @Selector@ for @setSearchField:@
setSearchFieldSelector :: Selector '[Id NSSearchField] ()
setSearchFieldSelector = mkSelector "setSearchField:"

-- | @Selector@ for @view@
viewSelector :: Selector '[] RawId
viewSelector = mkSelector "view"

-- | @Selector@ for @setView:@
setViewSelector :: Selector '[RawId] ()
setViewSelector = mkSelector "setView:"

-- | @Selector@ for @resignsFirstResponderWithCancel@
resignsFirstResponderWithCancelSelector :: Selector '[] Bool
resignsFirstResponderWithCancelSelector = mkSelector "resignsFirstResponderWithCancel"

-- | @Selector@ for @setResignsFirstResponderWithCancel:@
setResignsFirstResponderWithCancelSelector :: Selector '[Bool] ()
setResignsFirstResponderWithCancelSelector = mkSelector "setResignsFirstResponderWithCancel:"

-- | @Selector@ for @preferredWidthForSearchField@
preferredWidthForSearchFieldSelector :: Selector '[] CDouble
preferredWidthForSearchFieldSelector = mkSelector "preferredWidthForSearchField"

-- | @Selector@ for @setPreferredWidthForSearchField:@
setPreferredWidthForSearchFieldSelector :: Selector '[CDouble] ()
setPreferredWidthForSearchFieldSelector = mkSelector "setPreferredWidthForSearchField:"

