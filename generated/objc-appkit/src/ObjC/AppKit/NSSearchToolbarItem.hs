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
  , resignsFirstResponderWithCancel
  , setResignsFirstResponderWithCancel
  , preferredWidthForSearchField
  , setPreferredWidthForSearchField
  , beginSearchInteractionSelector
  , endSearchInteractionSelector
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
  sendMsg nsSearchToolbarItem (mkSelector "setPreferredWidthForSearchField:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @beginSearchInteraction@
beginSearchInteractionSelector :: Selector
beginSearchInteractionSelector = mkSelector "beginSearchInteraction"

-- | @Selector@ for @endSearchInteraction@
endSearchInteractionSelector :: Selector
endSearchInteractionSelector = mkSelector "endSearchInteraction"

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

