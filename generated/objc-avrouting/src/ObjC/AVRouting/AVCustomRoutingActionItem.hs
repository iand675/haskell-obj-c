{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents a custom action item to display in a device route picker.
--
-- Use this class to specify supplemental action items to display in the list of discovered routes. Tapping a custom item dismisses the picker and calls the ``AVCustomRoutingControllerDelegate/customRoutingController:didSelectItem:`` method of ``AVCustomRoutingControllerDelegate``.
--
-- Generated bindings for @AVCustomRoutingActionItem@.
module ObjC.AVRouting.AVCustomRoutingActionItem
  ( AVCustomRoutingActionItem
  , IsAVCustomRoutingActionItem(..)


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

import ObjC.AVRouting.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

