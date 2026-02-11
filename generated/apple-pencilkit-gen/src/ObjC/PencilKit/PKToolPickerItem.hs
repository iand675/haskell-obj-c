{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A user interface for a tool item in PKToolPicker.
--
-- Generated bindings for @PKToolPickerItem@.
module ObjC.PencilKit.PKToolPickerItem
  ( PKToolPickerItem
  , IsPKToolPickerItem(..)
  , init_
  , identifier
  , tool
  , initSelector
  , identifierSelector
  , toolSelector


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

import ObjC.PencilKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPKToolPickerItem pkToolPickerItem => pkToolPickerItem -> IO (Id PKToolPickerItem)
init_ pkToolPickerItem  =
    sendMsg pkToolPickerItem (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | A string that identifies the item in the picker. For example, com.example.myapp.toolpicker.pencil. If multiple items with the same identifier are used to create the picker, only the first instance is used.
--
-- ObjC selector: @- identifier@
identifier :: IsPKToolPickerItem pkToolPickerItem => pkToolPickerItem -> IO (Id NSString)
identifier pkToolPickerItem  =
    sendMsg pkToolPickerItem (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The @PKTool@ this tool picker item represents.
--
-- ObjC selector: @- tool@
tool :: IsPKToolPickerItem pkToolPickerItem => pkToolPickerItem -> IO (Id PKTool)
tool pkToolPickerItem  =
    sendMsg pkToolPickerItem (mkSelector "tool") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @tool@
toolSelector :: Selector
toolSelector = mkSelector "tool"

