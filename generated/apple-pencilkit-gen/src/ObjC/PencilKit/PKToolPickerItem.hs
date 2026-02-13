{-# LANGUAGE DataKinds #-}
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
  , identifierSelector
  , initSelector
  , toolSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PencilKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPKToolPickerItem pkToolPickerItem => pkToolPickerItem -> IO (Id PKToolPickerItem)
init_ pkToolPickerItem =
  sendOwnedMessage pkToolPickerItem initSelector

-- | A string that identifies the item in the picker. For example, com.example.myapp.toolpicker.pencil. If multiple items with the same identifier are used to create the picker, only the first instance is used.
--
-- ObjC selector: @- identifier@
identifier :: IsPKToolPickerItem pkToolPickerItem => pkToolPickerItem -> IO (Id NSString)
identifier pkToolPickerItem =
  sendMessage pkToolPickerItem identifierSelector

-- | The @PKTool@ this tool picker item represents.
--
-- ObjC selector: @- tool@
tool :: IsPKToolPickerItem pkToolPickerItem => pkToolPickerItem -> IO (Id PKTool)
tool pkToolPickerItem =
  sendMessage pkToolPickerItem toolSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKToolPickerItem)
initSelector = mkSelector "init"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @tool@
toolSelector :: Selector '[] (Id PKTool)
toolSelector = mkSelector "tool"

