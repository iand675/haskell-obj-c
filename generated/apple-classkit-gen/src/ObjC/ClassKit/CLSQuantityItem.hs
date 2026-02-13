{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CLSQuantityItem represents user generated quantity information.
--
-- Generated bindings for @CLSQuantityItem@.
module ObjC.ClassKit.CLSQuantityItem
  ( CLSQuantityItem
  , IsCLSQuantityItem(..)
  , initWithIdentifier_title
  , quantity
  , setQuantity
  , initWithIdentifier_titleSelector
  , quantitySelector
  , setQuantitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ClassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a quantity item with an identifier and title.
--
-- @identifier@ — An identifier that is unique within activity.
--
-- @title@ — Title of the quantity. Ex /Hints/
--
-- ObjC selector: @- initWithIdentifier:title:@
initWithIdentifier_title :: (IsCLSQuantityItem clsQuantityItem, IsNSString identifier, IsNSString title) => clsQuantityItem -> identifier -> title -> IO (Id CLSQuantityItem)
initWithIdentifier_title clsQuantityItem identifier title =
  sendOwnedMessage clsQuantityItem initWithIdentifier_titleSelector (toNSString identifier) (toNSString title)

-- | Quantity awarded.
--
-- ObjC selector: @- quantity@
quantity :: IsCLSQuantityItem clsQuantityItem => clsQuantityItem -> IO CDouble
quantity clsQuantityItem =
  sendMessage clsQuantityItem quantitySelector

-- | Quantity awarded.
--
-- ObjC selector: @- setQuantity:@
setQuantity :: IsCLSQuantityItem clsQuantityItem => clsQuantityItem -> CDouble -> IO ()
setQuantity clsQuantityItem value =
  sendMessage clsQuantityItem setQuantitySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:title:@
initWithIdentifier_titleSelector :: Selector '[Id NSString, Id NSString] (Id CLSQuantityItem)
initWithIdentifier_titleSelector = mkSelector "initWithIdentifier:title:"

-- | @Selector@ for @quantity@
quantitySelector :: Selector '[] CDouble
quantitySelector = mkSelector "quantity"

-- | @Selector@ for @setQuantity:@
setQuantitySelector :: Selector '[CDouble] ()
setQuantitySelector = mkSelector "setQuantity:"

