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
initWithIdentifier_title clsQuantityItem  identifier title =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr title $ \raw_title ->
      sendMsg clsQuantityItem (mkSelector "initWithIdentifier:title:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_title :: Ptr ())] >>= ownedObject . castPtr

-- | Quantity awarded.
--
-- ObjC selector: @- quantity@
quantity :: IsCLSQuantityItem clsQuantityItem => clsQuantityItem -> IO CDouble
quantity clsQuantityItem  =
  sendMsg clsQuantityItem (mkSelector "quantity") retCDouble []

-- | Quantity awarded.
--
-- ObjC selector: @- setQuantity:@
setQuantity :: IsCLSQuantityItem clsQuantityItem => clsQuantityItem -> CDouble -> IO ()
setQuantity clsQuantityItem  value =
  sendMsg clsQuantityItem (mkSelector "setQuantity:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:title:@
initWithIdentifier_titleSelector :: Selector
initWithIdentifier_titleSelector = mkSelector "initWithIdentifier:title:"

-- | @Selector@ for @quantity@
quantitySelector :: Selector
quantitySelector = mkSelector "quantity"

-- | @Selector@ for @setQuantity:@
setQuantitySelector :: Selector
setQuantitySelector = mkSelector "setQuantity:"

