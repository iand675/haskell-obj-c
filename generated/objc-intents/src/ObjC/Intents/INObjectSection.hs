{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INObjectSection@.
module ObjC.Intents.INObjectSection
  ( INObjectSection
  , IsINObjectSection(..)
  , initWithTitle_items
  , init_
  , title
  , items
  , initWithTitle_itemsSelector
  , initSelector
  , titleSelector
  , itemsSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithTitle:items:@
initWithTitle_items :: (IsINObjectSection inObjectSection, IsNSString title, IsNSArray items) => inObjectSection -> title -> items -> IO (Id INObjectSection)
initWithTitle_items inObjectSection  title items =
withObjCPtr title $ \raw_title ->
  withObjCPtr items $ \raw_items ->
      sendMsg inObjectSection (mkSelector "initWithTitle:items:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_items :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsINObjectSection inObjectSection => inObjectSection -> IO (Id INObjectSection)
init_ inObjectSection  =
  sendMsg inObjectSection (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- title@
title :: IsINObjectSection inObjectSection => inObjectSection -> IO (Id NSString)
title inObjectSection  =
  sendMsg inObjectSection (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- items@
items :: IsINObjectSection inObjectSection => inObjectSection -> IO (Id NSArray)
items inObjectSection  =
  sendMsg inObjectSection (mkSelector "items") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:items:@
initWithTitle_itemsSelector :: Selector
initWithTitle_itemsSelector = mkSelector "initWithTitle:items:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @items@
itemsSelector :: Selector
itemsSelector = mkSelector "items"

