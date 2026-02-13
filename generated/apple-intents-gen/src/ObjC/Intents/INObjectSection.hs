{-# LANGUAGE DataKinds #-}
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
  , initSelector
  , initWithTitle_itemsSelector
  , itemsSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithTitle:items:@
initWithTitle_items :: (IsINObjectSection inObjectSection, IsNSString title, IsNSArray items) => inObjectSection -> title -> items -> IO (Id INObjectSection)
initWithTitle_items inObjectSection title items =
  sendOwnedMessage inObjectSection initWithTitle_itemsSelector (toNSString title) (toNSArray items)

-- | @- init@
init_ :: IsINObjectSection inObjectSection => inObjectSection -> IO (Id INObjectSection)
init_ inObjectSection =
  sendOwnedMessage inObjectSection initSelector

-- | @- title@
title :: IsINObjectSection inObjectSection => inObjectSection -> IO (Id NSString)
title inObjectSection =
  sendMessage inObjectSection titleSelector

-- | @- items@
items :: IsINObjectSection inObjectSection => inObjectSection -> IO (Id NSArray)
items inObjectSection =
  sendMessage inObjectSection itemsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:items:@
initWithTitle_itemsSelector :: Selector '[Id NSString, Id NSArray] (Id INObjectSection)
initWithTitle_itemsSelector = mkSelector "initWithTitle:items:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INObjectSection)
initSelector = mkSelector "init"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @items@
itemsSelector :: Selector '[] (Id NSArray)
itemsSelector = mkSelector "items"

