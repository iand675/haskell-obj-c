{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INObjectCollection@.
module ObjC.Intents.INObjectCollection
  ( INObjectCollection
  , IsINObjectCollection(..)
  , initWithSections
  , initWithItems
  , init_
  , sections
  , allItems
  , usesIndexedCollation
  , setUsesIndexedCollation
  , allItemsSelector
  , initSelector
  , initWithItemsSelector
  , initWithSectionsSelector
  , sectionsSelector
  , setUsesIndexedCollationSelector
  , usesIndexedCollationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithSections:@
initWithSections :: (IsINObjectCollection inObjectCollection, IsNSArray sections) => inObjectCollection -> sections -> IO (Id INObjectCollection)
initWithSections inObjectCollection sections =
  sendOwnedMessage inObjectCollection initWithSectionsSelector (toNSArray sections)

-- | @- initWithItems:@
initWithItems :: (IsINObjectCollection inObjectCollection, IsNSArray items) => inObjectCollection -> items -> IO (Id INObjectCollection)
initWithItems inObjectCollection items =
  sendOwnedMessage inObjectCollection initWithItemsSelector (toNSArray items)

-- | @- init@
init_ :: IsINObjectCollection inObjectCollection => inObjectCollection -> IO (Id INObjectCollection)
init_ inObjectCollection =
  sendOwnedMessage inObjectCollection initSelector

-- | @- sections@
sections :: IsINObjectCollection inObjectCollection => inObjectCollection -> IO (Id NSArray)
sections inObjectCollection =
  sendMessage inObjectCollection sectionsSelector

-- | @- allItems@
allItems :: IsINObjectCollection inObjectCollection => inObjectCollection -> IO (Id NSArray)
allItems inObjectCollection =
  sendMessage inObjectCollection allItemsSelector

-- | @- usesIndexedCollation@
usesIndexedCollation :: IsINObjectCollection inObjectCollection => inObjectCollection -> IO Bool
usesIndexedCollation inObjectCollection =
  sendMessage inObjectCollection usesIndexedCollationSelector

-- | @- setUsesIndexedCollation:@
setUsesIndexedCollation :: IsINObjectCollection inObjectCollection => inObjectCollection -> Bool -> IO ()
setUsesIndexedCollation inObjectCollection value =
  sendMessage inObjectCollection setUsesIndexedCollationSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSections:@
initWithSectionsSelector :: Selector '[Id NSArray] (Id INObjectCollection)
initWithSectionsSelector = mkSelector "initWithSections:"

-- | @Selector@ for @initWithItems:@
initWithItemsSelector :: Selector '[Id NSArray] (Id INObjectCollection)
initWithItemsSelector = mkSelector "initWithItems:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INObjectCollection)
initSelector = mkSelector "init"

-- | @Selector@ for @sections@
sectionsSelector :: Selector '[] (Id NSArray)
sectionsSelector = mkSelector "sections"

-- | @Selector@ for @allItems@
allItemsSelector :: Selector '[] (Id NSArray)
allItemsSelector = mkSelector "allItems"

-- | @Selector@ for @usesIndexedCollation@
usesIndexedCollationSelector :: Selector '[] Bool
usesIndexedCollationSelector = mkSelector "usesIndexedCollation"

-- | @Selector@ for @setUsesIndexedCollation:@
setUsesIndexedCollationSelector :: Selector '[Bool] ()
setUsesIndexedCollationSelector = mkSelector "setUsesIndexedCollation:"

