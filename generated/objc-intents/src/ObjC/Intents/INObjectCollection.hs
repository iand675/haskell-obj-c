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
  , initWithSectionsSelector
  , initWithItemsSelector
  , initSelector
  , sectionsSelector
  , allItemsSelector
  , usesIndexedCollationSelector
  , setUsesIndexedCollationSelector


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

-- | @- initWithSections:@
initWithSections :: (IsINObjectCollection inObjectCollection, IsNSArray sections) => inObjectCollection -> sections -> IO (Id INObjectCollection)
initWithSections inObjectCollection  sections =
withObjCPtr sections $ \raw_sections ->
    sendMsg inObjectCollection (mkSelector "initWithSections:") (retPtr retVoid) [argPtr (castPtr raw_sections :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithItems:@
initWithItems :: (IsINObjectCollection inObjectCollection, IsNSArray items) => inObjectCollection -> items -> IO (Id INObjectCollection)
initWithItems inObjectCollection  items =
withObjCPtr items $ \raw_items ->
    sendMsg inObjectCollection (mkSelector "initWithItems:") (retPtr retVoid) [argPtr (castPtr raw_items :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsINObjectCollection inObjectCollection => inObjectCollection -> IO (Id INObjectCollection)
init_ inObjectCollection  =
  sendMsg inObjectCollection (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- sections@
sections :: IsINObjectCollection inObjectCollection => inObjectCollection -> IO (Id NSArray)
sections inObjectCollection  =
  sendMsg inObjectCollection (mkSelector "sections") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- allItems@
allItems :: IsINObjectCollection inObjectCollection => inObjectCollection -> IO (Id NSArray)
allItems inObjectCollection  =
  sendMsg inObjectCollection (mkSelector "allItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- usesIndexedCollation@
usesIndexedCollation :: IsINObjectCollection inObjectCollection => inObjectCollection -> IO Bool
usesIndexedCollation inObjectCollection  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inObjectCollection (mkSelector "usesIndexedCollation") retCULong []

-- | @- setUsesIndexedCollation:@
setUsesIndexedCollation :: IsINObjectCollection inObjectCollection => inObjectCollection -> Bool -> IO ()
setUsesIndexedCollation inObjectCollection  value =
  sendMsg inObjectCollection (mkSelector "setUsesIndexedCollation:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSections:@
initWithSectionsSelector :: Selector
initWithSectionsSelector = mkSelector "initWithSections:"

-- | @Selector@ for @initWithItems:@
initWithItemsSelector :: Selector
initWithItemsSelector = mkSelector "initWithItems:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @sections@
sectionsSelector :: Selector
sectionsSelector = mkSelector "sections"

-- | @Selector@ for @allItems@
allItemsSelector :: Selector
allItemsSelector = mkSelector "allItems"

-- | @Selector@ for @usesIndexedCollation@
usesIndexedCollationSelector :: Selector
usesIndexedCollationSelector = mkSelector "usesIndexedCollation"

-- | @Selector@ for @setUsesIndexedCollation:@
setUsesIndexedCollationSelector :: Selector
setUsesIndexedCollationSelector = mkSelector "setUsesIndexedCollation:"

