{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSIndexPath@.
module ObjC.AppKit.NSIndexPath
  ( NSIndexPath
  , IsNSIndexPath(..)
  , indexPathForItem_inSection
  , item
  , section
  , indexPathForItem_inSectionSelector
  , itemSelector
  , sectionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ indexPathForItem:inSection:@
indexPathForItem_inSection :: CLong -> CLong -> IO (Id NSIndexPath)
indexPathForItem_inSection item section =
  do
    cls' <- getRequiredClass "NSIndexPath"
    sendClassMessage cls' indexPathForItem_inSectionSelector item section

-- | @- item@
item :: IsNSIndexPath nsIndexPath => nsIndexPath -> IO CLong
item nsIndexPath =
  sendMessage nsIndexPath itemSelector

-- | @- section@
section :: IsNSIndexPath nsIndexPath => nsIndexPath -> IO CLong
section nsIndexPath =
  sendMessage nsIndexPath sectionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @indexPathForItem:inSection:@
indexPathForItem_inSectionSelector :: Selector '[CLong, CLong] (Id NSIndexPath)
indexPathForItem_inSectionSelector = mkSelector "indexPathForItem:inSection:"

-- | @Selector@ for @item@
itemSelector :: Selector '[] CLong
itemSelector = mkSelector "item"

-- | @Selector@ for @section@
sectionSelector :: Selector '[] CLong
sectionSelector = mkSelector "section"

