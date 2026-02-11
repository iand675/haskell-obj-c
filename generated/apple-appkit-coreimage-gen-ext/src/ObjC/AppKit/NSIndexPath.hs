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

import ObjC.AppKit.Internal.Classes

-- | @+ indexPathForItem:inSection:@
indexPathForItem_inSection :: CLong -> CLong -> IO RawId
indexPathForItem_inSection item section =
  do
    cls' <- getRequiredClass "NSIndexPath"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "indexPathForItem:inSection:") (retPtr retVoid) [argCLong item, argCLong section]

-- | @- item@
item :: IsNSIndexPath nsIndexPath => nsIndexPath -> IO CLong
item nsIndexPath  =
    sendMsg nsIndexPath (mkSelector "item") retCLong []

-- | @- section@
section :: IsNSIndexPath nsIndexPath => nsIndexPath -> IO CLong
section nsIndexPath  =
    sendMsg nsIndexPath (mkSelector "section") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @indexPathForItem:inSection:@
indexPathForItem_inSectionSelector :: Selector
indexPathForItem_inSectionSelector = mkSelector "indexPathForItem:inSection:"

-- | @Selector@ for @item@
itemSelector :: Selector
itemSelector = mkSelector "item"

-- | @Selector@ for @section@
sectionSelector :: Selector
sectionSelector = mkSelector "section"

