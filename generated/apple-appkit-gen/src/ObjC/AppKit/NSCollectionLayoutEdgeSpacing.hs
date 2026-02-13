{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionLayoutEdgeSpacing@.
module ObjC.AppKit.NSCollectionLayoutEdgeSpacing
  ( NSCollectionLayoutEdgeSpacing
  , IsNSCollectionLayoutEdgeSpacing(..)
  , spacingForLeading_top_trailing_bottom
  , init_
  , new
  , leading
  , top
  , trailing
  , bottom
  , bottomSelector
  , initSelector
  , leadingSelector
  , newSelector
  , spacingForLeading_top_trailing_bottomSelector
  , topSelector
  , trailingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ spacingForLeading:top:trailing:bottom:@
spacingForLeading_top_trailing_bottom :: (IsNSCollectionLayoutSpacing leading, IsNSCollectionLayoutSpacing top, IsNSCollectionLayoutSpacing trailing, IsNSCollectionLayoutSpacing bottom) => leading -> top -> trailing -> bottom -> IO (Id NSCollectionLayoutEdgeSpacing)
spacingForLeading_top_trailing_bottom leading top trailing bottom =
  do
    cls' <- getRequiredClass "NSCollectionLayoutEdgeSpacing"
    sendClassMessage cls' spacingForLeading_top_trailing_bottomSelector (toNSCollectionLayoutSpacing leading) (toNSCollectionLayoutSpacing top) (toNSCollectionLayoutSpacing trailing) (toNSCollectionLayoutSpacing bottom)

-- | @- init@
init_ :: IsNSCollectionLayoutEdgeSpacing nsCollectionLayoutEdgeSpacing => nsCollectionLayoutEdgeSpacing -> IO (Id NSCollectionLayoutEdgeSpacing)
init_ nsCollectionLayoutEdgeSpacing =
  sendOwnedMessage nsCollectionLayoutEdgeSpacing initSelector

-- | @+ new@
new :: IO (Id NSCollectionLayoutEdgeSpacing)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutEdgeSpacing"
    sendOwnedClassMessage cls' newSelector

-- | @- leading@
leading :: IsNSCollectionLayoutEdgeSpacing nsCollectionLayoutEdgeSpacing => nsCollectionLayoutEdgeSpacing -> IO (Id NSCollectionLayoutSpacing)
leading nsCollectionLayoutEdgeSpacing =
  sendMessage nsCollectionLayoutEdgeSpacing leadingSelector

-- | @- top@
top :: IsNSCollectionLayoutEdgeSpacing nsCollectionLayoutEdgeSpacing => nsCollectionLayoutEdgeSpacing -> IO (Id NSCollectionLayoutSpacing)
top nsCollectionLayoutEdgeSpacing =
  sendMessage nsCollectionLayoutEdgeSpacing topSelector

-- | @- trailing@
trailing :: IsNSCollectionLayoutEdgeSpacing nsCollectionLayoutEdgeSpacing => nsCollectionLayoutEdgeSpacing -> IO (Id NSCollectionLayoutSpacing)
trailing nsCollectionLayoutEdgeSpacing =
  sendMessage nsCollectionLayoutEdgeSpacing trailingSelector

-- | @- bottom@
bottom :: IsNSCollectionLayoutEdgeSpacing nsCollectionLayoutEdgeSpacing => nsCollectionLayoutEdgeSpacing -> IO (Id NSCollectionLayoutSpacing)
bottom nsCollectionLayoutEdgeSpacing =
  sendMessage nsCollectionLayoutEdgeSpacing bottomSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @spacingForLeading:top:trailing:bottom:@
spacingForLeading_top_trailing_bottomSelector :: Selector '[Id NSCollectionLayoutSpacing, Id NSCollectionLayoutSpacing, Id NSCollectionLayoutSpacing, Id NSCollectionLayoutSpacing] (Id NSCollectionLayoutEdgeSpacing)
spacingForLeading_top_trailing_bottomSelector = mkSelector "spacingForLeading:top:trailing:bottom:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSCollectionLayoutEdgeSpacing)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSCollectionLayoutEdgeSpacing)
newSelector = mkSelector "new"

-- | @Selector@ for @leading@
leadingSelector :: Selector '[] (Id NSCollectionLayoutSpacing)
leadingSelector = mkSelector "leading"

-- | @Selector@ for @top@
topSelector :: Selector '[] (Id NSCollectionLayoutSpacing)
topSelector = mkSelector "top"

-- | @Selector@ for @trailing@
trailingSelector :: Selector '[] (Id NSCollectionLayoutSpacing)
trailingSelector = mkSelector "trailing"

-- | @Selector@ for @bottom@
bottomSelector :: Selector '[] (Id NSCollectionLayoutSpacing)
bottomSelector = mkSelector "bottom"

