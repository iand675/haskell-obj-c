{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSViewLayoutRegion@.
module ObjC.AppKit.NSViewLayoutRegion
  ( NSViewLayoutRegion
  , IsNSViewLayoutRegion(..)
  , safeAreaLayoutRegionWithCornerAdaptation
  , marginsLayoutRegionWithCornerAdaptation
  , new
  , init_
  , initSelector
  , marginsLayoutRegionWithCornerAdaptationSelector
  , newSelector
  , safeAreaLayoutRegionWithCornerAdaptationSelector

  -- * Enum types
  , NSViewLayoutRegionAdaptivityAxis(NSViewLayoutRegionAdaptivityAxis)
  , pattern NSViewLayoutRegionAdaptivityAxisNone
  , pattern NSViewLayoutRegionAdaptivityAxisHorizontal
  , pattern NSViewLayoutRegionAdaptivityAxisVertical

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ safeAreaLayoutRegionWithCornerAdaptation:@
safeAreaLayoutRegionWithCornerAdaptation :: NSViewLayoutRegionAdaptivityAxis -> IO (Id NSViewLayoutRegion)
safeAreaLayoutRegionWithCornerAdaptation adaptivityAxis =
  do
    cls' <- getRequiredClass "NSViewLayoutRegion"
    sendClassMessage cls' safeAreaLayoutRegionWithCornerAdaptationSelector adaptivityAxis

-- | @+ marginsLayoutRegionWithCornerAdaptation:@
marginsLayoutRegionWithCornerAdaptation :: NSViewLayoutRegionAdaptivityAxis -> IO (Id NSViewLayoutRegion)
marginsLayoutRegionWithCornerAdaptation adaptivityAxis =
  do
    cls' <- getRequiredClass "NSViewLayoutRegion"
    sendClassMessage cls' marginsLayoutRegionWithCornerAdaptationSelector adaptivityAxis

-- | @+ new@
new :: IO (Id NSViewLayoutRegion)
new  =
  do
    cls' <- getRequiredClass "NSViewLayoutRegion"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsNSViewLayoutRegion nsViewLayoutRegion => nsViewLayoutRegion -> IO (Id NSViewLayoutRegion)
init_ nsViewLayoutRegion =
  sendOwnedMessage nsViewLayoutRegion initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @safeAreaLayoutRegionWithCornerAdaptation:@
safeAreaLayoutRegionWithCornerAdaptationSelector :: Selector '[NSViewLayoutRegionAdaptivityAxis] (Id NSViewLayoutRegion)
safeAreaLayoutRegionWithCornerAdaptationSelector = mkSelector "safeAreaLayoutRegionWithCornerAdaptation:"

-- | @Selector@ for @marginsLayoutRegionWithCornerAdaptation:@
marginsLayoutRegionWithCornerAdaptationSelector :: Selector '[NSViewLayoutRegionAdaptivityAxis] (Id NSViewLayoutRegion)
marginsLayoutRegionWithCornerAdaptationSelector = mkSelector "marginsLayoutRegionWithCornerAdaptation:"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSViewLayoutRegion)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSViewLayoutRegion)
initSelector = mkSelector "init"

