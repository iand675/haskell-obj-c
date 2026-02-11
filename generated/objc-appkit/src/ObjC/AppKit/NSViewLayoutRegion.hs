{-# LANGUAGE PatternSynonyms #-}
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
  , safeAreaLayoutRegionWithCornerAdaptationSelector
  , marginsLayoutRegionWithCornerAdaptationSelector
  , newSelector
  , initSelector

  -- * Enum types
  , NSViewLayoutRegionAdaptivityAxis(NSViewLayoutRegionAdaptivityAxis)
  , pattern NSViewLayoutRegionAdaptivityAxisNone
  , pattern NSViewLayoutRegionAdaptivityAxisHorizontal
  , pattern NSViewLayoutRegionAdaptivityAxisVertical

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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ safeAreaLayoutRegionWithCornerAdaptation:@
safeAreaLayoutRegionWithCornerAdaptation :: NSViewLayoutRegionAdaptivityAxis -> IO (Id NSViewLayoutRegion)
safeAreaLayoutRegionWithCornerAdaptation adaptivityAxis =
  do
    cls' <- getRequiredClass "NSViewLayoutRegion"
    sendClassMsg cls' (mkSelector "safeAreaLayoutRegionWithCornerAdaptation:") (retPtr retVoid) [argCLong (coerce adaptivityAxis)] >>= retainedObject . castPtr

-- | @+ marginsLayoutRegionWithCornerAdaptation:@
marginsLayoutRegionWithCornerAdaptation :: NSViewLayoutRegionAdaptivityAxis -> IO (Id NSViewLayoutRegion)
marginsLayoutRegionWithCornerAdaptation adaptivityAxis =
  do
    cls' <- getRequiredClass "NSViewLayoutRegion"
    sendClassMsg cls' (mkSelector "marginsLayoutRegionWithCornerAdaptation:") (retPtr retVoid) [argCLong (coerce adaptivityAxis)] >>= retainedObject . castPtr

-- | @+ new@
new :: IO (Id NSViewLayoutRegion)
new  =
  do
    cls' <- getRequiredClass "NSViewLayoutRegion"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSViewLayoutRegion nsViewLayoutRegion => nsViewLayoutRegion -> IO (Id NSViewLayoutRegion)
init_ nsViewLayoutRegion  =
  sendMsg nsViewLayoutRegion (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @safeAreaLayoutRegionWithCornerAdaptation:@
safeAreaLayoutRegionWithCornerAdaptationSelector :: Selector
safeAreaLayoutRegionWithCornerAdaptationSelector = mkSelector "safeAreaLayoutRegionWithCornerAdaptation:"

-- | @Selector@ for @marginsLayoutRegionWithCornerAdaptation:@
marginsLayoutRegionWithCornerAdaptationSelector :: Selector
marginsLayoutRegionWithCornerAdaptationSelector = mkSelector "marginsLayoutRegionWithCornerAdaptation:"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

