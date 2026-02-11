{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An abstract base class for storing reference signatures and their associated metadata.
--
-- This is the base class of your custom catalog.
--
-- Generated bindings for @SHCatalog@.
module ObjC.ShazamKit.SHCatalog
  ( SHCatalog
  , IsSHCatalog(..)
  , new
  , init_
  , minimumQuerySignatureDuration
  , maximumQuerySignatureDuration
  , newSelector
  , initSelector
  , minimumQuerySignatureDurationSelector
  , maximumQuerySignatureDurationSelector


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

import ObjC.ShazamKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id SHCatalog)
new  =
  do
    cls' <- getRequiredClass "SHCatalog"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSHCatalog shCatalog => shCatalog -> IO (Id SHCatalog)
init_ shCatalog  =
  sendMsg shCatalog (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The minimum duration of a query signature that you use to match reference signatures in the catalog.
--
-- ObjC selector: @- minimumQuerySignatureDuration@
minimumQuerySignatureDuration :: IsSHCatalog shCatalog => shCatalog -> IO CDouble
minimumQuerySignatureDuration shCatalog  =
  sendMsg shCatalog (mkSelector "minimumQuerySignatureDuration") retCDouble []

-- | The maximum duration of a query signature that you use to match reference signatures in the catalog.
--
-- ObjC selector: @- maximumQuerySignatureDuration@
maximumQuerySignatureDuration :: IsSHCatalog shCatalog => shCatalog -> IO CDouble
maximumQuerySignatureDuration shCatalog  =
  sendMsg shCatalog (mkSelector "maximumQuerySignatureDuration") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @minimumQuerySignatureDuration@
minimumQuerySignatureDurationSelector :: Selector
minimumQuerySignatureDurationSelector = mkSelector "minimumQuerySignatureDuration"

-- | @Selector@ for @maximumQuerySignatureDuration@
maximumQuerySignatureDurationSelector :: Selector
maximumQuerySignatureDurationSelector = mkSelector "maximumQuerySignatureDuration"

