{-# LANGUAGE DataKinds #-}
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
  , initSelector
  , maximumQuerySignatureDurationSelector
  , minimumQuerySignatureDurationSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ShazamKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id SHCatalog)
new  =
  do
    cls' <- getRequiredClass "SHCatalog"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsSHCatalog shCatalog => shCatalog -> IO (Id SHCatalog)
init_ shCatalog =
  sendOwnedMessage shCatalog initSelector

-- | The minimum duration of a query signature that you use to match reference signatures in the catalog.
--
-- ObjC selector: @- minimumQuerySignatureDuration@
minimumQuerySignatureDuration :: IsSHCatalog shCatalog => shCatalog -> IO CDouble
minimumQuerySignatureDuration shCatalog =
  sendMessage shCatalog minimumQuerySignatureDurationSelector

-- | The maximum duration of a query signature that you use to match reference signatures in the catalog.
--
-- ObjC selector: @- maximumQuerySignatureDuration@
maximumQuerySignatureDuration :: IsSHCatalog shCatalog => shCatalog -> IO CDouble
maximumQuerySignatureDuration shCatalog =
  sendMessage shCatalog maximumQuerySignatureDurationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SHCatalog)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SHCatalog)
initSelector = mkSelector "init"

-- | @Selector@ for @minimumQuerySignatureDuration@
minimumQuerySignatureDurationSelector :: Selector '[] CDouble
minimumQuerySignatureDurationSelector = mkSelector "minimumQuerySignatureDuration"

-- | @Selector@ for @maximumQuerySignatureDuration@
maximumQuerySignatureDurationSelector :: Selector '[] CDouble
maximumQuerySignatureDurationSelector = mkSelector "maximumQuerySignatureDuration"

