{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKTileOverlayRenderer@.
module ObjC.MapKit.MKTileOverlayRenderer
  ( MKTileOverlayRenderer
  , IsMKTileOverlayRenderer(..)
  , initWithTileOverlay
  , reloadData
  , initWithTileOverlaySelector
  , reloadDataSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithTileOverlay:@
initWithTileOverlay :: (IsMKTileOverlayRenderer mkTileOverlayRenderer, IsMKTileOverlay overlay) => mkTileOverlayRenderer -> overlay -> IO (Id MKTileOverlayRenderer)
initWithTileOverlay mkTileOverlayRenderer overlay =
  sendOwnedMessage mkTileOverlayRenderer initWithTileOverlaySelector (toMKTileOverlay overlay)

-- | @- reloadData@
reloadData :: IsMKTileOverlayRenderer mkTileOverlayRenderer => mkTileOverlayRenderer -> IO ()
reloadData mkTileOverlayRenderer =
  sendMessage mkTileOverlayRenderer reloadDataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTileOverlay:@
initWithTileOverlaySelector :: Selector '[Id MKTileOverlay] (Id MKTileOverlayRenderer)
initWithTileOverlaySelector = mkSelector "initWithTileOverlay:"

-- | @Selector@ for @reloadData@
reloadDataSelector :: Selector '[] ()
reloadDataSelector = mkSelector "reloadData"

