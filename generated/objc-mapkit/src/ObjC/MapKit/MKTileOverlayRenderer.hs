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

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithTileOverlay:@
initWithTileOverlay :: (IsMKTileOverlayRenderer mkTileOverlayRenderer, IsMKTileOverlay overlay) => mkTileOverlayRenderer -> overlay -> IO (Id MKTileOverlayRenderer)
initWithTileOverlay mkTileOverlayRenderer  overlay =
withObjCPtr overlay $ \raw_overlay ->
    sendMsg mkTileOverlayRenderer (mkSelector "initWithTileOverlay:") (retPtr retVoid) [argPtr (castPtr raw_overlay :: Ptr ())] >>= ownedObject . castPtr

-- | @- reloadData@
reloadData :: IsMKTileOverlayRenderer mkTileOverlayRenderer => mkTileOverlayRenderer -> IO ()
reloadData mkTileOverlayRenderer  =
  sendMsg mkTileOverlayRenderer (mkSelector "reloadData") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTileOverlay:@
initWithTileOverlaySelector :: Selector
initWithTileOverlaySelector = mkSelector "initWithTileOverlay:"

-- | @Selector@ for @reloadData@
reloadDataSelector :: Selector
reloadDataSelector = mkSelector "reloadData"

