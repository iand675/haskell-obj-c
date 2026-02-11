{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVAssetTrackSegment@.
module ObjC.AVFoundation.AVAssetTrackSegment
  ( AVAssetTrackSegment
  , IsAVAssetTrackSegment(..)
  , init_
  , new
  , empty
  , initSelector
  , newSelector
  , emptySelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetTrackSegment avAssetTrackSegment => avAssetTrackSegment -> IO (Id AVAssetTrackSegment)
init_ avAssetTrackSegment  =
  sendMsg avAssetTrackSegment (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetTrackSegment)
new  =
  do
    cls' <- getRequiredClass "AVAssetTrackSegment"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- empty@
empty :: IsAVAssetTrackSegment avAssetTrackSegment => avAssetTrackSegment -> IO Bool
empty avAssetTrackSegment  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAssetTrackSegment (mkSelector "empty") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @empty@
emptySelector :: Selector
emptySelector = mkSelector "empty"

