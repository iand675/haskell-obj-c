{-# LANGUAGE DataKinds #-}
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
  , emptySelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetTrackSegment avAssetTrackSegment => avAssetTrackSegment -> IO (Id AVAssetTrackSegment)
init_ avAssetTrackSegment =
  sendOwnedMessage avAssetTrackSegment initSelector

-- | @+ new@
new :: IO (Id AVAssetTrackSegment)
new  =
  do
    cls' <- getRequiredClass "AVAssetTrackSegment"
    sendOwnedClassMessage cls' newSelector

-- | @- empty@
empty :: IsAVAssetTrackSegment avAssetTrackSegment => avAssetTrackSegment -> IO Bool
empty avAssetTrackSegment =
  sendMessage avAssetTrackSegment emptySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetTrackSegment)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetTrackSegment)
newSelector = mkSelector "new"

-- | @Selector@ for @empty@
emptySelector :: Selector '[] Bool
emptySelector = mkSelector "empty"

