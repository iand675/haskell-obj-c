{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMediaPlaybackClusterPlaybackPosition@.
module ObjC.Matter.MTRMediaPlaybackClusterPlaybackPosition
  ( MTRMediaPlaybackClusterPlaybackPosition
  , IsMTRMediaPlaybackClusterPlaybackPosition(..)
  , updatedAt
  , setUpdatedAt
  , position
  , setPosition
  , positionSelector
  , setPositionSelector
  , setUpdatedAtSelector
  , updatedAtSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- updatedAt@
updatedAt :: IsMTRMediaPlaybackClusterPlaybackPosition mtrMediaPlaybackClusterPlaybackPosition => mtrMediaPlaybackClusterPlaybackPosition -> IO (Id NSNumber)
updatedAt mtrMediaPlaybackClusterPlaybackPosition =
  sendMessage mtrMediaPlaybackClusterPlaybackPosition updatedAtSelector

-- | @- setUpdatedAt:@
setUpdatedAt :: (IsMTRMediaPlaybackClusterPlaybackPosition mtrMediaPlaybackClusterPlaybackPosition, IsNSNumber value) => mtrMediaPlaybackClusterPlaybackPosition -> value -> IO ()
setUpdatedAt mtrMediaPlaybackClusterPlaybackPosition value =
  sendMessage mtrMediaPlaybackClusterPlaybackPosition setUpdatedAtSelector (toNSNumber value)

-- | @- position@
position :: IsMTRMediaPlaybackClusterPlaybackPosition mtrMediaPlaybackClusterPlaybackPosition => mtrMediaPlaybackClusterPlaybackPosition -> IO (Id NSNumber)
position mtrMediaPlaybackClusterPlaybackPosition =
  sendMessage mtrMediaPlaybackClusterPlaybackPosition positionSelector

-- | @- setPosition:@
setPosition :: (IsMTRMediaPlaybackClusterPlaybackPosition mtrMediaPlaybackClusterPlaybackPosition, IsNSNumber value) => mtrMediaPlaybackClusterPlaybackPosition -> value -> IO ()
setPosition mtrMediaPlaybackClusterPlaybackPosition value =
  sendMessage mtrMediaPlaybackClusterPlaybackPosition setPositionSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updatedAt@
updatedAtSelector :: Selector '[] (Id NSNumber)
updatedAtSelector = mkSelector "updatedAt"

-- | @Selector@ for @setUpdatedAt:@
setUpdatedAtSelector :: Selector '[Id NSNumber] ()
setUpdatedAtSelector = mkSelector "setUpdatedAt:"

-- | @Selector@ for @position@
positionSelector :: Selector '[] (Id NSNumber)
positionSelector = mkSelector "position"

-- | @Selector@ for @setPosition:@
setPositionSelector :: Selector '[Id NSNumber] ()
setPositionSelector = mkSelector "setPosition:"

