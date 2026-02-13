{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMediaPlaybackClusterPlaybackPositionStruct@.
module ObjC.Matter.MTRMediaPlaybackClusterPlaybackPositionStruct
  ( MTRMediaPlaybackClusterPlaybackPositionStruct
  , IsMTRMediaPlaybackClusterPlaybackPositionStruct(..)
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
updatedAt :: IsMTRMediaPlaybackClusterPlaybackPositionStruct mtrMediaPlaybackClusterPlaybackPositionStruct => mtrMediaPlaybackClusterPlaybackPositionStruct -> IO (Id NSNumber)
updatedAt mtrMediaPlaybackClusterPlaybackPositionStruct =
  sendMessage mtrMediaPlaybackClusterPlaybackPositionStruct updatedAtSelector

-- | @- setUpdatedAt:@
setUpdatedAt :: (IsMTRMediaPlaybackClusterPlaybackPositionStruct mtrMediaPlaybackClusterPlaybackPositionStruct, IsNSNumber value) => mtrMediaPlaybackClusterPlaybackPositionStruct -> value -> IO ()
setUpdatedAt mtrMediaPlaybackClusterPlaybackPositionStruct value =
  sendMessage mtrMediaPlaybackClusterPlaybackPositionStruct setUpdatedAtSelector (toNSNumber value)

-- | @- position@
position :: IsMTRMediaPlaybackClusterPlaybackPositionStruct mtrMediaPlaybackClusterPlaybackPositionStruct => mtrMediaPlaybackClusterPlaybackPositionStruct -> IO (Id NSNumber)
position mtrMediaPlaybackClusterPlaybackPositionStruct =
  sendMessage mtrMediaPlaybackClusterPlaybackPositionStruct positionSelector

-- | @- setPosition:@
setPosition :: (IsMTRMediaPlaybackClusterPlaybackPositionStruct mtrMediaPlaybackClusterPlaybackPositionStruct, IsNSNumber value) => mtrMediaPlaybackClusterPlaybackPositionStruct -> value -> IO ()
setPosition mtrMediaPlaybackClusterPlaybackPositionStruct value =
  sendMessage mtrMediaPlaybackClusterPlaybackPositionStruct setPositionSelector (toNSNumber value)

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

