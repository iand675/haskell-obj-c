{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CATiledLayer@.
module ObjC.QuartzCore.CATiledLayer
  ( CATiledLayer
  , IsCATiledLayer(..)
  , fadeDuration
  , levelsOfDetail
  , setLevelsOfDetail
  , levelsOfDetailBias
  , setLevelsOfDetailBias
  , fadeDurationSelector
  , levelsOfDetailBiasSelector
  , levelsOfDetailSelector
  , setLevelsOfDetailBiasSelector
  , setLevelsOfDetailSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ fadeDuration@
fadeDuration :: IO CDouble
fadeDuration  =
  do
    cls' <- getRequiredClass "CATiledLayer"
    sendClassMessage cls' fadeDurationSelector

-- | @- levelsOfDetail@
levelsOfDetail :: IsCATiledLayer caTiledLayer => caTiledLayer -> IO CULong
levelsOfDetail caTiledLayer =
  sendMessage caTiledLayer levelsOfDetailSelector

-- | @- setLevelsOfDetail:@
setLevelsOfDetail :: IsCATiledLayer caTiledLayer => caTiledLayer -> CULong -> IO ()
setLevelsOfDetail caTiledLayer value =
  sendMessage caTiledLayer setLevelsOfDetailSelector value

-- | @- levelsOfDetailBias@
levelsOfDetailBias :: IsCATiledLayer caTiledLayer => caTiledLayer -> IO CULong
levelsOfDetailBias caTiledLayer =
  sendMessage caTiledLayer levelsOfDetailBiasSelector

-- | @- setLevelsOfDetailBias:@
setLevelsOfDetailBias :: IsCATiledLayer caTiledLayer => caTiledLayer -> CULong -> IO ()
setLevelsOfDetailBias caTiledLayer value =
  sendMessage caTiledLayer setLevelsOfDetailBiasSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fadeDuration@
fadeDurationSelector :: Selector '[] CDouble
fadeDurationSelector = mkSelector "fadeDuration"

-- | @Selector@ for @levelsOfDetail@
levelsOfDetailSelector :: Selector '[] CULong
levelsOfDetailSelector = mkSelector "levelsOfDetail"

-- | @Selector@ for @setLevelsOfDetail:@
setLevelsOfDetailSelector :: Selector '[CULong] ()
setLevelsOfDetailSelector = mkSelector "setLevelsOfDetail:"

-- | @Selector@ for @levelsOfDetailBias@
levelsOfDetailBiasSelector :: Selector '[] CULong
levelsOfDetailBiasSelector = mkSelector "levelsOfDetailBias"

-- | @Selector@ for @setLevelsOfDetailBias:@
setLevelsOfDetailBiasSelector :: Selector '[CULong] ()
setLevelsOfDetailBiasSelector = mkSelector "setLevelsOfDetailBias:"

