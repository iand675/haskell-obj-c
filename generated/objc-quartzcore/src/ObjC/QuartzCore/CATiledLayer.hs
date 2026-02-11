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
  , levelsOfDetailSelector
  , setLevelsOfDetailSelector
  , levelsOfDetailBiasSelector
  , setLevelsOfDetailBiasSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ fadeDuration@
fadeDuration :: IO CDouble
fadeDuration  =
  do
    cls' <- getRequiredClass "CATiledLayer"
    sendClassMsg cls' (mkSelector "fadeDuration") retCDouble []

-- | @- levelsOfDetail@
levelsOfDetail :: IsCATiledLayer caTiledLayer => caTiledLayer -> IO CULong
levelsOfDetail caTiledLayer  =
  sendMsg caTiledLayer (mkSelector "levelsOfDetail") retCULong []

-- | @- setLevelsOfDetail:@
setLevelsOfDetail :: IsCATiledLayer caTiledLayer => caTiledLayer -> CULong -> IO ()
setLevelsOfDetail caTiledLayer  value =
  sendMsg caTiledLayer (mkSelector "setLevelsOfDetail:") retVoid [argCULong (fromIntegral value)]

-- | @- levelsOfDetailBias@
levelsOfDetailBias :: IsCATiledLayer caTiledLayer => caTiledLayer -> IO CULong
levelsOfDetailBias caTiledLayer  =
  sendMsg caTiledLayer (mkSelector "levelsOfDetailBias") retCULong []

-- | @- setLevelsOfDetailBias:@
setLevelsOfDetailBias :: IsCATiledLayer caTiledLayer => caTiledLayer -> CULong -> IO ()
setLevelsOfDetailBias caTiledLayer  value =
  sendMsg caTiledLayer (mkSelector "setLevelsOfDetailBias:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fadeDuration@
fadeDurationSelector :: Selector
fadeDurationSelector = mkSelector "fadeDuration"

-- | @Selector@ for @levelsOfDetail@
levelsOfDetailSelector :: Selector
levelsOfDetailSelector = mkSelector "levelsOfDetail"

-- | @Selector@ for @setLevelsOfDetail:@
setLevelsOfDetailSelector :: Selector
setLevelsOfDetailSelector = mkSelector "setLevelsOfDetail:"

-- | @Selector@ for @levelsOfDetailBias@
levelsOfDetailBiasSelector :: Selector
levelsOfDetailBiasSelector = mkSelector "levelsOfDetailBias"

-- | @Selector@ for @setLevelsOfDetailBias:@
setLevelsOfDetailBiasSelector :: Selector
setLevelsOfDetailBiasSelector = mkSelector "setLevelsOfDetailBias:"

