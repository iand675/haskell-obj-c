{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GLKEffectPropertyFog@.
module ObjC.GLKit.GLKEffectPropertyFog
  ( GLKEffectPropertyFog
  , IsGLKEffectPropertyFog(..)
  , enabled
  , setEnabled
  , mode
  , setMode
  , density
  , setDensity
  , start
  , setStart
  , end
  , setEnd
  , enabledSelector
  , setEnabledSelector
  , modeSelector
  , setModeSelector
  , densitySelector
  , setDensitySelector
  , startSelector
  , setStartSelector
  , endSelector
  , setEndSelector


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

import ObjC.GLKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- enabled@
enabled :: IsGLKEffectPropertyFog glkEffectPropertyFog => glkEffectPropertyFog -> IO CUChar
enabled glkEffectPropertyFog  =
  sendMsg glkEffectPropertyFog (mkSelector "enabled") retCUChar []

-- | @- setEnabled:@
setEnabled :: IsGLKEffectPropertyFog glkEffectPropertyFog => glkEffectPropertyFog -> CUChar -> IO ()
setEnabled glkEffectPropertyFog  value =
  sendMsg glkEffectPropertyFog (mkSelector "setEnabled:") retVoid [argCUChar (fromIntegral value)]

-- | @- mode@
mode :: IsGLKEffectPropertyFog glkEffectPropertyFog => glkEffectPropertyFog -> IO CInt
mode glkEffectPropertyFog  =
  sendMsg glkEffectPropertyFog (mkSelector "mode") retCInt []

-- | @- setMode:@
setMode :: IsGLKEffectPropertyFog glkEffectPropertyFog => glkEffectPropertyFog -> CInt -> IO ()
setMode glkEffectPropertyFog  value =
  sendMsg glkEffectPropertyFog (mkSelector "setMode:") retVoid [argCInt (fromIntegral value)]

-- | @- density@
density :: IsGLKEffectPropertyFog glkEffectPropertyFog => glkEffectPropertyFog -> IO CFloat
density glkEffectPropertyFog  =
  sendMsg glkEffectPropertyFog (mkSelector "density") retCFloat []

-- | @- setDensity:@
setDensity :: IsGLKEffectPropertyFog glkEffectPropertyFog => glkEffectPropertyFog -> CFloat -> IO ()
setDensity glkEffectPropertyFog  value =
  sendMsg glkEffectPropertyFog (mkSelector "setDensity:") retVoid [argCFloat (fromIntegral value)]

-- | @- start@
start :: IsGLKEffectPropertyFog glkEffectPropertyFog => glkEffectPropertyFog -> IO CFloat
start glkEffectPropertyFog  =
  sendMsg glkEffectPropertyFog (mkSelector "start") retCFloat []

-- | @- setStart:@
setStart :: IsGLKEffectPropertyFog glkEffectPropertyFog => glkEffectPropertyFog -> CFloat -> IO ()
setStart glkEffectPropertyFog  value =
  sendMsg glkEffectPropertyFog (mkSelector "setStart:") retVoid [argCFloat (fromIntegral value)]

-- | @- end@
end :: IsGLKEffectPropertyFog glkEffectPropertyFog => glkEffectPropertyFog -> IO CFloat
end glkEffectPropertyFog  =
  sendMsg glkEffectPropertyFog (mkSelector "end") retCFloat []

-- | @- setEnd:@
setEnd :: IsGLKEffectPropertyFog glkEffectPropertyFog => glkEffectPropertyFog -> CFloat -> IO ()
setEnd glkEffectPropertyFog  value =
  sendMsg glkEffectPropertyFog (mkSelector "setEnd:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @mode@
modeSelector :: Selector
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @density@
densitySelector :: Selector
densitySelector = mkSelector "density"

-- | @Selector@ for @setDensity:@
setDensitySelector :: Selector
setDensitySelector = mkSelector "setDensity:"

-- | @Selector@ for @start@
startSelector :: Selector
startSelector = mkSelector "start"

-- | @Selector@ for @setStart:@
setStartSelector :: Selector
setStartSelector = mkSelector "setStart:"

-- | @Selector@ for @end@
endSelector :: Selector
endSelector = mkSelector "end"

-- | @Selector@ for @setEnd:@
setEndSelector :: Selector
setEndSelector = mkSelector "setEnd:"

