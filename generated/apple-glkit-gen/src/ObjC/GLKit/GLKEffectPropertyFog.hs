{-# LANGUAGE DataKinds #-}
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
  , densitySelector
  , enabledSelector
  , endSelector
  , modeSelector
  , setDensitySelector
  , setEnabledSelector
  , setEndSelector
  , setModeSelector
  , setStartSelector
  , startSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GLKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- enabled@
enabled :: IsGLKEffectPropertyFog glkEffectPropertyFog => glkEffectPropertyFog -> IO CUChar
enabled glkEffectPropertyFog =
  sendMessage glkEffectPropertyFog enabledSelector

-- | @- setEnabled:@
setEnabled :: IsGLKEffectPropertyFog glkEffectPropertyFog => glkEffectPropertyFog -> CUChar -> IO ()
setEnabled glkEffectPropertyFog value =
  sendMessage glkEffectPropertyFog setEnabledSelector value

-- | @- mode@
mode :: IsGLKEffectPropertyFog glkEffectPropertyFog => glkEffectPropertyFog -> IO CInt
mode glkEffectPropertyFog =
  sendMessage glkEffectPropertyFog modeSelector

-- | @- setMode:@
setMode :: IsGLKEffectPropertyFog glkEffectPropertyFog => glkEffectPropertyFog -> CInt -> IO ()
setMode glkEffectPropertyFog value =
  sendMessage glkEffectPropertyFog setModeSelector value

-- | @- density@
density :: IsGLKEffectPropertyFog glkEffectPropertyFog => glkEffectPropertyFog -> IO CFloat
density glkEffectPropertyFog =
  sendMessage glkEffectPropertyFog densitySelector

-- | @- setDensity:@
setDensity :: IsGLKEffectPropertyFog glkEffectPropertyFog => glkEffectPropertyFog -> CFloat -> IO ()
setDensity glkEffectPropertyFog value =
  sendMessage glkEffectPropertyFog setDensitySelector value

-- | @- start@
start :: IsGLKEffectPropertyFog glkEffectPropertyFog => glkEffectPropertyFog -> IO CFloat
start glkEffectPropertyFog =
  sendMessage glkEffectPropertyFog startSelector

-- | @- setStart:@
setStart :: IsGLKEffectPropertyFog glkEffectPropertyFog => glkEffectPropertyFog -> CFloat -> IO ()
setStart glkEffectPropertyFog value =
  sendMessage glkEffectPropertyFog setStartSelector value

-- | @- end@
end :: IsGLKEffectPropertyFog glkEffectPropertyFog => glkEffectPropertyFog -> IO CFloat
end glkEffectPropertyFog =
  sendMessage glkEffectPropertyFog endSelector

-- | @- setEnd:@
setEnd :: IsGLKEffectPropertyFog glkEffectPropertyFog => glkEffectPropertyFog -> CFloat -> IO ()
setEnd glkEffectPropertyFog value =
  sendMessage glkEffectPropertyFog setEndSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] CUChar
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[CUChar] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @mode@
modeSelector :: Selector '[] CInt
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector '[CInt] ()
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @density@
densitySelector :: Selector '[] CFloat
densitySelector = mkSelector "density"

-- | @Selector@ for @setDensity:@
setDensitySelector :: Selector '[CFloat] ()
setDensitySelector = mkSelector "setDensity:"

-- | @Selector@ for @start@
startSelector :: Selector '[] CFloat
startSelector = mkSelector "start"

-- | @Selector@ for @setStart:@
setStartSelector :: Selector '[CFloat] ()
setStartSelector = mkSelector "setStart:"

-- | @Selector@ for @end@
endSelector :: Selector '[] CFloat
endSelector = mkSelector "end"

-- | @Selector@ for @setEnd:@
setEndSelector :: Selector '[CFloat] ()
setEndSelector = mkSelector "setEnd:"

