{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKShader@.
module ObjC.SpriteKit.SKShader
  ( SKShader
  , IsSKShader(..)
  , initWithSource
  , initWithSource_uniforms
  , shader
  , shaderWithSource
  , shaderWithSource_uniforms
  , shaderWithFileNamed
  , addUniform
  , uniformNamed
  , removeUniformNamed
  , source
  , setSource
  , uniforms
  , setUniforms
  , attributes
  , setAttributes
  , addUniformSelector
  , attributesSelector
  , initWithSourceSelector
  , initWithSource_uniformsSelector
  , removeUniformNamedSelector
  , setAttributesSelector
  , setSourceSelector
  , setUniformsSelector
  , shaderSelector
  , shaderWithFileNamedSelector
  , shaderWithSourceSelector
  , shaderWithSource_uniformsSelector
  , sourceSelector
  , uniformNamedSelector
  , uniformsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a custom shader with source code.
--
-- @source@ — the source code for the custom fragment shader.
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsSKShader skShader, IsNSString source) => skShader -> source -> IO (Id SKShader)
initWithSource skShader source =
  sendOwnedMessage skShader initWithSourceSelector (toNSString source)

-- | Create a custom shader with source code and uniforms.
--
-- @source@ — the source code for the custom fragment shader.
--
-- @uniforms@ — the array of uniforms supplied to this shader
--
-- ObjC selector: @- initWithSource:uniforms:@
initWithSource_uniforms :: (IsSKShader skShader, IsNSString source, IsNSArray uniforms) => skShader -> source -> uniforms -> IO (Id SKShader)
initWithSource_uniforms skShader source uniforms =
  sendOwnedMessage skShader initWithSource_uniformsSelector (toNSString source) (toNSArray uniforms)

-- | @+ shader@
shader :: IO (Id SKShader)
shader  =
  do
    cls' <- getRequiredClass "SKShader"
    sendClassMessage cls' shaderSelector

-- | @+ shaderWithSource:@
shaderWithSource :: IsNSString source => source -> IO (Id SKShader)
shaderWithSource source =
  do
    cls' <- getRequiredClass "SKShader"
    sendClassMessage cls' shaderWithSourceSelector (toNSString source)

-- | @+ shaderWithSource:uniforms:@
shaderWithSource_uniforms :: (IsNSString source, IsNSArray uniforms) => source -> uniforms -> IO (Id SKShader)
shaderWithSource_uniforms source uniforms =
  do
    cls' <- getRequiredClass "SKShader"
    sendClassMessage cls' shaderWithSource_uniformsSelector (toNSString source) (toNSArray uniforms)

-- | Loads a shader source file named 'name' from the main bundle. This is simpler yet functionally equivalent to the following code
--
-- [SKShader shaderWithSource:[NSString stringWithContentsOfFile:[[NSBundle mainBundle] pathForResource:name ofType:"fsh"]                   encoding:NSUTF8StringEncoding                      error:NULL]];
--
-- The encoding is assumed to be NSUTF8StringEncoding.
--
-- ObjC selector: @+ shaderWithFileNamed:@
shaderWithFileNamed :: IsNSString name => name -> IO (Id SKShader)
shaderWithFileNamed name =
  do
    cls' <- getRequiredClass "SKShader"
    sendClassMessage cls' shaderWithFileNamedSelector (toNSString name)

-- | @- addUniform:@
addUniform :: (IsSKShader skShader, IsSKUniform uniform) => skShader -> uniform -> IO ()
addUniform skShader uniform =
  sendMessage skShader addUniformSelector (toSKUniform uniform)

-- | @- uniformNamed:@
uniformNamed :: (IsSKShader skShader, IsNSString name) => skShader -> name -> IO (Id SKUniform)
uniformNamed skShader name =
  sendMessage skShader uniformNamedSelector (toNSString name)

-- | @- removeUniformNamed:@
removeUniformNamed :: (IsSKShader skShader, IsNSString name) => skShader -> name -> IO ()
removeUniformNamed skShader name =
  sendMessage skShader removeUniformNamedSelector (toNSString name)

-- | Shader source must define the 'main' method of the fragment shader
--
-- Your shader must assign a premultipled fragment value to 'gl_FragColor'
--
-- The following implicit uniforms are available:
--
-- 1. sampler2D u_texture  (the primary texuture attached the the sprite)
--
-- The following varyings are available:
--
-- 1. vec2 v_tex_coord  (normalized texture coordiantes for the primary texture)    2. vec4 v_color_mix  (premultiplied color value based on color & alpha)
--
-- The following functions are available:
--
-- 1. vec4 SKDefaultShading()  (returns the fragment value that would have been output if no shader was used)
--
-- Sample shader source that produces the same result are SpriteKit's normal rendering:
--
-- "void main() { gl_FragColor = SKDefaultShading(); }"
--
-- ObjC selector: @- source@
source :: IsSKShader skShader => skShader -> IO (Id NSString)
source skShader =
  sendMessage skShader sourceSelector

-- | Shader source must define the 'main' method of the fragment shader
--
-- Your shader must assign a premultipled fragment value to 'gl_FragColor'
--
-- The following implicit uniforms are available:
--
-- 1. sampler2D u_texture  (the primary texuture attached the the sprite)
--
-- The following varyings are available:
--
-- 1. vec2 v_tex_coord  (normalized texture coordiantes for the primary texture)    2. vec4 v_color_mix  (premultiplied color value based on color & alpha)
--
-- The following functions are available:
--
-- 1. vec4 SKDefaultShading()  (returns the fragment value that would have been output if no shader was used)
--
-- Sample shader source that produces the same result are SpriteKit's normal rendering:
--
-- "void main() { gl_FragColor = SKDefaultShading(); }"
--
-- ObjC selector: @- setSource:@
setSource :: (IsSKShader skShader, IsNSString value) => skShader -> value -> IO ()
setSource skShader value =
  sendMessage skShader setSourceSelector (toNSString value)

-- | You may define additional uniforms to be used in your shader here. There is no need to declare them in you source, just use them by name.
--
-- All uniforms declared must be used within the source.
--
-- ObjC selector: @- uniforms@
uniforms :: IsSKShader skShader => skShader -> IO (Id NSArray)
uniforms skShader =
  sendMessage skShader uniformsSelector

-- | You may define additional uniforms to be used in your shader here. There is no need to declare them in you source, just use them by name.
--
-- All uniforms declared must be used within the source.
--
-- ObjC selector: @- setUniforms:@
setUniforms :: (IsSKShader skShader, IsNSArray value) => skShader -> value -> IO ()
setUniforms skShader value =
  sendMessage skShader setUniformsSelector (toNSArray value)

-- | @- attributes@
attributes :: IsSKShader skShader => skShader -> IO (Id NSArray)
attributes skShader =
  sendMessage skShader attributesSelector

-- | @- setAttributes:@
setAttributes :: (IsSKShader skShader, IsNSArray value) => skShader -> value -> IO ()
setAttributes skShader value =
  sendMessage skShader setAttributesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id NSString] (Id SKShader)
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @initWithSource:uniforms:@
initWithSource_uniformsSelector :: Selector '[Id NSString, Id NSArray] (Id SKShader)
initWithSource_uniformsSelector = mkSelector "initWithSource:uniforms:"

-- | @Selector@ for @shader@
shaderSelector :: Selector '[] (Id SKShader)
shaderSelector = mkSelector "shader"

-- | @Selector@ for @shaderWithSource:@
shaderWithSourceSelector :: Selector '[Id NSString] (Id SKShader)
shaderWithSourceSelector = mkSelector "shaderWithSource:"

-- | @Selector@ for @shaderWithSource:uniforms:@
shaderWithSource_uniformsSelector :: Selector '[Id NSString, Id NSArray] (Id SKShader)
shaderWithSource_uniformsSelector = mkSelector "shaderWithSource:uniforms:"

-- | @Selector@ for @shaderWithFileNamed:@
shaderWithFileNamedSelector :: Selector '[Id NSString] (Id SKShader)
shaderWithFileNamedSelector = mkSelector "shaderWithFileNamed:"

-- | @Selector@ for @addUniform:@
addUniformSelector :: Selector '[Id SKUniform] ()
addUniformSelector = mkSelector "addUniform:"

-- | @Selector@ for @uniformNamed:@
uniformNamedSelector :: Selector '[Id NSString] (Id SKUniform)
uniformNamedSelector = mkSelector "uniformNamed:"

-- | @Selector@ for @removeUniformNamed:@
removeUniformNamedSelector :: Selector '[Id NSString] ()
removeUniformNamedSelector = mkSelector "removeUniformNamed:"

-- | @Selector@ for @source@
sourceSelector :: Selector '[] (Id NSString)
sourceSelector = mkSelector "source"

-- | @Selector@ for @setSource:@
setSourceSelector :: Selector '[Id NSString] ()
setSourceSelector = mkSelector "setSource:"

-- | @Selector@ for @uniforms@
uniformsSelector :: Selector '[] (Id NSArray)
uniformsSelector = mkSelector "uniforms"

-- | @Selector@ for @setUniforms:@
setUniformsSelector :: Selector '[Id NSArray] ()
setUniformsSelector = mkSelector "setUniforms:"

-- | @Selector@ for @attributes@
attributesSelector :: Selector '[] (Id NSArray)
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @setAttributes:@
setAttributesSelector :: Selector '[Id NSArray] ()
setAttributesSelector = mkSelector "setAttributes:"

