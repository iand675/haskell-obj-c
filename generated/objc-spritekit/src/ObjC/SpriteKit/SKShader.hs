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
  , initWithSourceSelector
  , initWithSource_uniformsSelector
  , shaderSelector
  , shaderWithSourceSelector
  , shaderWithSource_uniformsSelector
  , shaderWithFileNamedSelector
  , addUniformSelector
  , uniformNamedSelector
  , removeUniformNamedSelector
  , sourceSelector
  , setSourceSelector
  , uniformsSelector
  , setUniformsSelector
  , attributesSelector
  , setAttributesSelector


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

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a custom shader with source code.
--
-- @source@ — the source code for the custom fragment shader.
--
-- ObjC selector: @- initWithSource:@
initWithSource :: (IsSKShader skShader, IsNSString source) => skShader -> source -> IO (Id SKShader)
initWithSource skShader  source =
withObjCPtr source $ \raw_source ->
    sendMsg skShader (mkSelector "initWithSource:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ())] >>= ownedObject . castPtr

-- | Create a custom shader with source code and uniforms.
--
-- @source@ — the source code for the custom fragment shader.
--
-- @uniforms@ — the array of uniforms supplied to this shader
--
-- ObjC selector: @- initWithSource:uniforms:@
initWithSource_uniforms :: (IsSKShader skShader, IsNSString source, IsNSArray uniforms) => skShader -> source -> uniforms -> IO (Id SKShader)
initWithSource_uniforms skShader  source uniforms =
withObjCPtr source $ \raw_source ->
  withObjCPtr uniforms $ \raw_uniforms ->
      sendMsg skShader (mkSelector "initWithSource:uniforms:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argPtr (castPtr raw_uniforms :: Ptr ())] >>= ownedObject . castPtr

-- | @+ shader@
shader :: IO (Id SKShader)
shader  =
  do
    cls' <- getRequiredClass "SKShader"
    sendClassMsg cls' (mkSelector "shader") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ shaderWithSource:@
shaderWithSource :: IsNSString source => source -> IO (Id SKShader)
shaderWithSource source =
  do
    cls' <- getRequiredClass "SKShader"
    withObjCPtr source $ \raw_source ->
      sendClassMsg cls' (mkSelector "shaderWithSource:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ())] >>= retainedObject . castPtr

-- | @+ shaderWithSource:uniforms:@
shaderWithSource_uniforms :: (IsNSString source, IsNSArray uniforms) => source -> uniforms -> IO (Id SKShader)
shaderWithSource_uniforms source uniforms =
  do
    cls' <- getRequiredClass "SKShader"
    withObjCPtr source $ \raw_source ->
      withObjCPtr uniforms $ \raw_uniforms ->
        sendClassMsg cls' (mkSelector "shaderWithSource:uniforms:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argPtr (castPtr raw_uniforms :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "shaderWithFileNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- addUniform:@
addUniform :: (IsSKShader skShader, IsSKUniform uniform) => skShader -> uniform -> IO ()
addUniform skShader  uniform =
withObjCPtr uniform $ \raw_uniform ->
    sendMsg skShader (mkSelector "addUniform:") retVoid [argPtr (castPtr raw_uniform :: Ptr ())]

-- | @- uniformNamed:@
uniformNamed :: (IsSKShader skShader, IsNSString name) => skShader -> name -> IO (Id SKUniform)
uniformNamed skShader  name =
withObjCPtr name $ \raw_name ->
    sendMsg skShader (mkSelector "uniformNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- removeUniformNamed:@
removeUniformNamed :: (IsSKShader skShader, IsNSString name) => skShader -> name -> IO ()
removeUniformNamed skShader  name =
withObjCPtr name $ \raw_name ->
    sendMsg skShader (mkSelector "removeUniformNamed:") retVoid [argPtr (castPtr raw_name :: Ptr ())]

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
source skShader  =
  sendMsg skShader (mkSelector "source") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setSource skShader  value =
withObjCPtr value $ \raw_value ->
    sendMsg skShader (mkSelector "setSource:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | You may define additional uniforms to be used in your shader here. There is no need to declare them in you source, just use them by name.
--
-- All uniforms declared must be used within the source.
--
-- ObjC selector: @- uniforms@
uniforms :: IsSKShader skShader => skShader -> IO (Id NSArray)
uniforms skShader  =
  sendMsg skShader (mkSelector "uniforms") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | You may define additional uniforms to be used in your shader here. There is no need to declare them in you source, just use them by name.
--
-- All uniforms declared must be used within the source.
--
-- ObjC selector: @- setUniforms:@
setUniforms :: (IsSKShader skShader, IsNSArray value) => skShader -> value -> IO ()
setUniforms skShader  value =
withObjCPtr value $ \raw_value ->
    sendMsg skShader (mkSelector "setUniforms:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attributes@
attributes :: IsSKShader skShader => skShader -> IO (Id NSArray)
attributes skShader  =
  sendMsg skShader (mkSelector "attributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributes:@
setAttributes :: (IsSKShader skShader, IsNSArray value) => skShader -> value -> IO ()
setAttributes skShader  value =
withObjCPtr value $ \raw_value ->
    sendMsg skShader (mkSelector "setAttributes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @initWithSource:uniforms:@
initWithSource_uniformsSelector :: Selector
initWithSource_uniformsSelector = mkSelector "initWithSource:uniforms:"

-- | @Selector@ for @shader@
shaderSelector :: Selector
shaderSelector = mkSelector "shader"

-- | @Selector@ for @shaderWithSource:@
shaderWithSourceSelector :: Selector
shaderWithSourceSelector = mkSelector "shaderWithSource:"

-- | @Selector@ for @shaderWithSource:uniforms:@
shaderWithSource_uniformsSelector :: Selector
shaderWithSource_uniformsSelector = mkSelector "shaderWithSource:uniforms:"

-- | @Selector@ for @shaderWithFileNamed:@
shaderWithFileNamedSelector :: Selector
shaderWithFileNamedSelector = mkSelector "shaderWithFileNamed:"

-- | @Selector@ for @addUniform:@
addUniformSelector :: Selector
addUniformSelector = mkSelector "addUniform:"

-- | @Selector@ for @uniformNamed:@
uniformNamedSelector :: Selector
uniformNamedSelector = mkSelector "uniformNamed:"

-- | @Selector@ for @removeUniformNamed:@
removeUniformNamedSelector :: Selector
removeUniformNamedSelector = mkSelector "removeUniformNamed:"

-- | @Selector@ for @source@
sourceSelector :: Selector
sourceSelector = mkSelector "source"

-- | @Selector@ for @setSource:@
setSourceSelector :: Selector
setSourceSelector = mkSelector "setSource:"

-- | @Selector@ for @uniforms@
uniformsSelector :: Selector
uniformsSelector = mkSelector "uniforms"

-- | @Selector@ for @setUniforms:@
setUniformsSelector :: Selector
setUniformsSelector = mkSelector "setUniforms:"

-- | @Selector@ for @attributes@
attributesSelector :: Selector
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @setAttributes:@
setAttributesSelector :: Selector
setAttributesSelector = mkSelector "setAttributes:"

