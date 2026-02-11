{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNProgram
--
-- A SCNProgram lets you specify custom shaders to use when rendering materials.
--
-- Generated bindings for @SCNProgram@.
module ObjC.SceneKit.SCNProgram
  ( SCNProgram
  , IsSCNProgram(..)
  , program
  , handleBindingOfBufferNamed_frequency_usingBlock
  , setSemantic_forSymbol_options
  , semanticForSymbol
  , vertexShader
  , setVertexShader
  , fragmentShader
  , setFragmentShader
  , vertexFunctionName
  , setVertexFunctionName
  , fragmentFunctionName
  , setFragmentFunctionName
  , opaque
  , setOpaque
  , programSelector
  , handleBindingOfBufferNamed_frequency_usingBlockSelector
  , setSemantic_forSymbol_optionsSelector
  , semanticForSymbolSelector
  , vertexShaderSelector
  , setVertexShaderSelector
  , fragmentShaderSelector
  , setFragmentShaderSelector
  , vertexFunctionNameSelector
  , setVertexFunctionNameSelector
  , fragmentFunctionNameSelector
  , setFragmentFunctionNameSelector
  , opaqueSelector
  , setOpaqueSelector

  -- * Enum types
  , SCNBufferFrequency(SCNBufferFrequency)
  , pattern SCNBufferFrequencyPerFrame
  , pattern SCNBufferFrequencyPerNode
  , pattern SCNBufferFrequencyPerShadable

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

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | program
--
-- Creates and initialize a program instance.
--
-- ObjC selector: @+ program@
program :: IO (Id SCNProgram)
program  =
  do
    cls' <- getRequiredClass "SCNProgram"
    sendClassMsg cls' (mkSelector "program") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | handleBindingOfBufferNamed:frequency:usingBlock:
--
-- Sets the block to call at render time to bind the buffer of the specified symbol of the receiver's program.
--
-- @name@ — The name of the buffer to bind.
--
-- @frequency@ — The frequency at which the block has to be invoked. Can be per frame, per node or per geometry or material. See SCNBufferBindingBlock above.
--
-- @block@ — The block that binds the buffer.
--
-- This method can only be used with Metal based programs.
--
-- ObjC selector: @- handleBindingOfBufferNamed:frequency:usingBlock:@
handleBindingOfBufferNamed_frequency_usingBlock :: (IsSCNProgram scnProgram, IsNSString name) => scnProgram -> name -> SCNBufferFrequency -> Ptr () -> IO ()
handleBindingOfBufferNamed_frequency_usingBlock scnProgram  name frequency block =
withObjCPtr name $ \raw_name ->
    sendMsg scnProgram (mkSelector "handleBindingOfBufferNamed:frequency:usingBlock:") retVoid [argPtr (castPtr raw_name :: Ptr ()), argCLong (coerce frequency), argPtr (castPtr block :: Ptr ())]

-- | setSemantic:forSymbol:options:
--
-- Associates a SceneKit semantic to a symbol.
--
-- @semantic@ — The SceneKit semantic to associate to the specified symbol.
--
-- @symbol@ — A symbol from the program source code.
--
-- @options@ — An optional dictionary. See the 'Semantic options' above.
--
-- Associates semantics handled by the SceneKit runtime to a symbol from the program. Supported semantics are listed in SCNGeometry.h and SCNNode.h.
--
-- ObjC selector: @- setSemantic:forSymbol:options:@
setSemantic_forSymbol_options :: (IsSCNProgram scnProgram, IsNSString semantic, IsNSString symbol, IsNSDictionary options) => scnProgram -> semantic -> symbol -> options -> IO ()
setSemantic_forSymbol_options scnProgram  semantic symbol options =
withObjCPtr semantic $ \raw_semantic ->
  withObjCPtr symbol $ \raw_symbol ->
    withObjCPtr options $ \raw_options ->
        sendMsg scnProgram (mkSelector "setSemantic:forSymbol:options:") retVoid [argPtr (castPtr raw_semantic :: Ptr ()), argPtr (castPtr raw_symbol :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())]

-- | semanticForSymbol:
--
-- Retrieves the SceneKit semantic associated to a symbol from the program source code.
--
-- @symbol@ — A symbol from the program source code.
--
-- ObjC selector: @- semanticForSymbol:@
semanticForSymbol :: (IsSCNProgram scnProgram, IsNSString symbol) => scnProgram -> symbol -> IO (Id NSString)
semanticForSymbol scnProgram  symbol =
withObjCPtr symbol $ \raw_symbol ->
    sendMsg scnProgram (mkSelector "semanticForSymbol:") (retPtr retVoid) [argPtr (castPtr raw_symbol :: Ptr ())] >>= retainedObject . castPtr

-- | vertexShader
--
-- Determines the receiver's vertex shader.
--
-- ObjC selector: @- vertexShader@
vertexShader :: IsSCNProgram scnProgram => scnProgram -> IO (Id NSString)
vertexShader scnProgram  =
  sendMsg scnProgram (mkSelector "vertexShader") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | vertexShader
--
-- Determines the receiver's vertex shader.
--
-- ObjC selector: @- setVertexShader:@
setVertexShader :: (IsSCNProgram scnProgram, IsNSString value) => scnProgram -> value -> IO ()
setVertexShader scnProgram  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnProgram (mkSelector "setVertexShader:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | fragmentShader
--
-- Determines the receiver's fragment shader.
--
-- ObjC selector: @- fragmentShader@
fragmentShader :: IsSCNProgram scnProgram => scnProgram -> IO (Id NSString)
fragmentShader scnProgram  =
  sendMsg scnProgram (mkSelector "fragmentShader") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | fragmentShader
--
-- Determines the receiver's fragment shader.
--
-- ObjC selector: @- setFragmentShader:@
setFragmentShader :: (IsSCNProgram scnProgram, IsNSString value) => scnProgram -> value -> IO ()
setFragmentShader scnProgram  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnProgram (mkSelector "setFragmentShader:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | vertexFunctionName
--
-- Determines the receiver's vertex function name.
--
-- The name of the vertex function (for Metal programs).
--
-- ObjC selector: @- vertexFunctionName@
vertexFunctionName :: IsSCNProgram scnProgram => scnProgram -> IO (Id NSString)
vertexFunctionName scnProgram  =
  sendMsg scnProgram (mkSelector "vertexFunctionName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | vertexFunctionName
--
-- Determines the receiver's vertex function name.
--
-- The name of the vertex function (for Metal programs).
--
-- ObjC selector: @- setVertexFunctionName:@
setVertexFunctionName :: (IsSCNProgram scnProgram, IsNSString value) => scnProgram -> value -> IO ()
setVertexFunctionName scnProgram  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnProgram (mkSelector "setVertexFunctionName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | fragmentFunctionName
--
-- Determines the receiver's fragment function name.
--
-- The name of the fragment function (for Metal programs).
--
-- ObjC selector: @- fragmentFunctionName@
fragmentFunctionName :: IsSCNProgram scnProgram => scnProgram -> IO (Id NSString)
fragmentFunctionName scnProgram  =
  sendMsg scnProgram (mkSelector "fragmentFunctionName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | fragmentFunctionName
--
-- Determines the receiver's fragment function name.
--
-- The name of the fragment function (for Metal programs).
--
-- ObjC selector: @- setFragmentFunctionName:@
setFragmentFunctionName :: (IsSCNProgram scnProgram, IsNSString value) => scnProgram -> value -> IO ()
setFragmentFunctionName scnProgram  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnProgram (mkSelector "setFragmentFunctionName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | opaque
--
-- Determines the receiver's fragment are opaque or not. Defaults to YES.
--
-- ObjC selector: @- opaque@
opaque :: IsSCNProgram scnProgram => scnProgram -> IO Bool
opaque scnProgram  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnProgram (mkSelector "opaque") retCULong []

-- | opaque
--
-- Determines the receiver's fragment are opaque or not. Defaults to YES.
--
-- ObjC selector: @- setOpaque:@
setOpaque :: IsSCNProgram scnProgram => scnProgram -> Bool -> IO ()
setOpaque scnProgram  value =
  sendMsg scnProgram (mkSelector "setOpaque:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @program@
programSelector :: Selector
programSelector = mkSelector "program"

-- | @Selector@ for @handleBindingOfBufferNamed:frequency:usingBlock:@
handleBindingOfBufferNamed_frequency_usingBlockSelector :: Selector
handleBindingOfBufferNamed_frequency_usingBlockSelector = mkSelector "handleBindingOfBufferNamed:frequency:usingBlock:"

-- | @Selector@ for @setSemantic:forSymbol:options:@
setSemantic_forSymbol_optionsSelector :: Selector
setSemantic_forSymbol_optionsSelector = mkSelector "setSemantic:forSymbol:options:"

-- | @Selector@ for @semanticForSymbol:@
semanticForSymbolSelector :: Selector
semanticForSymbolSelector = mkSelector "semanticForSymbol:"

-- | @Selector@ for @vertexShader@
vertexShaderSelector :: Selector
vertexShaderSelector = mkSelector "vertexShader"

-- | @Selector@ for @setVertexShader:@
setVertexShaderSelector :: Selector
setVertexShaderSelector = mkSelector "setVertexShader:"

-- | @Selector@ for @fragmentShader@
fragmentShaderSelector :: Selector
fragmentShaderSelector = mkSelector "fragmentShader"

-- | @Selector@ for @setFragmentShader:@
setFragmentShaderSelector :: Selector
setFragmentShaderSelector = mkSelector "setFragmentShader:"

-- | @Selector@ for @vertexFunctionName@
vertexFunctionNameSelector :: Selector
vertexFunctionNameSelector = mkSelector "vertexFunctionName"

-- | @Selector@ for @setVertexFunctionName:@
setVertexFunctionNameSelector :: Selector
setVertexFunctionNameSelector = mkSelector "setVertexFunctionName:"

-- | @Selector@ for @fragmentFunctionName@
fragmentFunctionNameSelector :: Selector
fragmentFunctionNameSelector = mkSelector "fragmentFunctionName"

-- | @Selector@ for @setFragmentFunctionName:@
setFragmentFunctionNameSelector :: Selector
setFragmentFunctionNameSelector = mkSelector "setFragmentFunctionName:"

-- | @Selector@ for @opaque@
opaqueSelector :: Selector
opaqueSelector = mkSelector "opaque"

-- | @Selector@ for @setOpaque:@
setOpaqueSelector :: Selector
setOpaqueSelector = mkSelector "setOpaque:"

