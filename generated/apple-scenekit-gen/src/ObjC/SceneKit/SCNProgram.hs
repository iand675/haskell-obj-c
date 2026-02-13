{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , tessellationControlShader
  , setTessellationControlShader
  , tessellationEvaluationShader
  , setTessellationEvaluationShader
  , geometryShader
  , setGeometryShader
  , vertexFunctionName
  , setVertexFunctionName
  , fragmentFunctionName
  , setFragmentFunctionName
  , opaque
  , setOpaque
  , delegate
  , setDelegate
  , library
  , setLibrary
  , delegateSelector
  , fragmentFunctionNameSelector
  , fragmentShaderSelector
  , geometryShaderSelector
  , handleBindingOfBufferNamed_frequency_usingBlockSelector
  , librarySelector
  , opaqueSelector
  , programSelector
  , semanticForSymbolSelector
  , setDelegateSelector
  , setFragmentFunctionNameSelector
  , setFragmentShaderSelector
  , setGeometryShaderSelector
  , setLibrarySelector
  , setOpaqueSelector
  , setSemantic_forSymbol_optionsSelector
  , setTessellationControlShaderSelector
  , setTessellationEvaluationShaderSelector
  , setVertexFunctionNameSelector
  , setVertexShaderSelector
  , tessellationControlShaderSelector
  , tessellationEvaluationShaderSelector
  , vertexFunctionNameSelector
  , vertexShaderSelector

  -- * Enum types
  , SCNBufferFrequency(SCNBufferFrequency)
  , pattern SCNBufferFrequencyPerFrame
  , pattern SCNBufferFrequencyPerNode
  , pattern SCNBufferFrequencyPerShadable

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' programSelector

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
handleBindingOfBufferNamed_frequency_usingBlock scnProgram name frequency block =
  sendMessage scnProgram handleBindingOfBufferNamed_frequency_usingBlockSelector (toNSString name) frequency block

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
setSemantic_forSymbol_options scnProgram semantic symbol options =
  sendMessage scnProgram setSemantic_forSymbol_optionsSelector (toNSString semantic) (toNSString symbol) (toNSDictionary options)

-- | semanticForSymbol:
--
-- Retrieves the SceneKit semantic associated to a symbol from the program source code.
--
-- @symbol@ — A symbol from the program source code.
--
-- ObjC selector: @- semanticForSymbol:@
semanticForSymbol :: (IsSCNProgram scnProgram, IsNSString symbol) => scnProgram -> symbol -> IO (Id NSString)
semanticForSymbol scnProgram symbol =
  sendMessage scnProgram semanticForSymbolSelector (toNSString symbol)

-- | vertexShader
--
-- Determines the receiver's vertex shader.
--
-- ObjC selector: @- vertexShader@
vertexShader :: IsSCNProgram scnProgram => scnProgram -> IO (Id NSString)
vertexShader scnProgram =
  sendMessage scnProgram vertexShaderSelector

-- | vertexShader
--
-- Determines the receiver's vertex shader.
--
-- ObjC selector: @- setVertexShader:@
setVertexShader :: (IsSCNProgram scnProgram, IsNSString value) => scnProgram -> value -> IO ()
setVertexShader scnProgram value =
  sendMessage scnProgram setVertexShaderSelector (toNSString value)

-- | fragmentShader
--
-- Determines the receiver's fragment shader.
--
-- ObjC selector: @- fragmentShader@
fragmentShader :: IsSCNProgram scnProgram => scnProgram -> IO (Id NSString)
fragmentShader scnProgram =
  sendMessage scnProgram fragmentShaderSelector

-- | fragmentShader
--
-- Determines the receiver's fragment shader.
--
-- ObjC selector: @- setFragmentShader:@
setFragmentShader :: (IsSCNProgram scnProgram, IsNSString value) => scnProgram -> value -> IO ()
setFragmentShader scnProgram value =
  sendMessage scnProgram setFragmentShaderSelector (toNSString value)

-- | tessellationControlShader
--
-- Determines the receiver's tessellation control shader. Tessellation shaders require OpenGL Core Profile.
--
-- ObjC selector: @- tessellationControlShader@
tessellationControlShader :: IsSCNProgram scnProgram => scnProgram -> IO (Id NSString)
tessellationControlShader scnProgram =
  sendMessage scnProgram tessellationControlShaderSelector

-- | tessellationControlShader
--
-- Determines the receiver's tessellation control shader. Tessellation shaders require OpenGL Core Profile.
--
-- ObjC selector: @- setTessellationControlShader:@
setTessellationControlShader :: (IsSCNProgram scnProgram, IsNSString value) => scnProgram -> value -> IO ()
setTessellationControlShader scnProgram value =
  sendMessage scnProgram setTessellationControlShaderSelector (toNSString value)

-- | tessellationEvaluationShader
--
-- Determines the receiver's tessellation evaluation shader. Tessellation shaders require OpenGL Core Profile.
--
-- ObjC selector: @- tessellationEvaluationShader@
tessellationEvaluationShader :: IsSCNProgram scnProgram => scnProgram -> IO (Id NSString)
tessellationEvaluationShader scnProgram =
  sendMessage scnProgram tessellationEvaluationShaderSelector

-- | tessellationEvaluationShader
--
-- Determines the receiver's tessellation evaluation shader. Tessellation shaders require OpenGL Core Profile.
--
-- ObjC selector: @- setTessellationEvaluationShader:@
setTessellationEvaluationShader :: (IsSCNProgram scnProgram, IsNSString value) => scnProgram -> value -> IO ()
setTessellationEvaluationShader scnProgram value =
  sendMessage scnProgram setTessellationEvaluationShaderSelector (toNSString value)

-- | geometryShader
--
-- Determines the receiver's geometry shader. Geometry shaders require OpenGL Core Profile.
--
-- ObjC selector: @- geometryShader@
geometryShader :: IsSCNProgram scnProgram => scnProgram -> IO (Id NSString)
geometryShader scnProgram =
  sendMessage scnProgram geometryShaderSelector

-- | geometryShader
--
-- Determines the receiver's geometry shader. Geometry shaders require OpenGL Core Profile.
--
-- ObjC selector: @- setGeometryShader:@
setGeometryShader :: (IsSCNProgram scnProgram, IsNSString value) => scnProgram -> value -> IO ()
setGeometryShader scnProgram value =
  sendMessage scnProgram setGeometryShaderSelector (toNSString value)

-- | vertexFunctionName
--
-- Determines the receiver's vertex function name.
--
-- The name of the vertex function (for Metal programs).
--
-- ObjC selector: @- vertexFunctionName@
vertexFunctionName :: IsSCNProgram scnProgram => scnProgram -> IO (Id NSString)
vertexFunctionName scnProgram =
  sendMessage scnProgram vertexFunctionNameSelector

-- | vertexFunctionName
--
-- Determines the receiver's vertex function name.
--
-- The name of the vertex function (for Metal programs).
--
-- ObjC selector: @- setVertexFunctionName:@
setVertexFunctionName :: (IsSCNProgram scnProgram, IsNSString value) => scnProgram -> value -> IO ()
setVertexFunctionName scnProgram value =
  sendMessage scnProgram setVertexFunctionNameSelector (toNSString value)

-- | fragmentFunctionName
--
-- Determines the receiver's fragment function name.
--
-- The name of the fragment function (for Metal programs).
--
-- ObjC selector: @- fragmentFunctionName@
fragmentFunctionName :: IsSCNProgram scnProgram => scnProgram -> IO (Id NSString)
fragmentFunctionName scnProgram =
  sendMessage scnProgram fragmentFunctionNameSelector

-- | fragmentFunctionName
--
-- Determines the receiver's fragment function name.
--
-- The name of the fragment function (for Metal programs).
--
-- ObjC selector: @- setFragmentFunctionName:@
setFragmentFunctionName :: (IsSCNProgram scnProgram, IsNSString value) => scnProgram -> value -> IO ()
setFragmentFunctionName scnProgram value =
  sendMessage scnProgram setFragmentFunctionNameSelector (toNSString value)

-- | opaque
--
-- Determines the receiver's fragment are opaque or not. Defaults to YES.
--
-- ObjC selector: @- opaque@
opaque :: IsSCNProgram scnProgram => scnProgram -> IO Bool
opaque scnProgram =
  sendMessage scnProgram opaqueSelector

-- | opaque
--
-- Determines the receiver's fragment are opaque or not. Defaults to YES.
--
-- ObjC selector: @- setOpaque:@
setOpaque :: IsSCNProgram scnProgram => scnProgram -> Bool -> IO ()
setOpaque scnProgram value =
  sendMessage scnProgram setOpaqueSelector value

-- | delegate
--
-- Determines the receiver's delegate
--
-- ObjC selector: @- delegate@
delegate :: IsSCNProgram scnProgram => scnProgram -> IO RawId
delegate scnProgram =
  sendMessage scnProgram delegateSelector

-- | delegate
--
-- Determines the receiver's delegate
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsSCNProgram scnProgram => scnProgram -> RawId -> IO ()
setDelegate scnProgram value =
  sendMessage scnProgram setDelegateSelector value

-- | library
--
-- Specifies the Metal library to use to locate the function names specified above.
--
-- If set to nil the default library is used. Defaults to nil.
--
-- ObjC selector: @- library@
library :: IsSCNProgram scnProgram => scnProgram -> IO RawId
library scnProgram =
  sendMessage scnProgram librarySelector

-- | library
--
-- Specifies the Metal library to use to locate the function names specified above.
--
-- If set to nil the default library is used. Defaults to nil.
--
-- ObjC selector: @- setLibrary:@
setLibrary :: IsSCNProgram scnProgram => scnProgram -> RawId -> IO ()
setLibrary scnProgram value =
  sendMessage scnProgram setLibrarySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @program@
programSelector :: Selector '[] (Id SCNProgram)
programSelector = mkSelector "program"

-- | @Selector@ for @handleBindingOfBufferNamed:frequency:usingBlock:@
handleBindingOfBufferNamed_frequency_usingBlockSelector :: Selector '[Id NSString, SCNBufferFrequency, Ptr ()] ()
handleBindingOfBufferNamed_frequency_usingBlockSelector = mkSelector "handleBindingOfBufferNamed:frequency:usingBlock:"

-- | @Selector@ for @setSemantic:forSymbol:options:@
setSemantic_forSymbol_optionsSelector :: Selector '[Id NSString, Id NSString, Id NSDictionary] ()
setSemantic_forSymbol_optionsSelector = mkSelector "setSemantic:forSymbol:options:"

-- | @Selector@ for @semanticForSymbol:@
semanticForSymbolSelector :: Selector '[Id NSString] (Id NSString)
semanticForSymbolSelector = mkSelector "semanticForSymbol:"

-- | @Selector@ for @vertexShader@
vertexShaderSelector :: Selector '[] (Id NSString)
vertexShaderSelector = mkSelector "vertexShader"

-- | @Selector@ for @setVertexShader:@
setVertexShaderSelector :: Selector '[Id NSString] ()
setVertexShaderSelector = mkSelector "setVertexShader:"

-- | @Selector@ for @fragmentShader@
fragmentShaderSelector :: Selector '[] (Id NSString)
fragmentShaderSelector = mkSelector "fragmentShader"

-- | @Selector@ for @setFragmentShader:@
setFragmentShaderSelector :: Selector '[Id NSString] ()
setFragmentShaderSelector = mkSelector "setFragmentShader:"

-- | @Selector@ for @tessellationControlShader@
tessellationControlShaderSelector :: Selector '[] (Id NSString)
tessellationControlShaderSelector = mkSelector "tessellationControlShader"

-- | @Selector@ for @setTessellationControlShader:@
setTessellationControlShaderSelector :: Selector '[Id NSString] ()
setTessellationControlShaderSelector = mkSelector "setTessellationControlShader:"

-- | @Selector@ for @tessellationEvaluationShader@
tessellationEvaluationShaderSelector :: Selector '[] (Id NSString)
tessellationEvaluationShaderSelector = mkSelector "tessellationEvaluationShader"

-- | @Selector@ for @setTessellationEvaluationShader:@
setTessellationEvaluationShaderSelector :: Selector '[Id NSString] ()
setTessellationEvaluationShaderSelector = mkSelector "setTessellationEvaluationShader:"

-- | @Selector@ for @geometryShader@
geometryShaderSelector :: Selector '[] (Id NSString)
geometryShaderSelector = mkSelector "geometryShader"

-- | @Selector@ for @setGeometryShader:@
setGeometryShaderSelector :: Selector '[Id NSString] ()
setGeometryShaderSelector = mkSelector "setGeometryShader:"

-- | @Selector@ for @vertexFunctionName@
vertexFunctionNameSelector :: Selector '[] (Id NSString)
vertexFunctionNameSelector = mkSelector "vertexFunctionName"

-- | @Selector@ for @setVertexFunctionName:@
setVertexFunctionNameSelector :: Selector '[Id NSString] ()
setVertexFunctionNameSelector = mkSelector "setVertexFunctionName:"

-- | @Selector@ for @fragmentFunctionName@
fragmentFunctionNameSelector :: Selector '[] (Id NSString)
fragmentFunctionNameSelector = mkSelector "fragmentFunctionName"

-- | @Selector@ for @setFragmentFunctionName:@
setFragmentFunctionNameSelector :: Selector '[Id NSString] ()
setFragmentFunctionNameSelector = mkSelector "setFragmentFunctionName:"

-- | @Selector@ for @opaque@
opaqueSelector :: Selector '[] Bool
opaqueSelector = mkSelector "opaque"

-- | @Selector@ for @setOpaque:@
setOpaqueSelector :: Selector '[Bool] ()
setOpaqueSelector = mkSelector "setOpaque:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @library@
librarySelector :: Selector '[] RawId
librarySelector = mkSelector "library"

-- | @Selector@ for @setLibrary:@
setLibrarySelector :: Selector '[RawId] ()
setLibrarySelector = mkSelector "setLibrary:"

