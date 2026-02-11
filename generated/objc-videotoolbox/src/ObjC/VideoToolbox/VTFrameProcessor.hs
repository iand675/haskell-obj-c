{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Provides a unified interface you can use to apply video effects to frames.
--
-- The VTFrameProcessor gives access to a set of powerful video processing implementation suitable for different use cases. A configuration object (conforming to the ``VTFrameProcessorConfiguration`` protocol) passes initialization and configuration parameters for the processor. A Parameter object (conforming to the ``VTFrameProcessorParameters`` protocol) provides the parameters for each individual processing operation. A Configuration object and a Parameter object define each processor implementation. These Configuration and Parameters objects for each implementation are defined in a processor-specific header file.
--
-- Use an instance of this class to apply configured video effects either directly to pixel buffers or as a part of Metal pipeline. The video effect must be specified as a ``VTFrameProcessorConfiguration`` instance at session startup. Once a session is started, you need to call one of the process methods for each input frame. After all input frames have been provided, session must be ended for the system to finish all pending processing.
--
-- After you call the process function, you must not modify input and output buffers (including attachments) before the function returns or the system receives the callback, in the case of asynchronous processing.
--
-- Generated bindings for @VTFrameProcessor@.
module ObjC.VideoToolbox.VTFrameProcessor
  ( VTFrameProcessor
  , IsVTFrameProcessor(..)
  , init_
  , startSessionWithConfiguration_error
  , processWithParameters_error
  , processWithCommandBuffer_parameters
  , endSession
  , initSelector
  , startSessionWithConfiguration_errorSelector
  , processWithParameters_errorSelector
  , processWithCommandBuffer_parametersSelector
  , endSessionSelector


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

import ObjC.VideoToolbox.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a new instance of the frame processor.
--
-- ObjC selector: @- init@
init_ :: IsVTFrameProcessor vtFrameProcessor => vtFrameProcessor -> IO (Id VTFrameProcessor)
init_ vtFrameProcessor  =
  sendMsg vtFrameProcessor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Starts a new session and configures the processor pipeline for an effect.
--
-- - Parameters:   - configuration: The system uses this parameter to create an effect pipeline for processing frames. This object       must conform to the ``VTFrameProcessorConfiguration`` interface.   - error: Contains error information if any. You may specify NULL for this parameter if you do not want the error information.
--
-- ObjC selector: @- startSessionWithConfiguration:error:@
startSessionWithConfiguration_error :: (IsVTFrameProcessor vtFrameProcessor, IsNSError error_) => vtFrameProcessor -> RawId -> error_ -> IO Bool
startSessionWithConfiguration_error vtFrameProcessor  configuration error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg vtFrameProcessor (mkSelector "startSessionWithConfiguration:error:") retCULong [argPtr (castPtr (unRawId configuration) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Synchronously performs the processor effects.
--
-- Use the respective ``VTFrameProcessorParameters`` to pass frame level settings and frame level input/output parameters for the effect that you configured this session for by calling ``startSessionWithConfiguration:error``.
--
-- - Parameters:   - parameters: A @VTFrameProcessorParameters@ based object to specify additional frame based parameters to use     during processing. It needs to match the configuration type used during start session.   - error: Contains error information if any. You may specify NULL for this parameter if you do not want the error information.
--
-- ObjC selector: @- processWithParameters:error:@
processWithParameters_error :: (IsVTFrameProcessor vtFrameProcessor, IsNSError error_) => vtFrameProcessor -> RawId -> error_ -> IO Bool
processWithParameters_error vtFrameProcessor  parameters error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg vtFrameProcessor (mkSelector "processWithParameters:error:") retCULong [argPtr (castPtr (unRawId parameters) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Performs effects in a Metal command buffer.
--
-- This function allows you to add the effect to an existing Metal command buffer. The clients that have an existing Metal pipeline and want to add this effect to it can use this function.
--
-- > Note: this function waits until all previously inserted tasks in the command buffer finish before running. Tasks inserted after the @processWithCommandBuffer@ returns are run by the system after the effect is applied. Processing does not happen until the commandBuffer is executed.
--
-- - Parameters:   - commandBuffer: An existing Metal command buffer where the frame processing is inserted.   - parameters: A @VTFrameProcessorParameters@ based object to specify additional frame based parameters to use       during processing. It needs to match the configuration type used during start session.
--
-- ObjC selector: @- processWithCommandBuffer:parameters:@
processWithCommandBuffer_parameters :: IsVTFrameProcessor vtFrameProcessor => vtFrameProcessor -> RawId -> RawId -> IO ()
processWithCommandBuffer_parameters vtFrameProcessor  commandBuffer parameters =
  sendMsg vtFrameProcessor (mkSelector "processWithCommandBuffer:parameters:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId parameters) :: Ptr ())]

-- | Performs all necessary tasks to end the session.
--
-- After this call completes, you can process no new frames unless you call ``startSessionWithConfiguration`` again.
--
-- ObjC selector: @- endSession@
endSession :: IsVTFrameProcessor vtFrameProcessor => vtFrameProcessor -> IO ()
endSession vtFrameProcessor  =
  sendMsg vtFrameProcessor (mkSelector "endSession") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @startSessionWithConfiguration:error:@
startSessionWithConfiguration_errorSelector :: Selector
startSessionWithConfiguration_errorSelector = mkSelector "startSessionWithConfiguration:error:"

-- | @Selector@ for @processWithParameters:error:@
processWithParameters_errorSelector :: Selector
processWithParameters_errorSelector = mkSelector "processWithParameters:error:"

-- | @Selector@ for @processWithCommandBuffer:parameters:@
processWithCommandBuffer_parametersSelector :: Selector
processWithCommandBuffer_parametersSelector = mkSelector "processWithCommandBuffer:parameters:"

-- | @Selector@ for @endSession@
endSessionSelector :: Selector
endSessionSelector = mkSelector "endSession"

