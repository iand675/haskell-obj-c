{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MLCustomLayer@.
--
-- Usage:
--
-- @
-- delegate <- newMLCustomLayer defaultMLCustomLayerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CoreML.Delegate.MLCustomLayer
  ( MLCustomLayerOverrides(..)
  , defaultMLCustomLayerOverrides
  , newMLCustomLayer
  ) where

import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.C.Types
import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (withCString)
import Foreign.LibFFI (retCULong, argPtr)

import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass, class_createInstance)
import ObjC.Runtime.ClassBuilder (objc_allocateClassPair, objc_registerClassPair)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.MsgSend (sendSuperMsg)
import ObjC.Runtime.StableIvar

-- | Overrides record for @\@protocol MLCustomLayer@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MLCustomLayerOverrides = MLCustomLayerOverrides
  { _initWithParameterDictionary_error :: !(Maybe (RawId -> RawId -> IO RawId))
  , _setWeightData_error :: !(Maybe (RawId -> RawId -> IO Bool))
  , _outputShapesForInputShapes_error :: !(Maybe (RawId -> RawId -> IO RawId))
  , _evaluateOnCPUWithInputs_outputs_error :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _encodeToCommandBuffer_inputs_outputs_error :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultMLCustomLayerOverrides :: MLCustomLayerOverrides
defaultMLCustomLayerOverrides = MLCustomLayerOverrides
  { _initWithParameterDictionary_error = Nothing
  , _setWeightData_error = Nothing
  , _outputShapesForInputShapes_error = Nothing
  , _evaluateOnCPUWithInputs_outputs_error = Nothing
  , _encodeToCommandBuffer_inputs_outputs_error = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mlCustomLayerDelegateClass #-}
mlCustomLayerDelegateClass :: Class
mlCustomLayerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMLCustomLayer" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_initWithParameterDictionary_error = unSelector (mkSelector "initWithParameterDictionary:error:")
      sel_setWeightData_error = unSelector (mkSelector "setWeightData:error:")
      sel_outputShapesForInputShapes_error = unSelector (mkSelector "outputShapesForInputShapes:error:")
      sel_evaluateOnCPUWithInputs_outputs_error = unSelector (mkSelector "evaluateOnCPUWithInputs:outputs:error:")
      sel_encodeToCommandBuffer_inputs_outputs_error = unSelector (mkSelector "encodeToCommandBuffer:inputs:outputs:error:")
  -- initWithParameterDictionary:error:
  stub_0 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MLCustomLayerOverrides
    case _initWithParameterDictionary_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "initWithParameterDictionary:error:" "@@:@@" stub_0

  -- setWeightData:error:
  stub_1 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MLCustomLayerOverrides
    case _setWeightData_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "setWeightData:error:" "B@:@@" stub_1

  -- outputShapesForInputShapes:error:
  stub_2 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MLCustomLayerOverrides
    case _outputShapesForInputShapes_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "outputShapesForInputShapes:error:" "@@:@@" stub_2

  -- evaluateOnCPUWithInputs:outputs:error:
  stub_3 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MLCustomLayerOverrides
    case _evaluateOnCPUWithInputs_outputs_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "evaluateOnCPUWithInputs:outputs:error:" "B@:@@@" stub_3

  -- encodeToCommandBuffer:inputs:outputs:error:
  stub_4 <- wrap_at_at_at_at_B $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MLCustomLayerOverrides
    case _encodeToCommandBuffer_inputs_outputs_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
        pure (if r then 1 else 0)
  addObjCMethod cls "encodeToCommandBuffer:inputs:outputs:error:" "B@:@@@@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MLCustomLayerOverrides
    if queriedSel == sel_initWithParameterDictionary_error then pure (maybe 0 (const 1) (_initWithParameterDictionary_error rec_))
    else if queriedSel == sel_setWeightData_error then pure (maybe 0 (const 1) (_setWeightData_error rec_))
    else if queriedSel == sel_outputShapesForInputShapes_error then pure (maybe 0 (const 1) (_outputShapesForInputShapes_error rec_))
    else if queriedSel == sel_evaluateOnCPUWithInputs_outputs_error then pure (maybe 0 (const 1) (_evaluateOnCPUWithInputs_outputs_error rec_))
    else if queriedSel == sel_encodeToCommandBuffer_inputs_outputs_error then pure (maybe 0 (const 1) (_encodeToCommandBuffer_inputs_outputs_error rec_))
    else do
      let super_ = ObjCSuper (RawId self) superCls
      sendSuperMsg super_ (mkSelector "respondsToSelector:") retCULong
        [argPtr (castPtr queriedSel :: Ptr ())]
  addObjCMethod cls "respondsToSelector:" "B@::" rtsStub

  addStablePtrDeallocHandler cls
  objc_registerClassPair cls
  pure cls

-- | Create a new delegate implementing this protocol.
--
-- The returned 'RawId' can be used as a delegate or data source.
newMLCustomLayer :: MLCustomLayerOverrides -> IO RawId
newMLCustomLayer overrides = do
  inst <- class_createInstance mlCustomLayerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
