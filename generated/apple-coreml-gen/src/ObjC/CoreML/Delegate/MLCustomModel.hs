{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MLCustomModel@.
--
-- Usage:
--
-- @
-- delegate <- newMLCustomModel defaultMLCustomModelOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CoreML.Delegate.MLCustomModel
  ( MLCustomModelOverrides(..)
  , defaultMLCustomModelOverrides
  , newMLCustomModel
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

-- | Overrides record for @\@protocol MLCustomModel@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MLCustomModelOverrides = MLCustomModelOverrides
  { _initWithModelDescription_parameterDictionary_error :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _predictionFromFeatures_options_error :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _predictionsFromBatch_options_error :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMLCustomModelOverrides :: MLCustomModelOverrides
defaultMLCustomModelOverrides = MLCustomModelOverrides
  { _initWithModelDescription_parameterDictionary_error = Nothing
  , _predictionFromFeatures_options_error = Nothing
  , _predictionsFromBatch_options_error = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mlCustomModelDelegateClass #-}
mlCustomModelDelegateClass :: Class
mlCustomModelDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMLCustomModel" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_initWithModelDescription_parameterDictionary_error = unSelector (mkSelector "initWithModelDescription:parameterDictionary:error:")
      sel_predictionFromFeatures_options_error = unSelector (mkSelector "predictionFromFeatures:options:error:")
      sel_predictionsFromBatch_options_error = unSelector (mkSelector "predictionsFromBatch:options:error:")
  -- initWithModelDescription:parameterDictionary:error:
  stub_0 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MLCustomModelOverrides
    case _initWithModelDescription_parameterDictionary_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "initWithModelDescription:parameterDictionary:error:" "@@:@@@" stub_0

  -- predictionFromFeatures:options:error:
  stub_1 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MLCustomModelOverrides
    case _predictionFromFeatures_options_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "predictionFromFeatures:options:error:" "@@:@@@" stub_1

  -- predictionsFromBatch:options:error:
  stub_2 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MLCustomModelOverrides
    case _predictionsFromBatch_options_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "predictionsFromBatch:options:error:" "@@:@@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MLCustomModelOverrides
    if queriedSel == sel_initWithModelDescription_parameterDictionary_error then pure (maybe 0 (const 1) (_initWithModelDescription_parameterDictionary_error rec_))
    else if queriedSel == sel_predictionFromFeatures_options_error then pure (maybe 0 (const 1) (_predictionFromFeatures_options_error rec_))
    else if queriedSel == sel_predictionsFromBatch_options_error then pure (maybe 0 (const 1) (_predictionsFromBatch_options_error rec_))
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
newMLCustomModel :: MLCustomModelOverrides -> IO RawId
newMLCustomModel overrides = do
  inst <- class_createInstance mlCustomModelDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
