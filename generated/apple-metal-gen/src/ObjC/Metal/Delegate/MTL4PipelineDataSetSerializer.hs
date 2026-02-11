{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTL4PipelineDataSetSerializer@.
--
-- Usage:
--
-- @
-- delegate <- newMTL4PipelineDataSetSerializer defaultMTL4PipelineDataSetSerializerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTL4PipelineDataSetSerializer
  ( MTL4PipelineDataSetSerializerOverrides(..)
  , defaultMTL4PipelineDataSetSerializerOverrides
  , newMTL4PipelineDataSetSerializer
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

-- | Overrides record for @\@protocol MTL4PipelineDataSetSerializer@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTL4PipelineDataSetSerializerOverrides = MTL4PipelineDataSetSerializerOverrides
  { _serializeAsArchiveAndFlushToURL_error :: !(Maybe (RawId -> RawId -> IO Bool))
  , _serializeAsPipelinesScriptWithError :: !(Maybe (RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMTL4PipelineDataSetSerializerOverrides :: MTL4PipelineDataSetSerializerOverrides
defaultMTL4PipelineDataSetSerializerOverrides = MTL4PipelineDataSetSerializerOverrides
  { _serializeAsArchiveAndFlushToURL_error = Nothing
  , _serializeAsPipelinesScriptWithError = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtL4PipelineDataSetSerializerDelegateClass #-}
mtL4PipelineDataSetSerializerDelegateClass :: Class
mtL4PipelineDataSetSerializerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTL4PipelineDataSetSerializer" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_serializeAsArchiveAndFlushToURL_error = unSelector (mkSelector "serializeAsArchiveAndFlushToURL:error:")
      sel_serializeAsPipelinesScriptWithError = unSelector (mkSelector "serializeAsPipelinesScriptWithError:")
  -- serializeAsArchiveAndFlushToURL:error:
  stub_0 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4PipelineDataSetSerializerOverrides
    case _serializeAsArchiveAndFlushToURL_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "serializeAsArchiveAndFlushToURL:error:" "B@:@@" stub_0

  -- serializeAsPipelinesScriptWithError:
  stub_1 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4PipelineDataSetSerializerOverrides
    case _serializeAsPipelinesScriptWithError rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "serializeAsPipelinesScriptWithError:" "@@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4PipelineDataSetSerializerOverrides
    if queriedSel == sel_serializeAsArchiveAndFlushToURL_error then pure (maybe 0 (const 1) (_serializeAsArchiveAndFlushToURL_error rec_))
    else if queriedSel == sel_serializeAsPipelinesScriptWithError then pure (maybe 0 (const 1) (_serializeAsPipelinesScriptWithError rec_))
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
newMTL4PipelineDataSetSerializer :: MTL4PipelineDataSetSerializerOverrides -> IO RawId
newMTL4PipelineDataSetSerializer overrides = do
  inst <- class_createInstance mtL4PipelineDataSetSerializerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
