{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLBinaryArchive@.
--
-- Usage:
--
-- @
-- delegate <- newMTLBinaryArchive defaultMTLBinaryArchiveOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLBinaryArchive
  ( MTLBinaryArchiveOverrides(..)
  , defaultMTLBinaryArchiveOverrides
  , newMTLBinaryArchive
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

-- | Overrides record for @\@protocol MTLBinaryArchive@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLBinaryArchiveOverrides = MTLBinaryArchiveOverrides
  { _addComputePipelineFunctionsWithDescriptor_error :: !(Maybe (RawId -> RawId -> IO Bool))
  , _addRenderPipelineFunctionsWithDescriptor_error :: !(Maybe (RawId -> RawId -> IO Bool))
  , _addTileRenderPipelineFunctionsWithDescriptor_error :: !(Maybe (RawId -> RawId -> IO Bool))
  , _addMeshRenderPipelineFunctionsWithDescriptor_error :: !(Maybe (RawId -> RawId -> IO Bool))
  , _addLibraryWithDescriptor_error :: !(Maybe (RawId -> RawId -> IO Bool))
  , _serializeToURL_error :: !(Maybe (RawId -> RawId -> IO Bool))
  , _addFunctionWithDescriptor_library_error :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _label :: !(Maybe (IO RawId))
  , _setLabel :: !(Maybe (RawId -> IO ()))
  , _device :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLBinaryArchiveOverrides :: MTLBinaryArchiveOverrides
defaultMTLBinaryArchiveOverrides = MTLBinaryArchiveOverrides
  { _addComputePipelineFunctionsWithDescriptor_error = Nothing
  , _addRenderPipelineFunctionsWithDescriptor_error = Nothing
  , _addTileRenderPipelineFunctionsWithDescriptor_error = Nothing
  , _addMeshRenderPipelineFunctionsWithDescriptor_error = Nothing
  , _addLibraryWithDescriptor_error = Nothing
  , _serializeToURL_error = Nothing
  , _addFunctionWithDescriptor_library_error = Nothing
  , _label = Nothing
  , _setLabel = Nothing
  , _device = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtlBinaryArchiveDelegateClass #-}
mtlBinaryArchiveDelegateClass :: Class
mtlBinaryArchiveDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLBinaryArchive" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_addComputePipelineFunctionsWithDescriptor_error = unSelector (mkSelector "addComputePipelineFunctionsWithDescriptor:error:")
      sel_addRenderPipelineFunctionsWithDescriptor_error = unSelector (mkSelector "addRenderPipelineFunctionsWithDescriptor:error:")
      sel_addTileRenderPipelineFunctionsWithDescriptor_error = unSelector (mkSelector "addTileRenderPipelineFunctionsWithDescriptor:error:")
      sel_addMeshRenderPipelineFunctionsWithDescriptor_error = unSelector (mkSelector "addMeshRenderPipelineFunctionsWithDescriptor:error:")
      sel_addLibraryWithDescriptor_error = unSelector (mkSelector "addLibraryWithDescriptor:error:")
      sel_serializeToURL_error = unSelector (mkSelector "serializeToURL:error:")
      sel_addFunctionWithDescriptor_library_error = unSelector (mkSelector "addFunctionWithDescriptor:library:error:")
      sel_label = unSelector (mkSelector "label")
      sel_setLabel = unSelector (mkSelector "setLabel:")
      sel_device = unSelector (mkSelector "device")
  -- addComputePipelineFunctionsWithDescriptor:error:
  stub_0 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLBinaryArchiveOverrides
    case _addComputePipelineFunctionsWithDescriptor_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "addComputePipelineFunctionsWithDescriptor:error:" "B@:@@" stub_0

  -- addRenderPipelineFunctionsWithDescriptor:error:
  stub_1 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLBinaryArchiveOverrides
    case _addRenderPipelineFunctionsWithDescriptor_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "addRenderPipelineFunctionsWithDescriptor:error:" "B@:@@" stub_1

  -- addTileRenderPipelineFunctionsWithDescriptor:error:
  stub_2 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLBinaryArchiveOverrides
    case _addTileRenderPipelineFunctionsWithDescriptor_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "addTileRenderPipelineFunctionsWithDescriptor:error:" "B@:@@" stub_2

  -- addMeshRenderPipelineFunctionsWithDescriptor:error:
  stub_3 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLBinaryArchiveOverrides
    case _addMeshRenderPipelineFunctionsWithDescriptor_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "addMeshRenderPipelineFunctionsWithDescriptor:error:" "B@:@@" stub_3

  -- addLibraryWithDescriptor:error:
  stub_4 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLBinaryArchiveOverrides
    case _addLibraryWithDescriptor_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "addLibraryWithDescriptor:error:" "B@:@@" stub_4

  -- serializeToURL:error:
  stub_5 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLBinaryArchiveOverrides
    case _serializeToURL_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "serializeToURL:error:" "B@:@@" stub_5

  -- addFunctionWithDescriptor:library:error:
  stub_6 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLBinaryArchiveOverrides
    case _addFunctionWithDescriptor_library_error rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "addFunctionWithDescriptor:library:error:" "B@:@@@" stub_6

  -- label
  stub_7 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLBinaryArchiveOverrides
    case _label rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "label" "@@:" stub_7

  -- setLabel:
  stub_8 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLBinaryArchiveOverrides
    case _setLabel rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setLabel:" "v@:@" stub_8

  -- device
  stub_9 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLBinaryArchiveOverrides
    case _device rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "device" "@@:" stub_9

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLBinaryArchiveOverrides
    if queriedSel == sel_addComputePipelineFunctionsWithDescriptor_error then pure (maybe 0 (const 1) (_addComputePipelineFunctionsWithDescriptor_error rec_))
    else if queriedSel == sel_addRenderPipelineFunctionsWithDescriptor_error then pure (maybe 0 (const 1) (_addRenderPipelineFunctionsWithDescriptor_error rec_))
    else if queriedSel == sel_addTileRenderPipelineFunctionsWithDescriptor_error then pure (maybe 0 (const 1) (_addTileRenderPipelineFunctionsWithDescriptor_error rec_))
    else if queriedSel == sel_addMeshRenderPipelineFunctionsWithDescriptor_error then pure (maybe 0 (const 1) (_addMeshRenderPipelineFunctionsWithDescriptor_error rec_))
    else if queriedSel == sel_addLibraryWithDescriptor_error then pure (maybe 0 (const 1) (_addLibraryWithDescriptor_error rec_))
    else if queriedSel == sel_serializeToURL_error then pure (maybe 0 (const 1) (_serializeToURL_error rec_))
    else if queriedSel == sel_addFunctionWithDescriptor_library_error then pure (maybe 0 (const 1) (_addFunctionWithDescriptor_library_error rec_))
    else if queriedSel == sel_label then pure (maybe 0 (const 1) (_label rec_))
    else if queriedSel == sel_setLabel then pure (maybe 0 (const 1) (_setLabel rec_))
    else if queriedSel == sel_device then pure (maybe 0 (const 1) (_device rec_))
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
newMTLBinaryArchive :: MTLBinaryArchiveOverrides -> IO RawId
newMTLBinaryArchive overrides = do
  inst <- class_createInstance mtlBinaryArchiveDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
