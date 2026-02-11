{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTL4Archive@.
--
-- Usage:
--
-- @
-- delegate <- newMTL4Archive defaultMTL4ArchiveOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTL4Archive
  ( MTL4ArchiveOverrides(..)
  , defaultMTL4ArchiveOverrides
  , newMTL4Archive
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

-- | Overrides record for @\@protocol MTL4Archive@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTL4ArchiveOverrides = MTL4ArchiveOverrides
  { _newComputePipelineStateWithDescriptor_error :: !(Maybe (RawId -> RawId -> IO RawId))
  , _newComputePipelineStateWithDescriptor_dynamicLinkingDescriptor_error :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _newRenderPipelineStateWithDescriptor_error :: !(Maybe (RawId -> RawId -> IO RawId))
  , _newRenderPipelineStateWithDescriptor_dynamicLinkingDescriptor_error :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _newBinaryFunctionWithDescriptor_error :: !(Maybe (RawId -> RawId -> IO RawId))
  , _label :: !(Maybe (IO RawId))
  , _setLabel :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMTL4ArchiveOverrides :: MTL4ArchiveOverrides
defaultMTL4ArchiveOverrides = MTL4ArchiveOverrides
  { _newComputePipelineStateWithDescriptor_error = Nothing
  , _newComputePipelineStateWithDescriptor_dynamicLinkingDescriptor_error = Nothing
  , _newRenderPipelineStateWithDescriptor_error = Nothing
  , _newRenderPipelineStateWithDescriptor_dynamicLinkingDescriptor_error = Nothing
  , _newBinaryFunctionWithDescriptor_error = Nothing
  , _label = Nothing
  , _setLabel = Nothing
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
  wrap_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtL4ArchiveDelegateClass #-}
mtL4ArchiveDelegateClass :: Class
mtL4ArchiveDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTL4Archive" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_newComputePipelineStateWithDescriptor_error = unSelector (mkSelector "newComputePipelineStateWithDescriptor:error:")
      sel_newComputePipelineStateWithDescriptor_dynamicLinkingDescriptor_error = unSelector (mkSelector "newComputePipelineStateWithDescriptor:dynamicLinkingDescriptor:error:")
      sel_newRenderPipelineStateWithDescriptor_error = unSelector (mkSelector "newRenderPipelineStateWithDescriptor:error:")
      sel_newRenderPipelineStateWithDescriptor_dynamicLinkingDescriptor_error = unSelector (mkSelector "newRenderPipelineStateWithDescriptor:dynamicLinkingDescriptor:error:")
      sel_newBinaryFunctionWithDescriptor_error = unSelector (mkSelector "newBinaryFunctionWithDescriptor:error:")
      sel_label = unSelector (mkSelector "label")
      sel_setLabel = unSelector (mkSelector "setLabel:")
  -- newComputePipelineStateWithDescriptor:error:
  stub_0 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ArchiveOverrides
    case _newComputePipelineStateWithDescriptor_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "newComputePipelineStateWithDescriptor:error:" "@@:@@" stub_0

  -- newComputePipelineStateWithDescriptor:dynamicLinkingDescriptor:error:
  stub_1 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ArchiveOverrides
    case _newComputePipelineStateWithDescriptor_dynamicLinkingDescriptor_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "newComputePipelineStateWithDescriptor:dynamicLinkingDescriptor:error:" "@@:@@@" stub_1

  -- newRenderPipelineStateWithDescriptor:error:
  stub_2 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ArchiveOverrides
    case _newRenderPipelineStateWithDescriptor_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "newRenderPipelineStateWithDescriptor:error:" "@@:@@" stub_2

  -- newRenderPipelineStateWithDescriptor:dynamicLinkingDescriptor:error:
  stub_3 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ArchiveOverrides
    case _newRenderPipelineStateWithDescriptor_dynamicLinkingDescriptor_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "newRenderPipelineStateWithDescriptor:dynamicLinkingDescriptor:error:" "@@:@@@" stub_3

  -- newBinaryFunctionWithDescriptor:error:
  stub_4 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ArchiveOverrides
    case _newBinaryFunctionWithDescriptor_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "newBinaryFunctionWithDescriptor:error:" "@@:@@" stub_4

  -- label
  stub_5 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ArchiveOverrides
    case _label rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "label" "@@:" stub_5

  -- setLabel:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ArchiveOverrides
    case _setLabel rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setLabel:" "v@:@" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4ArchiveOverrides
    if queriedSel == sel_newComputePipelineStateWithDescriptor_error then pure (maybe 0 (const 1) (_newComputePipelineStateWithDescriptor_error rec_))
    else if queriedSel == sel_newComputePipelineStateWithDescriptor_dynamicLinkingDescriptor_error then pure (maybe 0 (const 1) (_newComputePipelineStateWithDescriptor_dynamicLinkingDescriptor_error rec_))
    else if queriedSel == sel_newRenderPipelineStateWithDescriptor_error then pure (maybe 0 (const 1) (_newRenderPipelineStateWithDescriptor_error rec_))
    else if queriedSel == sel_newRenderPipelineStateWithDescriptor_dynamicLinkingDescriptor_error then pure (maybe 0 (const 1) (_newRenderPipelineStateWithDescriptor_dynamicLinkingDescriptor_error rec_))
    else if queriedSel == sel_newBinaryFunctionWithDescriptor_error then pure (maybe 0 (const 1) (_newBinaryFunctionWithDescriptor_error rec_))
    else if queriedSel == sel_label then pure (maybe 0 (const 1) (_label rec_))
    else if queriedSel == sel_setLabel then pure (maybe 0 (const 1) (_setLabel rec_))
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
newMTL4Archive :: MTL4ArchiveOverrides -> IO RawId
newMTL4Archive overrides = do
  inst <- class_createInstance mtL4ArchiveDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
