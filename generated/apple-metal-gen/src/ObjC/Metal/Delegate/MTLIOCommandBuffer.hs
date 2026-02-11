{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLIOCommandBuffer@.
--
-- Usage:
--
-- @
-- delegate <- newMTLIOCommandBuffer defaultMTLIOCommandBufferOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLIOCommandBuffer
  ( MTLIOCommandBufferOverrides(..)
  , defaultMTLIOCommandBufferOverrides
  , newMTLIOCommandBuffer
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

-- | Overrides record for @\@protocol MTLIOCommandBuffer@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLIOCommandBufferOverrides = MTLIOCommandBufferOverrides
  { _loadBuffer_offset_size_sourceHandle_sourceHandleOffset :: !(Maybe (RawId -> Int -> Int -> RawId -> Int -> IO ()))
  , _copyStatusToBuffer_offset :: !(Maybe (RawId -> Int -> IO ()))
  , _commit :: !(Maybe (IO ()))
  , _waitUntilCompleted :: !(Maybe (IO ()))
  , _tryCancel :: !(Maybe (IO ()))
  , _addBarrier :: !(Maybe (IO ()))
  , _pushDebugGroup :: !(Maybe (RawId -> IO ()))
  , _popDebugGroup :: !(Maybe (IO ()))
  , _enqueue :: !(Maybe (IO ()))
  , _waitForEvent_value :: !(Maybe (RawId -> Int -> IO ()))
  , _signalEvent_value :: !(Maybe (RawId -> Int -> IO ()))
  , _label :: !(Maybe (IO RawId))
  , _setLabel :: !(Maybe (RawId -> IO ()))
  , _error :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLIOCommandBufferOverrides :: MTLIOCommandBufferOverrides
defaultMTLIOCommandBufferOverrides = MTLIOCommandBufferOverrides
  { _loadBuffer_offset_size_sourceHandle_sourceHandleOffset = Nothing
  , _copyStatusToBuffer_offset = Nothing
  , _commit = Nothing
  , _waitUntilCompleted = Nothing
  , _tryCancel = Nothing
  , _addBarrier = Nothing
  , _pushDebugGroup = Nothing
  , _popDebugGroup = Nothing
  , _enqueue = Nothing
  , _waitForEvent_value = Nothing
  , _signalEvent_value = Nothing
  , _label = Nothing
  , _setLabel = Nothing
  , _error = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))

foreign import ccall "wrapper"
  wrap_at_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_Q_Q_at_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> Ptr ObjCObject -> CULong -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtlioCommandBufferDelegateClass #-}
mtlioCommandBufferDelegateClass :: Class
mtlioCommandBufferDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLIOCommandBuffer" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_loadBuffer_offset_size_sourceHandle_sourceHandleOffset = unSelector (mkSelector "loadBuffer:offset:size:sourceHandle:sourceHandleOffset:")
      sel_copyStatusToBuffer_offset = unSelector (mkSelector "copyStatusToBuffer:offset:")
      sel_commit = unSelector (mkSelector "commit")
      sel_waitUntilCompleted = unSelector (mkSelector "waitUntilCompleted")
      sel_tryCancel = unSelector (mkSelector "tryCancel")
      sel_addBarrier = unSelector (mkSelector "addBarrier")
      sel_pushDebugGroup = unSelector (mkSelector "pushDebugGroup:")
      sel_popDebugGroup = unSelector (mkSelector "popDebugGroup")
      sel_enqueue = unSelector (mkSelector "enqueue")
      sel_waitForEvent_value = unSelector (mkSelector "waitForEvent:value:")
      sel_signalEvent_value = unSelector (mkSelector "signalEvent:value:")
      sel_label = unSelector (mkSelector "label")
      sel_setLabel = unSelector (mkSelector "setLabel:")
      sel_error = unSelector (mkSelector "error")
  -- loadBuffer:offset:size:sourceHandle:sourceHandleOffset:
  stub_0 <- wrap_at_Q_Q_at_Q_v $ \self _cmd arg0 arg1 arg2 arg3 arg4 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOCommandBufferOverrides
    case _loadBuffer_offset_size_sourceHandle_sourceHandleOffset rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2) (RawId arg3) (fromIntegral arg4)
  addObjCMethod cls "loadBuffer:offset:size:sourceHandle:sourceHandleOffset:" "v@:@QQ@Q" stub_0

  -- copyStatusToBuffer:offset:
  stub_1 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOCommandBufferOverrides
    case _copyStatusToBuffer_offset rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "copyStatusToBuffer:offset:" "v@:@Q" stub_1

  -- commit
  stub_2 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOCommandBufferOverrides
    case _commit rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "commit" "v@:" stub_2

  -- waitUntilCompleted
  stub_3 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOCommandBufferOverrides
    case _waitUntilCompleted rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "waitUntilCompleted" "v@:" stub_3

  -- tryCancel
  stub_4 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOCommandBufferOverrides
    case _tryCancel rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "tryCancel" "v@:" stub_4

  -- addBarrier
  stub_5 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOCommandBufferOverrides
    case _addBarrier rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "addBarrier" "v@:" stub_5

  -- pushDebugGroup:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOCommandBufferOverrides
    case _pushDebugGroup rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "pushDebugGroup:" "v@:@" stub_6

  -- popDebugGroup
  stub_7 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOCommandBufferOverrides
    case _popDebugGroup rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "popDebugGroup" "v@:" stub_7

  -- enqueue
  stub_8 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOCommandBufferOverrides
    case _enqueue rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "enqueue" "v@:" stub_8

  -- waitForEvent:value:
  stub_9 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOCommandBufferOverrides
    case _waitForEvent_value rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "waitForEvent:value:" "v@:@Q" stub_9

  -- signalEvent:value:
  stub_10 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOCommandBufferOverrides
    case _signalEvent_value rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "signalEvent:value:" "v@:@Q" stub_10

  -- label
  stub_11 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOCommandBufferOverrides
    case _label rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "label" "@@:" stub_11

  -- setLabel:
  stub_12 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOCommandBufferOverrides
    case _setLabel rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setLabel:" "v@:@" stub_12

  -- error
  stub_13 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOCommandBufferOverrides
    case _error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "error" "@@:" stub_13

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOCommandBufferOverrides
    if queriedSel == sel_loadBuffer_offset_size_sourceHandle_sourceHandleOffset then pure (maybe 0 (const 1) (_loadBuffer_offset_size_sourceHandle_sourceHandleOffset rec_))
    else if queriedSel == sel_copyStatusToBuffer_offset then pure (maybe 0 (const 1) (_copyStatusToBuffer_offset rec_))
    else if queriedSel == sel_commit then pure (maybe 0 (const 1) (_commit rec_))
    else if queriedSel == sel_waitUntilCompleted then pure (maybe 0 (const 1) (_waitUntilCompleted rec_))
    else if queriedSel == sel_tryCancel then pure (maybe 0 (const 1) (_tryCancel rec_))
    else if queriedSel == sel_addBarrier then pure (maybe 0 (const 1) (_addBarrier rec_))
    else if queriedSel == sel_pushDebugGroup then pure (maybe 0 (const 1) (_pushDebugGroup rec_))
    else if queriedSel == sel_popDebugGroup then pure (maybe 0 (const 1) (_popDebugGroup rec_))
    else if queriedSel == sel_enqueue then pure (maybe 0 (const 1) (_enqueue rec_))
    else if queriedSel == sel_waitForEvent_value then pure (maybe 0 (const 1) (_waitForEvent_value rec_))
    else if queriedSel == sel_signalEvent_value then pure (maybe 0 (const 1) (_signalEvent_value rec_))
    else if queriedSel == sel_label then pure (maybe 0 (const 1) (_label rec_))
    else if queriedSel == sel_setLabel then pure (maybe 0 (const 1) (_setLabel rec_))
    else if queriedSel == sel_error then pure (maybe 0 (const 1) (_error rec_))
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
newMTLIOCommandBuffer :: MTLIOCommandBufferOverrides -> IO RawId
newMTLIOCommandBuffer overrides = do
  inst <- class_createInstance mtlioCommandBufferDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
