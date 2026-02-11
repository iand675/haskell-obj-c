{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLIOCommandQueue@.
--
-- Usage:
--
-- @
-- delegate <- newMTLIOCommandQueue defaultMTLIOCommandQueueOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLIOCommandQueue
  ( MTLIOCommandQueueOverrides(..)
  , defaultMTLIOCommandQueueOverrides
  , newMTLIOCommandQueue
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

-- | Overrides record for @\@protocol MTLIOCommandQueue@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLIOCommandQueueOverrides = MTLIOCommandQueueOverrides
  { _enqueueBarrier :: !(Maybe (IO ()))
  , _commandBuffer :: !(Maybe (IO RawId))
  , _commandBufferWithUnretainedReferences :: !(Maybe (IO RawId))
  , _label :: !(Maybe (IO RawId))
  , _setLabel :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLIOCommandQueueOverrides :: MTLIOCommandQueueOverrides
defaultMTLIOCommandQueueOverrides = MTLIOCommandQueueOverrides
  { _enqueueBarrier = Nothing
  , _commandBuffer = Nothing
  , _commandBufferWithUnretainedReferences = Nothing
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
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtlioCommandQueueDelegateClass #-}
mtlioCommandQueueDelegateClass :: Class
mtlioCommandQueueDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLIOCommandQueue" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_enqueueBarrier = unSelector (mkSelector "enqueueBarrier")
      sel_commandBuffer = unSelector (mkSelector "commandBuffer")
      sel_commandBufferWithUnretainedReferences = unSelector (mkSelector "commandBufferWithUnretainedReferences")
      sel_label = unSelector (mkSelector "label")
      sel_setLabel = unSelector (mkSelector "setLabel:")
  -- enqueueBarrier
  stub_0 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOCommandQueueOverrides
    case _enqueueBarrier rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "enqueueBarrier" "v@:" stub_0

  -- commandBuffer
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOCommandQueueOverrides
    case _commandBuffer rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "commandBuffer" "@@:" stub_1

  -- commandBufferWithUnretainedReferences
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOCommandQueueOverrides
    case _commandBufferWithUnretainedReferences rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "commandBufferWithUnretainedReferences" "@@:" stub_2

  -- label
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOCommandQueueOverrides
    case _label rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "label" "@@:" stub_3

  -- setLabel:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOCommandQueueOverrides
    case _setLabel rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setLabel:" "v@:@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOCommandQueueOverrides
    if queriedSel == sel_enqueueBarrier then pure (maybe 0 (const 1) (_enqueueBarrier rec_))
    else if queriedSel == sel_commandBuffer then pure (maybe 0 (const 1) (_commandBuffer rec_))
    else if queriedSel == sel_commandBufferWithUnretainedReferences then pure (maybe 0 (const 1) (_commandBufferWithUnretainedReferences rec_))
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
newMTLIOCommandQueue :: MTLIOCommandQueueOverrides -> IO RawId
newMTLIOCommandQueue overrides = do
  inst <- class_createInstance mtlioCommandQueueDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
