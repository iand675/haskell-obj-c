{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTL4CommandAllocator@.
--
-- Usage:
--
-- @
-- delegate <- newMTL4CommandAllocator defaultMTL4CommandAllocatorOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTL4CommandAllocator
  ( MTL4CommandAllocatorOverrides(..)
  , defaultMTL4CommandAllocatorOverrides
  , newMTL4CommandAllocator
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

-- | Overrides record for @\@protocol MTL4CommandAllocator@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTL4CommandAllocatorOverrides = MTL4CommandAllocatorOverrides
  { _allocatedSize :: !(Maybe (IO Int))
  , _reset :: !(Maybe (IO ()))
  , _device :: !(Maybe (IO RawId))
  , _label :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMTL4CommandAllocatorOverrides :: MTL4CommandAllocatorOverrides
defaultMTL4CommandAllocatorOverrides = MTL4CommandAllocatorOverrides
  { _allocatedSize = Nothing
  , _reset = Nothing
  , _device = Nothing
  , _label = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtL4CommandAllocatorDelegateClass #-}
mtL4CommandAllocatorDelegateClass :: Class
mtL4CommandAllocatorDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTL4CommandAllocator" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_allocatedSize = unSelector (mkSelector "allocatedSize")
      sel_reset = unSelector (mkSelector "reset")
      sel_device = unSelector (mkSelector "device")
      sel_label = unSelector (mkSelector "label")
  -- allocatedSize
  stub_0 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommandAllocatorOverrides
    case _allocatedSize rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "allocatedSize" "Q@:" stub_0

  -- reset
  stub_1 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommandAllocatorOverrides
    case _reset rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "reset" "v@:" stub_1

  -- device
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommandAllocatorOverrides
    case _device rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "device" "@@:" stub_2

  -- label
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommandAllocatorOverrides
    case _label rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "label" "@@:" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommandAllocatorOverrides
    if queriedSel == sel_allocatedSize then pure (maybe 0 (const 1) (_allocatedSize rec_))
    else if queriedSel == sel_reset then pure (maybe 0 (const 1) (_reset rec_))
    else if queriedSel == sel_device then pure (maybe 0 (const 1) (_device rec_))
    else if queriedSel == sel_label then pure (maybe 0 (const 1) (_label rec_))
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
newMTL4CommandAllocator :: MTL4CommandAllocatorOverrides -> IO RawId
newMTL4CommandAllocator overrides = do
  inst <- class_createInstance mtL4CommandAllocatorDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
