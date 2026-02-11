{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLThreadgroupBinding@.
--
-- Usage:
--
-- @
-- delegate <- newMTLThreadgroupBinding defaultMTLThreadgroupBindingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLThreadgroupBinding
  ( MTLThreadgroupBindingOverrides(..)
  , defaultMTLThreadgroupBindingOverrides
  , newMTLThreadgroupBinding
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

-- | Overrides record for @\@protocol MTLThreadgroupBinding@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLThreadgroupBindingOverrides = MTLThreadgroupBindingOverrides
  { _threadgroupMemoryAlignment :: !(Maybe (IO Int))
  , _threadgroupMemoryDataSize :: !(Maybe (IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLThreadgroupBindingOverrides :: MTLThreadgroupBindingOverrides
defaultMTLThreadgroupBindingOverrides = MTLThreadgroupBindingOverrides
  { _threadgroupMemoryAlignment = Nothing
  , _threadgroupMemoryDataSize = Nothing
  }

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtlThreadgroupBindingDelegateClass #-}
mtlThreadgroupBindingDelegateClass :: Class
mtlThreadgroupBindingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLThreadgroupBinding" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_threadgroupMemoryAlignment = unSelector (mkSelector "threadgroupMemoryAlignment")
      sel_threadgroupMemoryDataSize = unSelector (mkSelector "threadgroupMemoryDataSize")
  -- threadgroupMemoryAlignment
  stub_0 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLThreadgroupBindingOverrides
    case _threadgroupMemoryAlignment rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "threadgroupMemoryAlignment" "Q@:" stub_0

  -- threadgroupMemoryDataSize
  stub_1 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLThreadgroupBindingOverrides
    case _threadgroupMemoryDataSize rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "threadgroupMemoryDataSize" "Q@:" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLThreadgroupBindingOverrides
    if queriedSel == sel_threadgroupMemoryAlignment then pure (maybe 0 (const 1) (_threadgroupMemoryAlignment rec_))
    else if queriedSel == sel_threadgroupMemoryDataSize then pure (maybe 0 (const 1) (_threadgroupMemoryDataSize rec_))
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
newMTLThreadgroupBinding :: MTLThreadgroupBindingOverrides -> IO RawId
newMTLThreadgroupBinding overrides = do
  inst <- class_createInstance mtlThreadgroupBindingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
