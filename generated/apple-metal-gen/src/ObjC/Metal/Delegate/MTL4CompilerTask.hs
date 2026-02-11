{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTL4CompilerTask@.
--
-- Usage:
--
-- @
-- delegate <- newMTL4CompilerTask defaultMTL4CompilerTaskOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTL4CompilerTask
  ( MTL4CompilerTaskOverrides(..)
  , defaultMTL4CompilerTaskOverrides
  , newMTL4CompilerTask
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

-- | Overrides record for @\@protocol MTL4CompilerTask@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTL4CompilerTaskOverrides = MTL4CompilerTaskOverrides
  { _waitUntilCompleted :: !(Maybe (IO ()))
  , _compiler :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMTL4CompilerTaskOverrides :: MTL4CompilerTaskOverrides
defaultMTL4CompilerTaskOverrides = MTL4CompilerTaskOverrides
  { _waitUntilCompleted = Nothing
  , _compiler = Nothing
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
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtL4CompilerTaskDelegateClass #-}
mtL4CompilerTaskDelegateClass :: Class
mtL4CompilerTaskDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTL4CompilerTask" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_waitUntilCompleted = unSelector (mkSelector "waitUntilCompleted")
      sel_compiler = unSelector (mkSelector "compiler")
  -- waitUntilCompleted
  stub_0 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CompilerTaskOverrides
    case _waitUntilCompleted rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "waitUntilCompleted" "v@:" stub_0

  -- compiler
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CompilerTaskOverrides
    case _compiler rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "compiler" "@@:" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CompilerTaskOverrides
    if queriedSel == sel_waitUntilCompleted then pure (maybe 0 (const 1) (_waitUntilCompleted rec_))
    else if queriedSel == sel_compiler then pure (maybe 0 (const 1) (_compiler rec_))
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
newMTL4CompilerTask :: MTL4CompilerTaskOverrides -> IO RawId
newMTL4CompilerTask overrides = do
  inst <- class_createInstance mtL4CompilerTaskDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
