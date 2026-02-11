{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLBinding@.
--
-- Usage:
--
-- @
-- delegate <- newMTLBinding defaultMTLBindingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLBinding
  ( MTLBindingOverrides(..)
  , defaultMTLBindingOverrides
  , newMTLBinding
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

-- | Overrides record for @\@protocol MTLBinding@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLBindingOverrides = MTLBindingOverrides
  { _name :: !(Maybe (IO RawId))
  , _index :: !(Maybe (IO Int))
  , _used :: !(Maybe (IO Bool))
  , _argument :: !(Maybe (IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLBindingOverrides :: MTLBindingOverrides
defaultMTLBindingOverrides = MTLBindingOverrides
  { _name = Nothing
  , _index = Nothing
  , _used = Nothing
  , _argument = Nothing
  }

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtlBindingDelegateClass #-}
mtlBindingDelegateClass :: Class
mtlBindingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLBinding" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_name = unSelector (mkSelector "name")
      sel_index = unSelector (mkSelector "index")
      sel_used = unSelector (mkSelector "used")
      sel_argument = unSelector (mkSelector "argument")
  -- name
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLBindingOverrides
    case _name rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "name" "@@:" stub_0

  -- index
  stub_1 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLBindingOverrides
    case _index rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "index" "Q@:" stub_1

  -- used
  stub_2 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLBindingOverrides
    case _used rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "used" "B@:" stub_2

  -- argument
  stub_3 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLBindingOverrides
    case _argument rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "argument" "B@:" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLBindingOverrides
    if queriedSel == sel_name then pure (maybe 0 (const 1) (_name rec_))
    else if queriedSel == sel_index then pure (maybe 0 (const 1) (_index rec_))
    else if queriedSel == sel_used then pure (maybe 0 (const 1) (_used rec_))
    else if queriedSel == sel_argument then pure (maybe 0 (const 1) (_argument rec_))
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
newMTLBinding :: MTLBindingOverrides -> IO RawId
newMTLBinding overrides = do
  inst <- class_createInstance mtlBindingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
