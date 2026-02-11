{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MDLMeshBuffer@.
--
-- Usage:
--
-- @
-- delegate <- newMDLMeshBuffer defaultMDLMeshBufferOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.ModelIO.Delegate.MDLMeshBuffer
  ( MDLMeshBufferOverrides(..)
  , defaultMDLMeshBufferOverrides
  , newMDLMeshBuffer
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

-- | Overrides record for @\@protocol MDLMeshBuffer@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MDLMeshBufferOverrides = MDLMeshBufferOverrides
  { _fillData_offset :: !(Maybe (RawId -> Int -> IO ()))
  , _map :: !(Maybe (IO RawId))
  , _length :: !(Maybe (IO Int))
  , _allocator :: !(Maybe (IO RawId))
  , _zone :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMDLMeshBufferOverrides :: MDLMeshBufferOverrides
defaultMDLMeshBufferOverrides = MDLMeshBufferOverrides
  { _fillData_offset = Nothing
  , _map = Nothing
  , _length = Nothing
  , _allocator = Nothing
  , _zone = Nothing
  }

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mdlMeshBufferDelegateClass #-}
mdlMeshBufferDelegateClass :: Class
mdlMeshBufferDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMDLMeshBuffer" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_fillData_offset = unSelector (mkSelector "fillData:offset:")
      sel_map = unSelector (mkSelector "map")
      sel_length = unSelector (mkSelector "length")
      sel_allocator = unSelector (mkSelector "allocator")
      sel_zone = unSelector (mkSelector "zone")
  -- fillData:offset:
  stub_0 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MDLMeshBufferOverrides
    case _fillData_offset rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "fillData:offset:" "v@:@Q" stub_0

  -- map
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MDLMeshBufferOverrides
    case _map rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "map" "@@:" stub_1

  -- length
  stub_2 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MDLMeshBufferOverrides
    case _length rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "length" "Q@:" stub_2

  -- allocator
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MDLMeshBufferOverrides
    case _allocator rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "allocator" "@@:" stub_3

  -- zone
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MDLMeshBufferOverrides
    case _zone rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "zone" "@@:" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MDLMeshBufferOverrides
    if queriedSel == sel_fillData_offset then pure (maybe 0 (const 1) (_fillData_offset rec_))
    else if queriedSel == sel_map then pure (maybe 0 (const 1) (_map rec_))
    else if queriedSel == sel_length then pure (maybe 0 (const 1) (_length rec_))
    else if queriedSel == sel_allocator then pure (maybe 0 (const 1) (_allocator rec_))
    else if queriedSel == sel_zone then pure (maybe 0 (const 1) (_zone rec_))
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
newMDLMeshBuffer :: MDLMeshBufferOverrides -> IO RawId
newMDLMeshBuffer overrides = do
  inst <- class_createInstance mdlMeshBufferDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
