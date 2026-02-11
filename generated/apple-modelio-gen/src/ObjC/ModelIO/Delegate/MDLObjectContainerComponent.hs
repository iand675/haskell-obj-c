{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MDLObjectContainerComponent@.
--
-- Usage:
--
-- @
-- delegate <- newMDLObjectContainerComponent defaultMDLObjectContainerComponentOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.ModelIO.Delegate.MDLObjectContainerComponent
  ( MDLObjectContainerComponentOverrides(..)
  , defaultMDLObjectContainerComponentOverrides
  , newMDLObjectContainerComponent
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

-- | Overrides record for @\@protocol MDLObjectContainerComponent@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MDLObjectContainerComponentOverrides = MDLObjectContainerComponentOverrides
  { _addObject :: !(Maybe (RawId -> IO ()))
  , _removeObject :: !(Maybe (RawId -> IO ()))
  , _objectAtIndexedSubscript :: !(Maybe (Int -> IO RawId))
  , _count :: !(Maybe (IO Int))
  , _objects :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMDLObjectContainerComponentOverrides :: MDLObjectContainerComponentOverrides
defaultMDLObjectContainerComponentOverrides = MDLObjectContainerComponentOverrides
  { _addObject = Nothing
  , _removeObject = Nothing
  , _objectAtIndexedSubscript = Nothing
  , _count = Nothing
  , _objects = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_Q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mdlObjectContainerComponentDelegateClass #-}
mdlObjectContainerComponentDelegateClass :: Class
mdlObjectContainerComponentDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMDLObjectContainerComponent" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_addObject = unSelector (mkSelector "addObject:")
      sel_removeObject = unSelector (mkSelector "removeObject:")
      sel_objectAtIndexedSubscript = unSelector (mkSelector "objectAtIndexedSubscript:")
      sel_count = unSelector (mkSelector "count")
      sel_objects = unSelector (mkSelector "objects")
  -- addObject:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MDLObjectContainerComponentOverrides
    case _addObject rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "addObject:" "v@:@" stub_0

  -- removeObject:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MDLObjectContainerComponentOverrides
    case _removeObject rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "removeObject:" "v@:@" stub_1

  -- objectAtIndexedSubscript:
  stub_2 <- wrap_Q_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MDLObjectContainerComponentOverrides
    case _objectAtIndexedSubscript rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (fromIntegral arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "objectAtIndexedSubscript:" "@@:Q" stub_2

  -- count
  stub_3 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MDLObjectContainerComponentOverrides
    case _count rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "count" "Q@:" stub_3

  -- objects
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MDLObjectContainerComponentOverrides
    case _objects rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "objects" "@@:" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MDLObjectContainerComponentOverrides
    if queriedSel == sel_addObject then pure (maybe 0 (const 1) (_addObject rec_))
    else if queriedSel == sel_removeObject then pure (maybe 0 (const 1) (_removeObject rec_))
    else if queriedSel == sel_objectAtIndexedSubscript then pure (maybe 0 (const 1) (_objectAtIndexedSubscript rec_))
    else if queriedSel == sel_count then pure (maybe 0 (const 1) (_count rec_))
    else if queriedSel == sel_objects then pure (maybe 0 (const 1) (_objects rec_))
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
newMDLObjectContainerComponent :: MDLObjectContainerComponentOverrides -> IO RawId
newMDLObjectContainerComponent overrides = do
  inst <- class_createInstance mdlObjectContainerComponentDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
