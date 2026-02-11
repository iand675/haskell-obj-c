{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLSharedEvent@.
--
-- Usage:
--
-- @
-- delegate <- newMTLSharedEvent defaultMTLSharedEventOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLSharedEvent
  ( MTLSharedEventOverrides(..)
  , defaultMTLSharedEventOverrides
  , newMTLSharedEvent
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

-- | Overrides record for @\@protocol MTLSharedEvent@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLSharedEventOverrides = MTLSharedEventOverrides
  { _newSharedEventHandle :: !(Maybe (IO RawId))
  , _waitUntilSignaledValue_timeoutMS :: !(Maybe (Int -> Int -> IO Bool))
  , _signaledValue :: !(Maybe (IO Int))
  , _setSignaledValue :: !(Maybe (Int -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLSharedEventOverrides :: MTLSharedEventOverrides
defaultMTLSharedEventOverrides = MTLSharedEventOverrides
  { _newSharedEventHandle = Nothing
  , _waitUntilSignaledValue_timeoutMS = Nothing
  , _signaledValue = Nothing
  , _setSignaledValue = Nothing
  }

foreign import ccall "wrapper"
  wrap_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_Q_Q_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> CULong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> CULong -> IO CULong))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtlSharedEventDelegateClass #-}
mtlSharedEventDelegateClass :: Class
mtlSharedEventDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLSharedEvent" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_newSharedEventHandle = unSelector (mkSelector "newSharedEventHandle")
      sel_waitUntilSignaledValue_timeoutMS = unSelector (mkSelector "waitUntilSignaledValue:timeoutMS:")
      sel_signaledValue = unSelector (mkSelector "signaledValue")
      sel_setSignaledValue = unSelector (mkSelector "setSignaledValue:")
  -- newSharedEventHandle
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLSharedEventOverrides
    case _newSharedEventHandle rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "newSharedEventHandle" "@@:" stub_0

  -- waitUntilSignaledValue:timeoutMS:
  stub_1 <- wrap_Q_Q_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLSharedEventOverrides
    case _waitUntilSignaledValue_timeoutMS rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (fromIntegral arg0) (fromIntegral arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "waitUntilSignaledValue:timeoutMS:" "B@:QQ" stub_1

  -- signaledValue
  stub_2 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLSharedEventOverrides
    case _signaledValue rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "signaledValue" "Q@:" stub_2

  -- setSignaledValue:
  stub_3 <- wrap_Q_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLSharedEventOverrides
    case _setSignaledValue rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0)
  addObjCMethod cls "setSignaledValue:" "v@:Q" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLSharedEventOverrides
    if queriedSel == sel_newSharedEventHandle then pure (maybe 0 (const 1) (_newSharedEventHandle rec_))
    else if queriedSel == sel_waitUntilSignaledValue_timeoutMS then pure (maybe 0 (const 1) (_waitUntilSignaledValue_timeoutMS rec_))
    else if queriedSel == sel_signaledValue then pure (maybe 0 (const 1) (_signaledValue rec_))
    else if queriedSel == sel_setSignaledValue then pure (maybe 0 (const 1) (_setSignaledValue rec_))
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
newMTLSharedEvent :: MTLSharedEventOverrides -> IO RawId
newMTLSharedEvent overrides = do
  inst <- class_createInstance mtlSharedEventDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
