{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol PGDevice@.
--
-- Usage:
--
-- @
-- delegate <- newPGDevice defaultPGDeviceOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.ParavirtualizedGraphics.Delegate.PGDevice
  ( PGDeviceOverrides(..)
  , defaultPGDeviceOverrides
  , newPGDevice
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

-- | Overrides record for @\@protocol PGDevice@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data PGDeviceOverrides = PGDeviceOverrides
  { _mmioReadAtOffset :: !(Maybe (Int -> IO Int))
  , _mmioWriteAtOffset_value :: !(Maybe (Int -> Int -> IO ()))
  , _newDisplayWithDescriptor_port_serialNum :: !(Maybe (RawId -> Int -> Int -> IO RawId))
  , _willSuspend :: !(Maybe (IO ()))
  , _finishSuspend :: !(Maybe (IO RawId))
  , _didResume :: !(Maybe (IO ()))
  , _pause :: !(Maybe (IO ()))
  , _unpause :: !(Maybe (IO ()))
  , _stop :: !(Maybe (IO ()))
  , _reset :: !(Maybe (IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultPGDeviceOverrides :: PGDeviceOverrides
defaultPGDeviceOverrides = PGDeviceOverrides
  { _mmioReadAtOffset = Nothing
  , _mmioWriteAtOffset_value = Nothing
  , _newDisplayWithDescriptor_port_serialNum = Nothing
  , _willSuspend = Nothing
  , _finishSuspend = Nothing
  , _didResume = Nothing
  , _pause = Nothing
  , _unpause = Nothing
  , _stop = Nothing
  , _reset = Nothing
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
  wrap_at_Q_I_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CUInt -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CUInt -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_Q_I_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> CUInt -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> CUInt -> IO ()))

foreign import ccall "wrapper"
  wrap_Q_I
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO CUInt)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO CUInt))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE pgDeviceDelegateClass #-}
pgDeviceDelegateClass :: Class
pgDeviceDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsPGDevice" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_mmioReadAtOffset = unSelector (mkSelector "mmioReadAtOffset:")
      sel_mmioWriteAtOffset_value = unSelector (mkSelector "mmioWriteAtOffset:value:")
      sel_newDisplayWithDescriptor_port_serialNum = unSelector (mkSelector "newDisplayWithDescriptor:port:serialNum:")
      sel_willSuspend = unSelector (mkSelector "willSuspend")
      sel_finishSuspend = unSelector (mkSelector "finishSuspend")
      sel_didResume = unSelector (mkSelector "didResume")
      sel_pause = unSelector (mkSelector "pause")
      sel_unpause = unSelector (mkSelector "unpause")
      sel_stop = unSelector (mkSelector "stop")
      sel_reset = unSelector (mkSelector "reset")
  -- mmioReadAtOffset:
  stub_0 <- wrap_Q_I $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PGDeviceOverrides
    case _mmioReadAtOffset rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (fromIntegral arg0)
        pure (fromIntegral r)
  addObjCMethod cls "mmioReadAtOffset:" "I@:Q" stub_0

  -- mmioWriteAtOffset:value:
  stub_1 <- wrap_Q_I_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PGDeviceOverrides
    case _mmioWriteAtOffset_value rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0) (fromIntegral arg1)
  addObjCMethod cls "mmioWriteAtOffset:value:" "v@:QI" stub_1

  -- newDisplayWithDescriptor:port:serialNum:
  stub_2 <- wrap_at_Q_I_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PGDeviceOverrides
    case _newDisplayWithDescriptor_port_serialNum rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "newDisplayWithDescriptor:port:serialNum:" "@@:@QI" stub_2

  -- willSuspend
  stub_3 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PGDeviceOverrides
    case _willSuspend rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "willSuspend" "v@:" stub_3

  -- finishSuspend
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PGDeviceOverrides
    case _finishSuspend rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "finishSuspend" "@@:" stub_4

  -- didResume
  stub_5 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PGDeviceOverrides
    case _didResume rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "didResume" "v@:" stub_5

  -- pause
  stub_6 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PGDeviceOverrides
    case _pause rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "pause" "v@:" stub_6

  -- unpause
  stub_7 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PGDeviceOverrides
    case _unpause rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "unpause" "v@:" stub_7

  -- stop
  stub_8 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PGDeviceOverrides
    case _stop rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "stop" "v@:" stub_8

  -- reset
  stub_9 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PGDeviceOverrides
    case _reset rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "reset" "v@:" stub_9

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PGDeviceOverrides
    if queriedSel == sel_mmioReadAtOffset then pure (maybe 0 (const 1) (_mmioReadAtOffset rec_))
    else if queriedSel == sel_mmioWriteAtOffset_value then pure (maybe 0 (const 1) (_mmioWriteAtOffset_value rec_))
    else if queriedSel == sel_newDisplayWithDescriptor_port_serialNum then pure (maybe 0 (const 1) (_newDisplayWithDescriptor_port_serialNum rec_))
    else if queriedSel == sel_willSuspend then pure (maybe 0 (const 1) (_willSuspend rec_))
    else if queriedSel == sel_finishSuspend then pure (maybe 0 (const 1) (_finishSuspend rec_))
    else if queriedSel == sel_didResume then pure (maybe 0 (const 1) (_didResume rec_))
    else if queriedSel == sel_pause then pure (maybe 0 (const 1) (_pause rec_))
    else if queriedSel == sel_unpause then pure (maybe 0 (const 1) (_unpause rec_))
    else if queriedSel == sel_stop then pure (maybe 0 (const 1) (_stop rec_))
    else if queriedSel == sel_reset then pure (maybe 0 (const 1) (_reset rec_))
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
newPGDevice :: PGDeviceOverrides -> IO RawId
newPGDevice overrides = do
  inst <- class_createInstance pgDeviceDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
