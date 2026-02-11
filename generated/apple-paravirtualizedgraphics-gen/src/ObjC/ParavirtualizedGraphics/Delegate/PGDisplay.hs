{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol PGDisplay@.
--
-- Usage:
--
-- @
-- delegate <- newPGDisplay defaultPGDisplayOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.ParavirtualizedGraphics.Delegate.PGDisplay
  ( PGDisplayOverrides(..)
  , defaultPGDisplayOverrides
  , newPGDisplay
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

-- | Overrides record for @\@protocol PGDisplay@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data PGDisplayOverrides = PGDisplayOverrides
  { _name :: !(Maybe (IO RawId))
  , _queue :: !(Maybe (IO RawId))
  , _serialNum :: !(Maybe (IO Int))
  , _port :: !(Maybe (IO Int))
  , _guestPresentCount :: !(Maybe (IO Int))
  , _hostPresentCount :: !(Maybe (IO Int))
  , _modeList :: !(Maybe (IO RawId))
  , _setModeList :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultPGDisplayOverrides :: PGDisplayOverrides
defaultPGDisplayOverrides = PGDisplayOverrides
  { _name = Nothing
  , _queue = Nothing
  , _serialNum = Nothing
  , _port = Nothing
  , _guestPresentCount = Nothing
  , _hostPresentCount = Nothing
  , _modeList = Nothing
  , _setModeList = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_I
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CUInt)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CUInt))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE pgDisplayDelegateClass #-}
pgDisplayDelegateClass :: Class
pgDisplayDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsPGDisplay" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_name = unSelector (mkSelector "name")
      sel_queue = unSelector (mkSelector "queue")
      sel_serialNum = unSelector (mkSelector "serialNum")
      sel_port = unSelector (mkSelector "port")
      sel_guestPresentCount = unSelector (mkSelector "guestPresentCount")
      sel_hostPresentCount = unSelector (mkSelector "hostPresentCount")
      sel_modeList = unSelector (mkSelector "modeList")
      sel_setModeList = unSelector (mkSelector "setModeList:")
  -- name
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PGDisplayOverrides
    case _name rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "name" "@@:" stub_0

  -- queue
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PGDisplayOverrides
    case _queue rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "queue" "@@:" stub_1

  -- serialNum
  stub_2 <- wrap_I $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PGDisplayOverrides
    case _serialNum rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "serialNum" "I@:" stub_2

  -- port
  stub_3 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PGDisplayOverrides
    case _port rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "port" "Q@:" stub_3

  -- guestPresentCount
  stub_4 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PGDisplayOverrides
    case _guestPresentCount rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "guestPresentCount" "Q@:" stub_4

  -- hostPresentCount
  stub_5 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PGDisplayOverrides
    case _hostPresentCount rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "hostPresentCount" "Q@:" stub_5

  -- modeList
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PGDisplayOverrides
    case _modeList rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "modeList" "@@:" stub_6

  -- setModeList:
  stub_7 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PGDisplayOverrides
    case _setModeList rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setModeList:" "v@:@" stub_7

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PGDisplayOverrides
    if queriedSel == sel_name then pure (maybe 0 (const 1) (_name rec_))
    else if queriedSel == sel_queue then pure (maybe 0 (const 1) (_queue rec_))
    else if queriedSel == sel_serialNum then pure (maybe 0 (const 1) (_serialNum rec_))
    else if queriedSel == sel_port then pure (maybe 0 (const 1) (_port rec_))
    else if queriedSel == sel_guestPresentCount then pure (maybe 0 (const 1) (_guestPresentCount rec_))
    else if queriedSel == sel_hostPresentCount then pure (maybe 0 (const 1) (_hostPresentCount rec_))
    else if queriedSel == sel_modeList then pure (maybe 0 (const 1) (_modeList rec_))
    else if queriedSel == sel_setModeList then pure (maybe 0 (const 1) (_setModeList rec_))
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
newPGDisplay :: PGDisplayOverrides -> IO RawId
newPGDisplay overrides = do
  inst <- class_createInstance pgDisplayDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
