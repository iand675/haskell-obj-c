{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol OSLogEntryFromProcess@.
--
-- Usage:
--
-- @
-- delegate <- newOSLogEntryFromProcess defaultOSLogEntryFromProcessOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.OSLog.Delegate.OSLogEntryFromProcess
  ( OSLogEntryFromProcessOverrides(..)
  , defaultOSLogEntryFromProcessOverrides
  , newOSLogEntryFromProcess
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

-- | Overrides record for @\@protocol OSLogEntryFromProcess@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data OSLogEntryFromProcessOverrides = OSLogEntryFromProcessOverrides
  { _activityIdentifier :: !(Maybe (IO Int))
  , _process :: !(Maybe (IO RawId))
  , _processIdentifier :: !(Maybe (IO Int))
  , _sender :: !(Maybe (IO RawId))
  , _threadIdentifier :: !(Maybe (IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultOSLogEntryFromProcessOverrides :: OSLogEntryFromProcessOverrides
defaultOSLogEntryFromProcessOverrides = OSLogEntryFromProcessOverrides
  { _activityIdentifier = Nothing
  , _process = Nothing
  , _processIdentifier = Nothing
  , _sender = Nothing
  , _threadIdentifier = Nothing
  }

foreign import ccall "wrapper"
  wrap_i
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CInt)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CInt))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE osLogEntryFromProcessDelegateClass #-}
osLogEntryFromProcessDelegateClass :: Class
osLogEntryFromProcessDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsOSLogEntryFromProcess" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_activityIdentifier = unSelector (mkSelector "activityIdentifier")
      sel_process = unSelector (mkSelector "process")
      sel_processIdentifier = unSelector (mkSelector "processIdentifier")
      sel_sender = unSelector (mkSelector "sender")
      sel_threadIdentifier = unSelector (mkSelector "threadIdentifier")
  -- activityIdentifier
  stub_0 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO OSLogEntryFromProcessOverrides
    case _activityIdentifier rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "activityIdentifier" "Q@:" stub_0

  -- process
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO OSLogEntryFromProcessOverrides
    case _process rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "process" "@@:" stub_1

  -- processIdentifier
  stub_2 <- wrap_i $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO OSLogEntryFromProcessOverrides
    case _processIdentifier rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "processIdentifier" "i@:" stub_2

  -- sender
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO OSLogEntryFromProcessOverrides
    case _sender rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "sender" "@@:" stub_3

  -- threadIdentifier
  stub_4 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO OSLogEntryFromProcessOverrides
    case _threadIdentifier rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "threadIdentifier" "Q@:" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO OSLogEntryFromProcessOverrides
    if queriedSel == sel_activityIdentifier then pure (maybe 0 (const 1) (_activityIdentifier rec_))
    else if queriedSel == sel_process then pure (maybe 0 (const 1) (_process rec_))
    else if queriedSel == sel_processIdentifier then pure (maybe 0 (const 1) (_processIdentifier rec_))
    else if queriedSel == sel_sender then pure (maybe 0 (const 1) (_sender rec_))
    else if queriedSel == sel_threadIdentifier then pure (maybe 0 (const 1) (_threadIdentifier rec_))
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
newOSLogEntryFromProcess :: OSLogEntryFromProcessOverrides -> IO RawId
newOSLogEntryFromProcess overrides = do
  inst <- class_createInstance osLogEntryFromProcessDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
