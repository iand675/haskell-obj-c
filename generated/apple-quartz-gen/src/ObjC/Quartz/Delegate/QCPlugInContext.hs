{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol QCPlugInContext@.
--
-- Usage:
--
-- @
-- delegate <- newQCPlugInContext defaultQCPlugInContextOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Quartz.Delegate.QCPlugInContext
  ( QCPlugInContextOverrides(..)
  , defaultQCPlugInContextOverrides
  , newQCPlugInContext
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

-- | Overrides record for @\@protocol QCPlugInContext@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data QCPlugInContextOverrides = QCPlugInContextOverrides
  { _compositionURL :: !(Maybe (IO RawId))
  , _logMessage :: !(Maybe (RawId -> IO ()))
  , _userInfo :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultQCPlugInContextOverrides :: QCPlugInContextOverrides
defaultQCPlugInContextOverrides = QCPlugInContextOverrides
  { _compositionURL = Nothing
  , _logMessage = Nothing
  , _userInfo = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE qcPlugInContextDelegateClass #-}
qcPlugInContextDelegateClass :: Class
qcPlugInContextDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsQCPlugInContext" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_compositionURL = unSelector (mkSelector "compositionURL")
      sel_logMessage = unSelector (mkSelector "logMessage:")
      sel_userInfo = unSelector (mkSelector "userInfo")
  -- compositionURL
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCPlugInContextOverrides
    case _compositionURL rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "compositionURL" "@@:" stub_0

  -- logMessage:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCPlugInContextOverrides
    case _logMessage rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "logMessage:" "v@:@" stub_1

  -- userInfo
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCPlugInContextOverrides
    case _userInfo rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "userInfo" "@@:" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCPlugInContextOverrides
    if queriedSel == sel_compositionURL then pure (maybe 0 (const 1) (_compositionURL rec_))
    else if queriedSel == sel_logMessage then pure (maybe 0 (const 1) (_logMessage rec_))
    else if queriedSel == sel_userInfo then pure (maybe 0 (const 1) (_userInfo rec_))
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
newQCPlugInContext :: QCPlugInContextOverrides -> IO RawId
newQCPlugInContext overrides = do
  inst <- class_createInstance qcPlugInContextDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
