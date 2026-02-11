{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol TKTokenDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newTKTokenDelegate defaultTKTokenDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CryptoTokenKit.Delegate.TKTokenDelegate
  ( TKTokenDelegateOverrides(..)
  , defaultTKTokenDelegateOverrides
  , newTKTokenDelegate
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

-- | Overrides record for @\@protocol TKTokenDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data TKTokenDelegateOverrides = TKTokenDelegateOverrides
  { _token_createSessionWithError :: !(Maybe (RawId -> RawId -> IO RawId))
  , _token_terminateSession :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultTKTokenDelegateOverrides :: TKTokenDelegateOverrides
defaultTKTokenDelegateOverrides = TKTokenDelegateOverrides
  { _token_createSessionWithError = Nothing
  , _token_terminateSession = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE tkTokenDelegateDelegateClass #-}
tkTokenDelegateDelegateClass :: Class
tkTokenDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsTKTokenDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_token_createSessionWithError = unSelector (mkSelector "token:createSessionWithError:")
      sel_token_terminateSession = unSelector (mkSelector "token:terminateSession:")
  -- token:createSessionWithError:
  stub_0 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO TKTokenDelegateOverrides
    case _token_createSessionWithError rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "token:createSessionWithError:" "@@:@@" stub_0

  -- token:terminateSession:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO TKTokenDelegateOverrides
    case _token_terminateSession rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "token:terminateSession:" "v@:@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO TKTokenDelegateOverrides
    if queriedSel == sel_token_createSessionWithError then pure (maybe 0 (const 1) (_token_createSessionWithError rec_))
    else if queriedSel == sel_token_terminateSession then pure (maybe 0 (const 1) (_token_terminateSession rec_))
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
newTKTokenDelegate :: TKTokenDelegateOverrides -> IO RawId
newTKTokenDelegate overrides = do
  inst <- class_createInstance tkTokenDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
