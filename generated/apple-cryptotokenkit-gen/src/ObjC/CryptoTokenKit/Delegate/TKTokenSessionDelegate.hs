{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol TKTokenSessionDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newTKTokenSessionDelegate defaultTKTokenSessionDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CryptoTokenKit.Delegate.TKTokenSessionDelegate
  ( TKTokenSessionDelegateOverrides(..)
  , defaultTKTokenSessionDelegateOverrides
  , newTKTokenSessionDelegate
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

-- | Overrides record for @\@protocol TKTokenSessionDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data TKTokenSessionDelegateOverrides = TKTokenSessionDelegateOverrides
  { _tokenSession_signData_usingKey_algorithm_error :: !(Maybe (RawId -> RawId -> RawId -> RawId -> RawId -> IO RawId))
  , _tokenSession_decryptData_usingKey_algorithm_error :: !(Maybe (RawId -> RawId -> RawId -> RawId -> RawId -> IO RawId))
  , _tokenSession_performKeyExchangeWithPublicKey_usingKey_algorithm_parameters_error :: !(Maybe (RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultTKTokenSessionDelegateOverrides :: TKTokenSessionDelegateOverrides
defaultTKTokenSessionDelegateOverrides = TKTokenSessionDelegateOverrides
  { _tokenSession_signData_usingKey_algorithm_error = Nothing
  , _tokenSession_decryptData_usingKey_algorithm_error = Nothing
  , _tokenSession_performKeyExchangeWithPublicKey_usingKey_algorithm_parameters_error = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE tkTokenSessionDelegateDelegateClass #-}
tkTokenSessionDelegateDelegateClass :: Class
tkTokenSessionDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsTKTokenSessionDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_tokenSession_signData_usingKey_algorithm_error = unSelector (mkSelector "tokenSession:signData:usingKey:algorithm:error:")
      sel_tokenSession_decryptData_usingKey_algorithm_error = unSelector (mkSelector "tokenSession:decryptData:usingKey:algorithm:error:")
      sel_tokenSession_performKeyExchangeWithPublicKey_usingKey_algorithm_parameters_error = unSelector (mkSelector "tokenSession:performKeyExchangeWithPublicKey:usingKey:algorithm:parameters:error:")
  -- tokenSession:signData:usingKey:algorithm:error:
  stub_0 <- wrap_at_at_at_at_at_at $ \self _cmd arg0 arg1 arg2 arg3 arg4 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO TKTokenSessionDelegateOverrides
    case _tokenSession_signData_usingKey_algorithm_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3) (RawId arg4)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tokenSession:signData:usingKey:algorithm:error:" "@@:@@@@@" stub_0

  -- tokenSession:decryptData:usingKey:algorithm:error:
  stub_1 <- wrap_at_at_at_at_at_at $ \self _cmd arg0 arg1 arg2 arg3 arg4 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO TKTokenSessionDelegateOverrides
    case _tokenSession_decryptData_usingKey_algorithm_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3) (RawId arg4)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tokenSession:decryptData:usingKey:algorithm:error:" "@@:@@@@@" stub_1

  -- tokenSession:performKeyExchangeWithPublicKey:usingKey:algorithm:parameters:error:
  stub_2 <- wrap_at_at_at_at_at_at_at $ \self _cmd arg0 arg1 arg2 arg3 arg4 arg5 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO TKTokenSessionDelegateOverrides
    case _tokenSession_performKeyExchangeWithPublicKey_usingKey_algorithm_parameters_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3) (RawId arg4) (RawId arg5)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tokenSession:performKeyExchangeWithPublicKey:usingKey:algorithm:parameters:error:" "@@:@@@@@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO TKTokenSessionDelegateOverrides
    if queriedSel == sel_tokenSession_signData_usingKey_algorithm_error then pure (maybe 0 (const 1) (_tokenSession_signData_usingKey_algorithm_error rec_))
    else if queriedSel == sel_tokenSession_decryptData_usingKey_algorithm_error then pure (maybe 0 (const 1) (_tokenSession_decryptData_usingKey_algorithm_error rec_))
    else if queriedSel == sel_tokenSession_performKeyExchangeWithPublicKey_usingKey_algorithm_parameters_error then pure (maybe 0 (const 1) (_tokenSession_performKeyExchangeWithPublicKey_usingKey_algorithm_parameters_error rec_))
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
newTKTokenSessionDelegate :: TKTokenSessionDelegateOverrides -> IO RawId
newTKTokenSessionDelegate overrides = do
  inst <- class_createInstance tkTokenSessionDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
