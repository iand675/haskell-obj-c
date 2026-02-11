{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLFunctionLogDebugLocation@.
--
-- Usage:
--
-- @
-- delegate <- newMTLFunctionLogDebugLocation defaultMTLFunctionLogDebugLocationOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLFunctionLogDebugLocation
  ( MTLFunctionLogDebugLocationOverrides(..)
  , defaultMTLFunctionLogDebugLocationOverrides
  , newMTLFunctionLogDebugLocation
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

-- | Overrides record for @\@protocol MTLFunctionLogDebugLocation@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLFunctionLogDebugLocationOverrides = MTLFunctionLogDebugLocationOverrides
  { _functionName :: !(Maybe (IO RawId))
  , _url :: !(Maybe (IO RawId))
  , _line :: !(Maybe (IO Int))
  , _column :: !(Maybe (IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLFunctionLogDebugLocationOverrides :: MTLFunctionLogDebugLocationOverrides
defaultMTLFunctionLogDebugLocationOverrides = MTLFunctionLogDebugLocationOverrides
  { _functionName = Nothing
  , _url = Nothing
  , _line = Nothing
  , _column = Nothing
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
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtlFunctionLogDebugLocationDelegateClass #-}
mtlFunctionLogDebugLocationDelegateClass :: Class
mtlFunctionLogDebugLocationDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLFunctionLogDebugLocation" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_functionName = unSelector (mkSelector "functionName")
      sel_url = unSelector (mkSelector "URL")
      sel_line = unSelector (mkSelector "line")
      sel_column = unSelector (mkSelector "column")
  -- functionName
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFunctionLogDebugLocationOverrides
    case _functionName rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "functionName" "@@:" stub_0

  -- URL
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFunctionLogDebugLocationOverrides
    case _url rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "URL" "@@:" stub_1

  -- line
  stub_2 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFunctionLogDebugLocationOverrides
    case _line rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "line" "Q@:" stub_2

  -- column
  stub_3 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFunctionLogDebugLocationOverrides
    case _column rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "column" "Q@:" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFunctionLogDebugLocationOverrides
    if queriedSel == sel_functionName then pure (maybe 0 (const 1) (_functionName rec_))
    else if queriedSel == sel_url then pure (maybe 0 (const 1) (_url rec_))
    else if queriedSel == sel_line then pure (maybe 0 (const 1) (_line rec_))
    else if queriedSel == sel_column then pure (maybe 0 (const 1) (_column rec_))
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
newMTLFunctionLogDebugLocation :: MTLFunctionLogDebugLocationOverrides -> IO RawId
newMTLFunctionLogDebugLocation overrides = do
  inst <- class_createInstance mtlFunctionLogDebugLocationDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
