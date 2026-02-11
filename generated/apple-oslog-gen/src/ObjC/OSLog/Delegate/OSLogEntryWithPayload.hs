{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol OSLogEntryWithPayload@.
--
-- Usage:
--
-- @
-- delegate <- newOSLogEntryWithPayload defaultOSLogEntryWithPayloadOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.OSLog.Delegate.OSLogEntryWithPayload
  ( OSLogEntryWithPayloadOverrides(..)
  , defaultOSLogEntryWithPayloadOverrides
  , newOSLogEntryWithPayload
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

-- | Overrides record for @\@protocol OSLogEntryWithPayload@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data OSLogEntryWithPayloadOverrides = OSLogEntryWithPayloadOverrides
  { _category :: !(Maybe (IO RawId))
  , _components :: !(Maybe (IO RawId))
  , _formatString :: !(Maybe (IO RawId))
  , _subsystem :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultOSLogEntryWithPayloadOverrides :: OSLogEntryWithPayloadOverrides
defaultOSLogEntryWithPayloadOverrides = OSLogEntryWithPayloadOverrides
  { _category = Nothing
  , _components = Nothing
  , _formatString = Nothing
  , _subsystem = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE osLogEntryWithPayloadDelegateClass #-}
osLogEntryWithPayloadDelegateClass :: Class
osLogEntryWithPayloadDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsOSLogEntryWithPayload" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_category = unSelector (mkSelector "category")
      sel_components = unSelector (mkSelector "components")
      sel_formatString = unSelector (mkSelector "formatString")
      sel_subsystem = unSelector (mkSelector "subsystem")
  -- category
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO OSLogEntryWithPayloadOverrides
    case _category rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "category" "@@:" stub_0

  -- components
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO OSLogEntryWithPayloadOverrides
    case _components rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "components" "@@:" stub_1

  -- formatString
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO OSLogEntryWithPayloadOverrides
    case _formatString rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "formatString" "@@:" stub_2

  -- subsystem
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO OSLogEntryWithPayloadOverrides
    case _subsystem rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "subsystem" "@@:" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO OSLogEntryWithPayloadOverrides
    if queriedSel == sel_category then pure (maybe 0 (const 1) (_category rec_))
    else if queriedSel == sel_components then pure (maybe 0 (const 1) (_components rec_))
    else if queriedSel == sel_formatString then pure (maybe 0 (const 1) (_formatString rec_))
    else if queriedSel == sel_subsystem then pure (maybe 0 (const 1) (_subsystem rec_))
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
newOSLogEntryWithPayload :: OSLogEntryWithPayloadOverrides -> IO RawId
newOSLogEntryWithPayload overrides = do
  inst <- class_createInstance osLogEntryWithPayloadDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
