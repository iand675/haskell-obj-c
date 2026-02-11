{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSUserInterfaceCompression@.
--
-- Usage:
--
-- @
-- delegate <- newNSUserInterfaceCompression defaultNSUserInterfaceCompressionOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSUserInterfaceCompression
  ( NSUserInterfaceCompressionOverrides(..)
  , defaultNSUserInterfaceCompressionOverrides
  , newNSUserInterfaceCompression
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

-- | Overrides record for @\@protocol NSUserInterfaceCompression@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSUserInterfaceCompressionOverrides = NSUserInterfaceCompressionOverrides
  { _compressWithPrioritizedCompressionOptions :: !(Maybe (RawId -> IO ()))
  , _activeCompressionOptions :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSUserInterfaceCompressionOverrides :: NSUserInterfaceCompressionOverrides
defaultNSUserInterfaceCompressionOverrides = NSUserInterfaceCompressionOverrides
  { _compressWithPrioritizedCompressionOptions = Nothing
  , _activeCompressionOptions = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsUserInterfaceCompressionDelegateClass #-}
nsUserInterfaceCompressionDelegateClass :: Class
nsUserInterfaceCompressionDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSUserInterfaceCompression" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_compressWithPrioritizedCompressionOptions = unSelector (mkSelector "compressWithPrioritizedCompressionOptions:")
      sel_activeCompressionOptions = unSelector (mkSelector "activeCompressionOptions")
  -- compressWithPrioritizedCompressionOptions:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSUserInterfaceCompressionOverrides
    case _compressWithPrioritizedCompressionOptions rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "compressWithPrioritizedCompressionOptions:" "v@:@" stub_0

  -- activeCompressionOptions
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSUserInterfaceCompressionOverrides
    case _activeCompressionOptions rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "activeCompressionOptions" "@@:" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSUserInterfaceCompressionOverrides
    if queriedSel == sel_compressWithPrioritizedCompressionOptions then pure (maybe 0 (const 1) (_compressWithPrioritizedCompressionOptions rec_))
    else if queriedSel == sel_activeCompressionOptions then pure (maybe 0 (const 1) (_activeCompressionOptions rec_))
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
newNSUserInterfaceCompression :: NSUserInterfaceCompressionOverrides -> IO RawId
newNSUserInterfaceCompression overrides = do
  inst <- class_createInstance nsUserInterfaceCompressionDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
