{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSUserInterfaceItemIdentification@.
--
-- Usage:
--
-- @
-- delegate <- newNSUserInterfaceItemIdentification defaultNSUserInterfaceItemIdentificationOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSUserInterfaceItemIdentification
  ( NSUserInterfaceItemIdentificationOverrides(..)
  , defaultNSUserInterfaceItemIdentificationOverrides
  , newNSUserInterfaceItemIdentification
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

-- | Overrides record for @\@protocol NSUserInterfaceItemIdentification@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSUserInterfaceItemIdentificationOverrides = NSUserInterfaceItemIdentificationOverrides
  { _identifier :: !(Maybe (IO RawId))
  , _setIdentifier :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSUserInterfaceItemIdentificationOverrides :: NSUserInterfaceItemIdentificationOverrides
defaultNSUserInterfaceItemIdentificationOverrides = NSUserInterfaceItemIdentificationOverrides
  { _identifier = Nothing
  , _setIdentifier = Nothing
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
{-# NOINLINE nsUserInterfaceItemIdentificationDelegateClass #-}
nsUserInterfaceItemIdentificationDelegateClass :: Class
nsUserInterfaceItemIdentificationDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSUserInterfaceItemIdentification" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_identifier = unSelector (mkSelector "identifier")
      sel_setIdentifier = unSelector (mkSelector "setIdentifier:")
  -- identifier
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSUserInterfaceItemIdentificationOverrides
    case _identifier rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "identifier" "@@:" stub_0

  -- setIdentifier:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSUserInterfaceItemIdentificationOverrides
    case _setIdentifier rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setIdentifier:" "v@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSUserInterfaceItemIdentificationOverrides
    if queriedSel == sel_identifier then pure (maybe 0 (const 1) (_identifier rec_))
    else if queriedSel == sel_setIdentifier then pure (maybe 0 (const 1) (_setIdentifier rec_))
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
newNSUserInterfaceItemIdentification :: NSUserInterfaceItemIdentificationOverrides -> IO RawId
newNSUserInterfaceItemIdentification overrides = do
  inst <- class_createInstance nsUserInterfaceItemIdentificationDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
