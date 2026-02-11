{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSEditorRegistration@.
--
-- Usage:
--
-- @
-- delegate <- newNSEditorRegistration defaultNSEditorRegistrationOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSEditorRegistration
  ( NSEditorRegistrationOverrides(..)
  , defaultNSEditorRegistrationOverrides
  , newNSEditorRegistration
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

-- | Overrides record for @\@protocol NSEditorRegistration@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSEditorRegistrationOverrides = NSEditorRegistrationOverrides
  { _objectDidBeginEditing :: !(Maybe (RawId -> IO ()))
  , _objectDidEndEditing :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSEditorRegistrationOverrides :: NSEditorRegistrationOverrides
defaultNSEditorRegistrationOverrides = NSEditorRegistrationOverrides
  { _objectDidBeginEditing = Nothing
  , _objectDidEndEditing = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsEditorRegistrationDelegateClass #-}
nsEditorRegistrationDelegateClass :: Class
nsEditorRegistrationDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSEditorRegistration" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_objectDidBeginEditing = unSelector (mkSelector "objectDidBeginEditing:")
      sel_objectDidEndEditing = unSelector (mkSelector "objectDidEndEditing:")
  -- objectDidBeginEditing:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSEditorRegistrationOverrides
    case _objectDidBeginEditing rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "objectDidBeginEditing:" "v@:@" stub_0

  -- objectDidEndEditing:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSEditorRegistrationOverrides
    case _objectDidEndEditing rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "objectDidEndEditing:" "v@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSEditorRegistrationOverrides
    if queriedSel == sel_objectDidBeginEditing then pure (maybe 0 (const 1) (_objectDidBeginEditing rec_))
    else if queriedSel == sel_objectDidEndEditing then pure (maybe 0 (const 1) (_objectDidEndEditing rec_))
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
newNSEditorRegistration :: NSEditorRegistrationOverrides -> IO RawId
newNSEditorRegistration overrides = do
  inst <- class_createInstance nsEditorRegistrationDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
