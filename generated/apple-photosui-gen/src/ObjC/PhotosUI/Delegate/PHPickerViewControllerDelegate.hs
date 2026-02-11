{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol PHPickerViewControllerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newPHPickerViewControllerDelegate defaultPHPickerViewControllerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.PhotosUI.Delegate.PHPickerViewControllerDelegate
  ( PHPickerViewControllerDelegateOverrides(..)
  , defaultPHPickerViewControllerDelegateOverrides
  , newPHPickerViewControllerDelegate
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

-- | Overrides record for @\@protocol PHPickerViewControllerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data PHPickerViewControllerDelegateOverrides = PHPickerViewControllerDelegateOverrides
  { _picker_didFinishPicking :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultPHPickerViewControllerDelegateOverrides :: PHPickerViewControllerDelegateOverrides
defaultPHPickerViewControllerDelegateOverrides = PHPickerViewControllerDelegateOverrides
  { _picker_didFinishPicking = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE phPickerViewControllerDelegateDelegateClass #-}
phPickerViewControllerDelegateDelegateClass :: Class
phPickerViewControllerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsPHPickerViewControllerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_picker_didFinishPicking = unSelector (mkSelector "picker:didFinishPicking:")
  -- picker:didFinishPicking:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PHPickerViewControllerDelegateOverrides
    case _picker_didFinishPicking rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "picker:didFinishPicking:" "v@:@@" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PHPickerViewControllerDelegateOverrides
    if queriedSel == sel_picker_didFinishPicking then pure (maybe 0 (const 1) (_picker_didFinishPicking rec_))
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
newPHPickerViewControllerDelegate :: PHPickerViewControllerDelegateOverrides -> IO RawId
newPHPickerViewControllerDelegate overrides = do
  inst <- class_createInstance phPickerViewControllerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
