{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSImageDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSImageDelegate defaultNSImageDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSImageDelegate
  ( NSImageDelegateOverrides(..)
  , defaultNSImageDelegateOverrides
  , newNSImageDelegate
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

-- | Overrides record for @\@protocol NSImageDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSImageDelegateOverrides = NSImageDelegateOverrides
  { _image_willLoadRepresentation :: !(Maybe (RawId -> RawId -> IO ()))
  , _image_didLoadRepresentationHeader :: !(Maybe (RawId -> RawId -> IO ()))
  , _image_didLoadPartOfRepresentation_withValidRows :: !(Maybe (RawId -> RawId -> Int -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSImageDelegateOverrides :: NSImageDelegateOverrides
defaultNSImageDelegateOverrides = NSImageDelegateOverrides
  { _image_willLoadRepresentation = Nothing
  , _image_didLoadRepresentationHeader = Nothing
  , _image_didLoadPartOfRepresentation_withValidRows = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsImageDelegateDelegateClass #-}
nsImageDelegateDelegateClass :: Class
nsImageDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSImageDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_image_willLoadRepresentation = unSelector (mkSelector "image:willLoadRepresentation:")
      sel_image_didLoadRepresentationHeader = unSelector (mkSelector "image:didLoadRepresentationHeader:")
      sel_image_didLoadPartOfRepresentation_withValidRows = unSelector (mkSelector "image:didLoadPartOfRepresentation:withValidRows:")
  -- image:willLoadRepresentation:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSImageDelegateOverrides
    case _image_willLoadRepresentation rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "image:willLoadRepresentation:" "v@:@@" stub_0

  -- image:didLoadRepresentationHeader:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSImageDelegateOverrides
    case _image_didLoadRepresentationHeader rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "image:didLoadRepresentationHeader:" "v@:@@" stub_1

  -- image:didLoadPartOfRepresentation:withValidRows:
  stub_2 <- wrap_at_at_q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSImageDelegateOverrides
    case _image_didLoadPartOfRepresentation_withValidRows rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (fromIntegral arg2)
  addObjCMethod cls "image:didLoadPartOfRepresentation:withValidRows:" "v@:@@q" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSImageDelegateOverrides
    if queriedSel == sel_image_willLoadRepresentation then pure (maybe 0 (const 1) (_image_willLoadRepresentation rec_))
    else if queriedSel == sel_image_didLoadRepresentationHeader then pure (maybe 0 (const 1) (_image_didLoadRepresentationHeader rec_))
    else if queriedSel == sel_image_didLoadPartOfRepresentation_withValidRows then pure (maybe 0 (const 1) (_image_didLoadPartOfRepresentation_withValidRows rec_))
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
newNSImageDelegate :: NSImageDelegateOverrides -> IO RawId
newNSImageDelegate overrides = do
  inst <- class_createInstance nsImageDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
