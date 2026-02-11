{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CAMetalDrawable@.
--
-- Usage:
--
-- @
-- delegate <- newCAMetalDrawable defaultCAMetalDrawableOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.QuartzCore.Delegate.CAMetalDrawable
  ( CAMetalDrawableOverrides(..)
  , defaultCAMetalDrawableOverrides
  , newCAMetalDrawable
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

-- | Overrides record for @\@protocol CAMetalDrawable@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CAMetalDrawableOverrides = CAMetalDrawableOverrides
  { _texture :: !(Maybe (IO RawId))
  , _layer :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultCAMetalDrawableOverrides :: CAMetalDrawableOverrides
defaultCAMetalDrawableOverrides = CAMetalDrawableOverrides
  { _texture = Nothing
  , _layer = Nothing
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
{-# NOINLINE caMetalDrawableDelegateClass #-}
caMetalDrawableDelegateClass :: Class
caMetalDrawableDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCAMetalDrawable" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_texture = unSelector (mkSelector "texture")
      sel_layer = unSelector (mkSelector "layer")
  -- texture
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAMetalDrawableOverrides
    case _texture rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "texture" "@@:" stub_0

  -- layer
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAMetalDrawableOverrides
    case _layer rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "layer" "@@:" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CAMetalDrawableOverrides
    if queriedSel == sel_texture then pure (maybe 0 (const 1) (_texture rec_))
    else if queriedSel == sel_layer then pure (maybe 0 (const 1) (_layer rec_))
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
newCAMetalDrawable :: CAMetalDrawableOverrides -> IO RawId
newCAMetalDrawable overrides = do
  inst <- class_createInstance caMetalDrawableDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
