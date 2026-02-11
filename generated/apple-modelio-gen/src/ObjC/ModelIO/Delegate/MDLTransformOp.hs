{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MDLTransformOp@.
--
-- Usage:
--
-- @
-- delegate <- newMDLTransformOp defaultMDLTransformOpOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.ModelIO.Delegate.MDLTransformOp
  ( MDLTransformOpOverrides(..)
  , defaultMDLTransformOpOverrides
  , newMDLTransformOp
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

-- | Overrides record for @\@protocol MDLTransformOp@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MDLTransformOpOverrides = MDLTransformOpOverrides
  { _name :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMDLTransformOpOverrides :: MDLTransformOpOverrides
defaultMDLTransformOpOverrides = MDLTransformOpOverrides
  { _name = Nothing
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
{-# NOINLINE mdlTransformOpDelegateClass #-}
mdlTransformOpDelegateClass :: Class
mdlTransformOpDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMDLTransformOp" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_name = unSelector (mkSelector "name")
  -- name
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MDLTransformOpOverrides
    case _name rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "name" "@@:" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MDLTransformOpOverrides
    if queriedSel == sel_name then pure (maybe 0 (const 1) (_name rec_))
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
newMDLTransformOp :: MDLTransformOpOverrides -> IO RawId
newMDLTransformOp overrides = do
  inst <- class_createInstance mdlTransformOpDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
