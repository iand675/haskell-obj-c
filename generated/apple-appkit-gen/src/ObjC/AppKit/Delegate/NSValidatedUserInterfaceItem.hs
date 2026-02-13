{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSValidatedUserInterfaceItem@.
--
-- Usage:
--
-- @
-- delegate <- newNSValidatedUserInterfaceItem defaultNSValidatedUserInterfaceItemOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSValidatedUserInterfaceItem
  ( NSValidatedUserInterfaceItemOverrides(..)
  , defaultNSValidatedUserInterfaceItemOverrides
  , newNSValidatedUserInterfaceItem
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

-- | Overrides record for @\@protocol NSValidatedUserInterfaceItem@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSValidatedUserInterfaceItemOverrides = NSValidatedUserInterfaceItemOverrides
  { _action :: !(Maybe (IO Sel))
  , _tag :: !(Maybe (IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultNSValidatedUserInterfaceItemOverrides :: NSValidatedUserInterfaceItemOverrides
defaultNSValidatedUserInterfaceItemOverrides = NSValidatedUserInterfaceItemOverrides
  { _action = Nothing
  , _tag = Nothing
  }

foreign import ccall "wrapper"
  wrap_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong))

foreign import ccall "wrapper"
  wrap_sel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCSel))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCSel)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsValidatedUserInterfaceItemDelegateClass #-}
nsValidatedUserInterfaceItemDelegateClass :: Class
nsValidatedUserInterfaceItemDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSValidatedUserInterfaceItem" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_action = unSelector (mkSelector "action")
      sel_tag = unSelector (mkSelector "tag")
  -- action
  stub_0 <- wrap_sel $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSValidatedUserInterfaceItemOverrides
    case _action rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (unSelector r)
  addObjCMethod cls "action" ":@:" stub_0

  -- tag
  stub_1 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSValidatedUserInterfaceItemOverrides
    case _tag rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "tag" "q@:" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSValidatedUserInterfaceItemOverrides
    if queriedSel == sel_action then pure (maybe 0 (const 1) (_action rec_))
    else if queriedSel == sel_tag then pure (maybe 0 (const 1) (_tag rec_))
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
newNSValidatedUserInterfaceItem :: NSValidatedUserInterfaceItemOverrides -> IO RawId
newNSValidatedUserInterfaceItem overrides = do
  inst <- class_createInstance nsValidatedUserInterfaceItemDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
