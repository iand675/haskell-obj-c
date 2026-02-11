{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GKGameModelPlayer@.
--
-- Usage:
--
-- @
-- delegate <- newGKGameModelPlayer defaultGKGameModelPlayerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameplayKit.Delegate.GKGameModelPlayer
  ( GKGameModelPlayerOverrides(..)
  , defaultGKGameModelPlayerOverrides
  , newGKGameModelPlayer
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

-- | Overrides record for @\@protocol GKGameModelPlayer@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GKGameModelPlayerOverrides = GKGameModelPlayerOverrides
  { _playerId :: !(Maybe (IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultGKGameModelPlayerOverrides :: GKGameModelPlayerOverrides
defaultGKGameModelPlayerOverrides = GKGameModelPlayerOverrides
  { _playerId = Nothing
  }

foreign import ccall "wrapper"
  wrap_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE gkGameModelPlayerDelegateClass #-}
gkGameModelPlayerDelegateClass :: Class
gkGameModelPlayerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGKGameModelPlayer" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_playerId = unSelector (mkSelector "playerId")
  -- playerId
  stub_0 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKGameModelPlayerOverrides
    case _playerId rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "playerId" "q@:" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKGameModelPlayerOverrides
    if queriedSel == sel_playerId then pure (maybe 0 (const 1) (_playerId rec_))
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
newGKGameModelPlayer :: GKGameModelPlayerOverrides -> IO RawId
newGKGameModelPlayer overrides = do
  inst <- class_createInstance gkGameModelPlayerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
