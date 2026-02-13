{-# LANGUAGE ForeignFunctionInterface #-}

-- | Compose multiple delegate objects into a single ObjC object via
-- message forwarding.
--
-- When an ObjC API requires the /same/ object to serve as both delegate
-- and data source (or conform to multiple protocols), this module lets
-- you combine per-protocol delegate objects without Template Haskell.
--
-- = How It Works
--
-- The proxy class overrides @forwardingTargetForSelector:@ — the ObjC
-- runtime calls this before raising @doesNotRecognizeSelector:@ when a
-- message arrives that the proxy doesn't directly implement.  The
-- override iterates through the delegate list and returns the first one
-- that responds, causing the runtime to re-dispatch the message to that
-- delegate.  No @NSInvocation@ boxing is involved; this is the fast
-- forwarding path.
--
-- = Usage
--
-- @
-- dataSource <- newNSTableViewDataSource $ ...
-- delegate   <- newNSTableViewDelegate $ ...
-- combined   <- newDelegateProxy [dataSource, delegate]
--
-- -- Set 'combined' as both delegate and dataSource on the table view.
-- @
--
-- When separate objects work fine (most cases), prefer that over a
-- proxy — it's simpler and avoids the extra forwarding hop.
module ObjC.Runtime.DelegateProxy
  ( newDelegateProxy
  ) where

import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.C.Types (CULong(..))
import Foreign.C.String (withCString)
import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import Foreign.LibFFI (retCULong, argPtr)
import System.IO.Unsafe (unsafePerformIO)

import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass, class_createInstance)
import ObjC.Runtime.ClassBuilder (objc_allocateClassPair, objc_registerClassPair)
import ObjC.Runtime.MsgSend (sendMsg, sendSuperMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.StableIvar
  (addHsDataIvar, writeHsData, readHsData, addStablePtrDeallocHandler,
   addObjCMethod, castIMP)

-- ---------------------------------------------------------------------------
-- Wrapper types for the two custom IMPs
-- ---------------------------------------------------------------------------

-- forwardingTargetForSelector:  signature: (id, SEL, SEL) -> id
foreign import ccall "wrapper"
  wrapForwardingIMP
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel
                    -> IO (Ptr ObjCObject)))

-- respondsToSelector:  signature: (id, SEL, SEL) -> BOOL (CULong on ARM64)
foreign import ccall "wrapper"
  wrapRespondsIMP
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- ---------------------------------------------------------------------------
-- The proxy class (created once)
-- ---------------------------------------------------------------------------

{-# NOINLINE delegateProxyClass #-}
delegateProxyClass :: Class
delegateProxyClass = unsafePerformIO $ do
  super <- getRequiredClass "NSObject"
  cls <- withCString "HsDelegateProxy" $ \n ->
    objc_allocateClassPair super n 0
  addHsDataIvar cls

  -- forwardingTargetForSelector:
  fwdStub <- wrapForwardingIMP $ \self _cmd sel -> do
    delegates <- readDelegates self
    findResponder delegates sel
  addObjCMethod cls "forwardingTargetForSelector:" "@@::" fwdStub

  -- respondsToSelector:
  rspStub <- wrapRespondsIMP $ \self _cmd sel -> do
    delegates <- readDelegates self
    found <- anyResponds delegates sel
    if found
      then pure 1
      else do
        -- Fall through to NSObject's respondsToSelector: for inherited
        -- selectors (dealloc, class, description, etc.)
        let super_ = ObjCSuper (RawId self) super
        sendSuperMsg super_ respondsSel retCULong
          [argPtr (castPtr sel :: Ptr ())]
  addObjCMethod cls "respondsToSelector:" "B@::" rspStub

  addStablePtrDeallocHandler cls
  objc_registerClassPair cls
  pure cls

respondsSel :: Sel
respondsSel = mkSelector "respondsToSelector:"

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Create a proxy that forwards messages to the first delegate that
-- responds to each selector.
--
-- The delegates are tried in order, so earlier entries take priority
-- when multiple delegates respond to the same selector.
newDelegateProxy :: [RawId] -> IO RawId
newDelegateProxy delegates = do
  inst <- class_createInstance delegateProxyClass 0
  sp <- newStablePtr delegates
  writeHsData inst sp
  pure inst

-- ---------------------------------------------------------------------------
-- Internals
-- ---------------------------------------------------------------------------

readDelegates :: Ptr ObjCObject -> IO [RawId]
readDelegates self = do
  sp <- readHsData self
  deRefStablePtr sp

-- | Find the first delegate that responds to a selector.
-- Returns its pointer for @forwardingTargetForSelector:@ to return,
-- or nullPtr if none responds (triggers doesNotRecognizeSelector:).
findResponder :: [RawId] -> Ptr ObjCSel -> IO (Ptr ObjCObject)
findResponder [] _sel = pure nullPtr
findResponder (d : ds) sel = do
  responds <- delegateResponds d sel
  if responds
    then pure (castPtr (unRawId d))
    else findResponder ds sel

-- | Check whether any delegate responds to a selector.
anyResponds :: [RawId] -> Ptr ObjCSel -> IO Bool
anyResponds [] _sel = pure False
anyResponds (d : ds) sel = do
  responds <- delegateResponds d sel
  if responds
    then pure True
    else anyResponds ds sel

-- | Ask a single delegate whether it responds to a selector.
delegateResponds :: RawId -> Ptr ObjCSel -> IO Bool
delegateResponds obj sel = do
  result <- sendMsg obj respondsSel retCULong
    [argPtr (castPtr sel :: Ptr ())]
  pure (result /= 0)
