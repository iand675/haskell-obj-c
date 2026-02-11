{-# LANGUAGE ForeignFunctionInterface #-}

-- | Create ObjC objects that respond to target\/action selectors,
-- backed by Haskell closures.  No Template Haskell required.
--
-- Every Cocoa target\/action method has the same C signature:
-- @-(void)action:(id)sender@ (type encoding @v\@:\@@).  This means a
-- single @foreign import ccall \"wrapper\"@ covers all action methods.
--
-- = Usage
--
-- @
-- import ObjC.Runtime
-- import ObjC.Runtime.ActionTarget
--
-- target <- newActionTarget
--   [ (\"increment:\", \\_ -> modifyIORef' counter (+1))
--   , (\"decrement:\", \\_ -> modifyIORef' counter (subtract 1))
--   ]
-- Ctrl.setTarget button target
-- Ctrl.setAction button (mkSelector \"increment:\")
-- @
module ObjC.Runtime.ActionTarget
  ( newActionTarget
  ) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.String (withCString)
import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import System.IO.Unsafe (unsafePerformIO)

import Data.List (sort)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS

import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass, class_createInstance)
import ObjC.Runtime.ClassBuilder (objc_allocateClassPair, objc_registerClassPair)
import ObjC.Runtime.StableIvar
  (addHsDataIvar, writeHsData, readHsData, addStablePtrDeallocHandler,
   addObjCMethod)

-- ---------------------------------------------------------------------------
-- The single wrapper type for all action IMPs: (id, SEL, id) -> void
-- ---------------------------------------------------------------------------

foreign import ccall "wrapper"
  wrapActionIMP
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

-- ---------------------------------------------------------------------------
-- Class cache
-- ---------------------------------------------------------------------------

-- | Global cache: selector set -> registered Class.
--
-- Classes registered with the ObjC runtime live forever (they cannot be
-- unregistered), so caching is both safe and necessary.
{-# NOINLINE classCache #-}
classCache :: MVar (HashMap (HashSet String) Class)
classCache = unsafePerformIO (newMVar HM.empty)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Create an ObjC object that responds to the given action selectors.
--
-- Each selector is wired to a Haskell callback that receives the sender
-- as 'RawId'.  The returned 'RawId' can be set as a control's target.
--
-- Multiple calls with the same set of selectors (regardless of order)
-- share a single ObjC class registration.
newActionTarget :: [(String, RawId -> IO ())] -> IO RawId
newActionTarget actions = do
  let selSet = HS.fromList (fmap fst actions)
  cls <- getOrCreateClass selSet
  inst <- class_createInstance cls 0
  let handlerMap = HM.fromList actions
  sp <- newStablePtr handlerMap
  writeHsData inst sp
  pure inst

-- ---------------------------------------------------------------------------
-- Class creation and caching
-- ---------------------------------------------------------------------------

getOrCreateClass :: HashSet String -> IO Class
getOrCreateClass selSet =
  modifyMVar classCache $ \cache ->
    case HM.lookup selSet cache of
      Just cls -> pure (cache, cls)
      Nothing -> do
        cls <- createActionClass selSet
        pure (HM.insert selSet cls cache, cls)

createActionClass :: HashSet String -> IO Class
createActionClass selSet = do
  super <- getRequiredClass "NSObject"
  -- Sort for a deterministic class name only.
  let className = "HsActionTarget_" ++ selsFingerprint (sort (HS.toList selSet))
  cls <- withCString className $ \n ->
    objc_allocateClassPair super n 0
  addHsDataIvar cls

  -- Register a stub IMP for each selector.  Each stub captures the
  -- selector name so it knows which handler to look up in the map.
  mapM_ (registerActionStub cls) (HS.toList selSet)

  addStablePtrDeallocHandler cls
  objc_registerClassPair cls
  pure cls

registerActionStub :: Class -> String -> IO ()
registerActionStub cls selName = do
  stub <- wrapActionIMP $ \self _cmd sender -> do
    sp <- readHsData self
    handlerMap <- deRefStablePtr sp :: IO (HashMap String (RawId -> IO ()))
    case HM.lookup selName handlerMap of
      Just handler -> handler (RawId sender)
      Nothing      -> pure ()
  addObjCMethod cls selName "v@:@" stub

-- ---------------------------------------------------------------------------
-- Fingerprinting
-- ---------------------------------------------------------------------------

-- | Produce a short fingerprint from a sorted list of selector names
-- for use in the ObjC class name.  Uses a simple hash to keep the name
-- short while avoiding collisions in practice.
selsFingerprint :: [String] -> String
selsFingerprint = show . foldl step 5381
  where
    step :: Int -> String -> Int
    step acc s = foldl (\h c -> h * 33 + fromEnum c) acc s
