{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Create ObjC objects that respond to target\/action selectors,
-- backed by Haskell closures.  No Template Haskell required.
--
-- Every Cocoa target\/action method has the same C signature:
-- @-(void)action:(id)sender@ (type encoding @v\@:\@@).  This means a
-- single @foreign import ccall \"wrapper\"@ covers all action methods.
--
-- The 'ActionHandler' GADT pairs a typed 'Selector' with a handler
-- whose argument types are driven by the selector's phantom parameters.
-- The '(:=)' constructor is existentially quantified so handlers of
-- different types can appear in the same list.
--
-- = Usage
--
-- @
-- import ObjC.Runtime
-- import ObjC.Runtime.ActionTarget
--
-- -- Action that ignores the sender (Sel = Selector '[] ())
-- addItemSel :: Sel
-- addItemSel = mkSelector \"addItem:\"
--
-- -- Action that receives a typed sender
-- toggleSel :: Selector '[Id NSView] ()
-- toggleSel = mkSelector \"toggle:\"
--
-- target <- newActionTarget
--   [ addItemSel := do
--       putStrLn \"Add item!\"
--   , toggleSel := \\sender -> do
--       rowIdx <- TV.rowForView tableView sender
--       ...
--   ]
-- Ctrl.setTarget button target
-- Ctrl.setAction button (asSel addItemSel)
-- @
module ObjC.Runtime.ActionTarget
  ( -- * Action handler type
    ActionHandler(..)
  , DecodeActionArg(..)
  , InvokeAction(..)

    -- * Creating action targets
  , newActionTarget
  ) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Control.Monad (forM)
import Data.Kind (Type)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.String (withCString)
import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import System.IO.Unsafe (unsafePerformIO)

import Data.List (sort)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS

import VarArgs (type (:->:))

import ObjC.Runtime.Types
import ObjC.Runtime.Selector (selName)
import ObjC.Runtime.Class (getRequiredClass, class_createInstance)
import ObjC.Runtime.ClassBuilder (objc_allocateClassPair, objc_registerClassPair)
import ObjC.Runtime.StableIvar
  (addHsDataIvar, writeHsData, readHsData, addStablePtrDeallocHandler,
   addObjCMethod)

-- ---------------------------------------------------------------------------
-- DecodeActionArg — per-argument decoding from a raw ObjC pointer
-- ---------------------------------------------------------------------------

-- | Decode a single Objective-C argument from a raw pointer.
--
-- Instances control how the raw @id@ pointer delivered by the ObjC runtime
-- is presented to the Haskell handler.
class DecodeActionArg a where
  decodeActionArg :: Ptr ObjCObject -> IO a

-- | Pass through as an unmanaged 'RawId'.
instance DecodeActionArg RawId where
  decodeActionArg ptr = pure (RawId ptr)

-- | Wrap as a managed @'Id' a@ via 'retainedObject'.
-- The extra retain is balanced by the 'ForeignPtr' release finalizer.
instance DecodeActionArg (Id a) where
  decodeActionArg ptr = retainedObject ptr

-- ---------------------------------------------------------------------------
-- InvokeAction — bridge typed handler to uniform [Ptr ObjCObject] -> IO ()
-- ---------------------------------------------------------------------------

-- | Convert a typed handler (whose shape is driven by the selector's
-- phantom type-level argument list) into the uniform representation
-- used internally by the action target infrastructure.
--
-- The @[Ptr ObjCObject]@ list contains the raw arguments delivered by
-- the ObjC IMP stub (currently just the sender for the standard
-- target\/action pattern @v\@:\@@).
class InvokeAction (args :: [Type]) where
  invokeAction :: (args :->: IO ()) -> [Ptr ObjCObject] -> IO ()

-- | No arguments — handler is @IO ()@, sender ignored.
instance InvokeAction '[] where
  invokeAction action _ = action

-- | Single argument — decode the list head and apply.
instance DecodeActionArg a => InvokeAction '[a] where
  invokeAction action (ptr:_) = decodeActionArg ptr >>= action
  invokeAction _      _       = pure ()

-- | Two arguments — for future multi-argument callbacks
-- (e.g., UIControl @action:forEvent:@ on iOS).
instance (DecodeActionArg a, DecodeActionArg b) => InvokeAction '[a, b] where
  invokeAction action (p1:p2:_) = do
    a <- decodeActionArg p1
    b <- decodeActionArg p2
    action a b
  invokeAction _ _ = pure ()

-- ---------------------------------------------------------------------------
-- ActionHandler GADT
-- ---------------------------------------------------------------------------

-- | An existentially quantified action handler that pairs a typed
-- 'Selector' with a handler function whose arguments match the
-- selector's phantom type parameters.
--
-- Use the '(:=)' constructor as an infix operator:
--
-- @
-- mySel := \\sender -> putStrLn \"clicked\"
-- @
data ActionHandler where
  (:=) :: InvokeAction args
       => Selector args () -> (args :->: IO ()) -> ActionHandler

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
-- Each 'ActionHandler' pairs a typed selector with a matching Haskell
-- handler.  The returned 'RawId' can be set as a control's target.
--
-- Multiple calls with the same set of selectors (regardless of order)
-- share a single ObjC class registration.
newActionTarget :: [ActionHandler] -> IO RawId
newActionTarget handlers = do
  -- Convert typed handlers to uniform (name, callback) pairs.
  actions <- forM handlers $ \ah -> do
    let (name, cb) = unpackHandler ah
    n <- name
    pure (n, cb)
  let selSet = HS.fromList (fmap fst actions)
  cls <- getOrCreateClass selSet
  inst <- class_createInstance cls 0
  let handlerMap = HM.fromList actions
  sp <- newStablePtr handlerMap
  writeHsData inst sp
  pure inst

-- | Unpack an 'ActionHandler', resolving the existential type variable
-- via 'TypeApplications' so that 'invokeAction' can be selected.
unpackHandler :: ActionHandler -> (IO String, [Ptr ObjCObject] -> IO ())
unpackHandler ((:=) (sel :: Selector args ()) handler) =
  (selName sel, invokeAction @args handler)

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
registerActionStub cls selName' = do
  stub <- wrapActionIMP $ \self _cmd sender -> do
    sp <- readHsData self
    handlerMap <- deRefStablePtr sp :: IO (HashMap String ([Ptr ObjCObject] -> IO ()))
    case HM.lookup selName' handlerMap of
      Just handler -> handler [sender]
      Nothing      -> pure ()
  addObjCMethod cls selName' "v@:@" stub

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
