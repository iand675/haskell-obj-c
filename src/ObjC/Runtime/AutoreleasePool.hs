{-# LANGUAGE ForeignFunctionInterface #-}

-- | Autorelease pool management via the Objective-C runtime.
module ObjC.Runtime.AutoreleasePool
  ( objc_autoreleasePoolPush
  , objc_autoreleasePoolPop
  , withAutoreleasePool
  ) where

import Foreign.Ptr (Ptr)
import Control.Exception (bracket)
-- ---------------------------------------------------------------------------
-- Raw FFI
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "objc_autoreleasePoolPush"
  c_objc_autoreleasePoolPush :: IO (Ptr ())

foreign import ccall unsafe "objc_autoreleasePoolPop"
  c_objc_autoreleasePoolPop :: Ptr () -> IO ()

-- ---------------------------------------------------------------------------
-- Haskell wrappers
-- ---------------------------------------------------------------------------

-- | Push a new autorelease pool. Returns a token to pass to 'objc_autoreleasePoolPop'.
objc_autoreleasePoolPush :: IO (Ptr ())
objc_autoreleasePoolPush = c_objc_autoreleasePoolPush

-- | Pop an autorelease pool (releasing all autoreleased objects since the
-- corresponding push).
objc_autoreleasePoolPop :: Ptr () -> IO ()
objc_autoreleasePoolPop = c_objc_autoreleasePoolPop

-- | Execute an action inside an autorelease pool. The pool is drained
-- after the action completes (or on exception).
withAutoreleasePool :: IO a -> IO a
withAutoreleasePool action = bracket
  c_objc_autoreleasePoolPush
  c_objc_autoreleasePoolPop
  (\_ -> action)