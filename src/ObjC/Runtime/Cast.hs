{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Safe runtime-checked downcasting for phantom-typed ObjC pointers.
--
-- Upcasting (e.g., @Id NSMutableString@ -> @Id NSString@) is handled at
-- compile time via the @IsXxx@ type class hierarchy. Downcasting requires a
-- runtime check because the Haskell type system cannot prove that a given
-- @Id NSString@ is actually an @Id NSMutableString@.
--
-- This module provides 'safeCast', which uses Objective-C's
-- @isKindOfClass:@ message to perform the check, returning 'Nothing'
-- if the object is not an instance of (or a subclass of) the target class.
module ObjC.Runtime.Cast
  ( safeCast
  ) where

import Data.Proxy (Proxy(..))
import Foreign.Ptr (nullPtr)
import Foreign.LibFFI (retPtr, retVoid, argPtr)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg)
import ObjC.Runtime.Selector (mkSelector)

-- | Safe runtime-checked downcast using Objective-C's @isKindOfClass:@.
--
-- Returns 'Just' the object with a refined type if the object is an
-- instance of the target class (or any of its subclasses), or 'Nothing'
-- otherwise.
--
-- This is a zero-cost re-tag — no retain\/release is performed.
--
-- @
-- -- Attempt to downcast an Id NSString to Id NSMutableString:
-- result <- safeCast \@NSMutableString someString
-- case result of
--   Just mutable -> ...   -- it was mutable
--   Nothing      -> ...   -- it was not
-- @
safeCast :: forall b a. (IsObjCObject (Id a), IsObjCObject (Id b))
         => Id a -> IO (Maybe (Id b))
safeCast obj = do
  targetCls <- staticClass (Proxy @(Id b))
  -- Send [obj isKindOfClass:targetCls]
  -- BOOL is returned as a register-width value; we interpret it as a Ptr:
  -- 0x0 = NO (nullPtr), non-zero = YES.
  result <- sendMsg obj isKindOfClassSel (retPtr retVoid) [argPtr (unClass targetCls)]
  pure $ if result /= nullPtr
    then Just (unsafeCastId obj)
    else Nothing
  where
    isKindOfClassSel = mkSelector "isKindOfClass:"
