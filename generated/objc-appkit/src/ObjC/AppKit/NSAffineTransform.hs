{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAffineTransform@.
module ObjC.AppKit.NSAffineTransform
  ( NSAffineTransform
  , IsNSAffineTransform(..)
  , transformBezierPath
  , set
  , concat_
  , transformBezierPathSelector
  , setSelector
  , concatSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- transformBezierPath:@
transformBezierPath :: (IsNSAffineTransform nsAffineTransform, IsNSBezierPath path) => nsAffineTransform -> path -> IO (Id NSBezierPath)
transformBezierPath nsAffineTransform  path =
withObjCPtr path $ \raw_path ->
    sendMsg nsAffineTransform (mkSelector "transformBezierPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @- set@
set :: IsNSAffineTransform nsAffineTransform => nsAffineTransform -> IO ()
set nsAffineTransform  =
  sendMsg nsAffineTransform (mkSelector "set") retVoid []

-- | @- concat@
concat_ :: IsNSAffineTransform nsAffineTransform => nsAffineTransform -> IO ()
concat_ nsAffineTransform  =
  sendMsg nsAffineTransform (mkSelector "concat") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @transformBezierPath:@
transformBezierPathSelector :: Selector
transformBezierPathSelector = mkSelector "transformBezierPath:"

-- | @Selector@ for @set@
setSelector :: Selector
setSelector = mkSelector "set"

-- | @Selector@ for @concat@
concatSelector :: Selector
concatSelector = mkSelector "concat"

