{-# LANGUAGE DataKinds #-}
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
  , concatSelector
  , setSelector
  , transformBezierPathSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- transformBezierPath:@
transformBezierPath :: (IsNSAffineTransform nsAffineTransform, IsNSBezierPath path) => nsAffineTransform -> path -> IO (Id NSBezierPath)
transformBezierPath nsAffineTransform path =
  sendMessage nsAffineTransform transformBezierPathSelector (toNSBezierPath path)

-- | @- set@
set :: IsNSAffineTransform nsAffineTransform => nsAffineTransform -> IO ()
set nsAffineTransform =
  sendMessage nsAffineTransform setSelector

-- | @- concat@
concat_ :: IsNSAffineTransform nsAffineTransform => nsAffineTransform -> IO ()
concat_ nsAffineTransform =
  sendMessage nsAffineTransform concatSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @transformBezierPath:@
transformBezierPathSelector :: Selector '[Id NSBezierPath] (Id NSBezierPath)
transformBezierPathSelector = mkSelector "transformBezierPath:"

-- | @Selector@ for @set@
setSelector :: Selector '[] ()
setSelector = mkSelector "set"

-- | @Selector@ for @concat@
concatSelector :: Selector '[] ()
concatSelector = mkSelector "concat"

