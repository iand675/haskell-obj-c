{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCursor@.
module ObjC.JavaRuntimeSupport.NSCursor
  ( NSCursor
  , IsNSCursor(..)
  , javaBusyButClickableCursor
  , javaResizeNECursor
  , javaResizeNWCursor
  , javaResizeSECursor
  , javaResizeSWCursor
  , javaMoveCursor
  , javaSetAllowsCursorSetInBackground
  , javaBusyButClickableCursorSelector
  , javaResizeNECursorSelector
  , javaResizeNWCursorSelector
  , javaResizeSECursorSelector
  , javaResizeSWCursorSelector
  , javaMoveCursorSelector
  , javaSetAllowsCursorSetInBackgroundSelector


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

import ObjC.JavaRuntimeSupport.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ javaBusyButClickableCursor@
javaBusyButClickableCursor :: IO RawId
javaBusyButClickableCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "javaBusyButClickableCursor") (retPtr retVoid) []

-- | @+ javaResizeNECursor@
javaResizeNECursor :: IO RawId
javaResizeNECursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "javaResizeNECursor") (retPtr retVoid) []

-- | @+ javaResizeNWCursor@
javaResizeNWCursor :: IO RawId
javaResizeNWCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "javaResizeNWCursor") (retPtr retVoid) []

-- | @+ javaResizeSECursor@
javaResizeSECursor :: IO RawId
javaResizeSECursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "javaResizeSECursor") (retPtr retVoid) []

-- | @+ javaResizeSWCursor@
javaResizeSWCursor :: IO RawId
javaResizeSWCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "javaResizeSWCursor") (retPtr retVoid) []

-- | @+ javaMoveCursor@
javaMoveCursor :: IO RawId
javaMoveCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "javaMoveCursor") (retPtr retVoid) []

-- | @+ javaSetAllowsCursorSetInBackground:@
javaSetAllowsCursorSetInBackground :: Bool -> IO ()
javaSetAllowsCursorSetInBackground allows =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMsg cls' (mkSelector "javaSetAllowsCursorSetInBackground:") retVoid [argCULong (if allows then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @javaBusyButClickableCursor@
javaBusyButClickableCursorSelector :: Selector
javaBusyButClickableCursorSelector = mkSelector "javaBusyButClickableCursor"

-- | @Selector@ for @javaResizeNECursor@
javaResizeNECursorSelector :: Selector
javaResizeNECursorSelector = mkSelector "javaResizeNECursor"

-- | @Selector@ for @javaResizeNWCursor@
javaResizeNWCursorSelector :: Selector
javaResizeNWCursorSelector = mkSelector "javaResizeNWCursor"

-- | @Selector@ for @javaResizeSECursor@
javaResizeSECursorSelector :: Selector
javaResizeSECursorSelector = mkSelector "javaResizeSECursor"

-- | @Selector@ for @javaResizeSWCursor@
javaResizeSWCursorSelector :: Selector
javaResizeSWCursorSelector = mkSelector "javaResizeSWCursor"

-- | @Selector@ for @javaMoveCursor@
javaMoveCursorSelector :: Selector
javaMoveCursorSelector = mkSelector "javaMoveCursor"

-- | @Selector@ for @javaSetAllowsCursorSetInBackground:@
javaSetAllowsCursorSetInBackgroundSelector :: Selector
javaSetAllowsCursorSetInBackgroundSelector = mkSelector "javaSetAllowsCursorSetInBackground:"

