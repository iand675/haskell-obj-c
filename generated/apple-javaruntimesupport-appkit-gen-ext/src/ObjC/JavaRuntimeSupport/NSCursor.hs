{-# LANGUAGE DataKinds #-}
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
  , javaMoveCursorSelector
  , javaResizeNECursorSelector
  , javaResizeNWCursorSelector
  , javaResizeSECursorSelector
  , javaResizeSWCursorSelector
  , javaSetAllowsCursorSetInBackgroundSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.JavaRuntimeSupport.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ javaBusyButClickableCursor@
javaBusyButClickableCursor :: IO (Id NSCursor)
javaBusyButClickableCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' javaBusyButClickableCursorSelector

-- | @+ javaResizeNECursor@
javaResizeNECursor :: IO (Id NSCursor)
javaResizeNECursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' javaResizeNECursorSelector

-- | @+ javaResizeNWCursor@
javaResizeNWCursor :: IO (Id NSCursor)
javaResizeNWCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' javaResizeNWCursorSelector

-- | @+ javaResizeSECursor@
javaResizeSECursor :: IO (Id NSCursor)
javaResizeSECursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' javaResizeSECursorSelector

-- | @+ javaResizeSWCursor@
javaResizeSWCursor :: IO (Id NSCursor)
javaResizeSWCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' javaResizeSWCursorSelector

-- | @+ javaMoveCursor@
javaMoveCursor :: IO (Id NSCursor)
javaMoveCursor  =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' javaMoveCursorSelector

-- | @+ javaSetAllowsCursorSetInBackground:@
javaSetAllowsCursorSetInBackground :: Bool -> IO ()
javaSetAllowsCursorSetInBackground allows =
  do
    cls' <- getRequiredClass "NSCursor"
    sendClassMessage cls' javaSetAllowsCursorSetInBackgroundSelector allows

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @javaBusyButClickableCursor@
javaBusyButClickableCursorSelector :: Selector '[] (Id NSCursor)
javaBusyButClickableCursorSelector = mkSelector "javaBusyButClickableCursor"

-- | @Selector@ for @javaResizeNECursor@
javaResizeNECursorSelector :: Selector '[] (Id NSCursor)
javaResizeNECursorSelector = mkSelector "javaResizeNECursor"

-- | @Selector@ for @javaResizeNWCursor@
javaResizeNWCursorSelector :: Selector '[] (Id NSCursor)
javaResizeNWCursorSelector = mkSelector "javaResizeNWCursor"

-- | @Selector@ for @javaResizeSECursor@
javaResizeSECursorSelector :: Selector '[] (Id NSCursor)
javaResizeSECursorSelector = mkSelector "javaResizeSECursor"

-- | @Selector@ for @javaResizeSWCursor@
javaResizeSWCursorSelector :: Selector '[] (Id NSCursor)
javaResizeSWCursorSelector = mkSelector "javaResizeSWCursor"

-- | @Selector@ for @javaMoveCursor@
javaMoveCursorSelector :: Selector '[] (Id NSCursor)
javaMoveCursorSelector = mkSelector "javaMoveCursor"

-- | @Selector@ for @javaSetAllowsCursorSetInBackground:@
javaSetAllowsCursorSetInBackgroundSelector :: Selector '[Bool] ()
javaSetAllowsCursorSetInBackgroundSelector = mkSelector "javaSetAllowsCursorSetInBackground:"

