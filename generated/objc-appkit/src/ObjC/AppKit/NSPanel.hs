{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPanel@.
module ObjC.AppKit.NSPanel
  ( NSPanel
  , IsNSPanel(..)
  , floatingPanel
  , setFloatingPanel
  , becomesKeyOnlyIfNeeded
  , setBecomesKeyOnlyIfNeeded
  , worksWhenModal
  , setWorksWhenModal
  , floatingPanelSelector
  , setFloatingPanelSelector
  , becomesKeyOnlyIfNeededSelector
  , setBecomesKeyOnlyIfNeededSelector
  , worksWhenModalSelector
  , setWorksWhenModalSelector


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

-- | @- floatingPanel@
floatingPanel :: IsNSPanel nsPanel => nsPanel -> IO Bool
floatingPanel nsPanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPanel (mkSelector "floatingPanel") retCULong []

-- | @- setFloatingPanel:@
setFloatingPanel :: IsNSPanel nsPanel => nsPanel -> Bool -> IO ()
setFloatingPanel nsPanel  value =
  sendMsg nsPanel (mkSelector "setFloatingPanel:") retVoid [argCULong (if value then 1 else 0)]

-- | @- becomesKeyOnlyIfNeeded@
becomesKeyOnlyIfNeeded :: IsNSPanel nsPanel => nsPanel -> IO Bool
becomesKeyOnlyIfNeeded nsPanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPanel (mkSelector "becomesKeyOnlyIfNeeded") retCULong []

-- | @- setBecomesKeyOnlyIfNeeded:@
setBecomesKeyOnlyIfNeeded :: IsNSPanel nsPanel => nsPanel -> Bool -> IO ()
setBecomesKeyOnlyIfNeeded nsPanel  value =
  sendMsg nsPanel (mkSelector "setBecomesKeyOnlyIfNeeded:") retVoid [argCULong (if value then 1 else 0)]

-- | @- worksWhenModal@
worksWhenModal :: IsNSPanel nsPanel => nsPanel -> IO Bool
worksWhenModal nsPanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPanel (mkSelector "worksWhenModal") retCULong []

-- | @- setWorksWhenModal:@
setWorksWhenModal :: IsNSPanel nsPanel => nsPanel -> Bool -> IO ()
setWorksWhenModal nsPanel  value =
  sendMsg nsPanel (mkSelector "setWorksWhenModal:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @floatingPanel@
floatingPanelSelector :: Selector
floatingPanelSelector = mkSelector "floatingPanel"

-- | @Selector@ for @setFloatingPanel:@
setFloatingPanelSelector :: Selector
setFloatingPanelSelector = mkSelector "setFloatingPanel:"

-- | @Selector@ for @becomesKeyOnlyIfNeeded@
becomesKeyOnlyIfNeededSelector :: Selector
becomesKeyOnlyIfNeededSelector = mkSelector "becomesKeyOnlyIfNeeded"

-- | @Selector@ for @setBecomesKeyOnlyIfNeeded:@
setBecomesKeyOnlyIfNeededSelector :: Selector
setBecomesKeyOnlyIfNeededSelector = mkSelector "setBecomesKeyOnlyIfNeeded:"

-- | @Selector@ for @worksWhenModal@
worksWhenModalSelector :: Selector
worksWhenModalSelector = mkSelector "worksWhenModal"

-- | @Selector@ for @setWorksWhenModal:@
setWorksWhenModalSelector :: Selector
setWorksWhenModalSelector = mkSelector "setWorksWhenModal:"

