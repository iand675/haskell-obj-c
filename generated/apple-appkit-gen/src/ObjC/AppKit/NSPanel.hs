{-# LANGUAGE DataKinds #-}
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
  , becomesKeyOnlyIfNeededSelector
  , floatingPanelSelector
  , setBecomesKeyOnlyIfNeededSelector
  , setFloatingPanelSelector
  , setWorksWhenModalSelector
  , worksWhenModalSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- floatingPanel@
floatingPanel :: IsNSPanel nsPanel => nsPanel -> IO Bool
floatingPanel nsPanel =
  sendMessage nsPanel floatingPanelSelector

-- | @- setFloatingPanel:@
setFloatingPanel :: IsNSPanel nsPanel => nsPanel -> Bool -> IO ()
setFloatingPanel nsPanel value =
  sendMessage nsPanel setFloatingPanelSelector value

-- | @- becomesKeyOnlyIfNeeded@
becomesKeyOnlyIfNeeded :: IsNSPanel nsPanel => nsPanel -> IO Bool
becomesKeyOnlyIfNeeded nsPanel =
  sendMessage nsPanel becomesKeyOnlyIfNeededSelector

-- | @- setBecomesKeyOnlyIfNeeded:@
setBecomesKeyOnlyIfNeeded :: IsNSPanel nsPanel => nsPanel -> Bool -> IO ()
setBecomesKeyOnlyIfNeeded nsPanel value =
  sendMessage nsPanel setBecomesKeyOnlyIfNeededSelector value

-- | @- worksWhenModal@
worksWhenModal :: IsNSPanel nsPanel => nsPanel -> IO Bool
worksWhenModal nsPanel =
  sendMessage nsPanel worksWhenModalSelector

-- | @- setWorksWhenModal:@
setWorksWhenModal :: IsNSPanel nsPanel => nsPanel -> Bool -> IO ()
setWorksWhenModal nsPanel value =
  sendMessage nsPanel setWorksWhenModalSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @floatingPanel@
floatingPanelSelector :: Selector '[] Bool
floatingPanelSelector = mkSelector "floatingPanel"

-- | @Selector@ for @setFloatingPanel:@
setFloatingPanelSelector :: Selector '[Bool] ()
setFloatingPanelSelector = mkSelector "setFloatingPanel:"

-- | @Selector@ for @becomesKeyOnlyIfNeeded@
becomesKeyOnlyIfNeededSelector :: Selector '[] Bool
becomesKeyOnlyIfNeededSelector = mkSelector "becomesKeyOnlyIfNeeded"

-- | @Selector@ for @setBecomesKeyOnlyIfNeeded:@
setBecomesKeyOnlyIfNeededSelector :: Selector '[Bool] ()
setBecomesKeyOnlyIfNeededSelector = mkSelector "setBecomesKeyOnlyIfNeeded:"

-- | @Selector@ for @worksWhenModal@
worksWhenModalSelector :: Selector '[] Bool
worksWhenModalSelector = mkSelector "worksWhenModal"

-- | @Selector@ for @setWorksWhenModal:@
setWorksWhenModalSelector :: Selector '[Bool] ()
setWorksWhenModalSelector = mkSelector "setWorksWhenModal:"

