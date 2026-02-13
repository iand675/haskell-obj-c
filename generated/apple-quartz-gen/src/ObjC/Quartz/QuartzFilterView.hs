{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @QuartzFilterView@.
module ObjC.Quartz.QuartzFilterView
  ( QuartzFilterView
  , IsQuartzFilterView(..)
  , sizeToFit
  , sizeToFitSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Quartz.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- sizeToFit@
sizeToFit :: IsQuartzFilterView quartzFilterView => quartzFilterView -> IO ()
sizeToFit quartzFilterView =
  sendMessage quartzFilterView sizeToFitSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sizeToFit@
sizeToFitSelector :: Selector '[] ()
sizeToFitSelector = mkSelector "sizeToFit"

