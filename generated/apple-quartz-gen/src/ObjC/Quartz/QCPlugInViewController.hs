{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @QCPlugInViewController@.
module ObjC.Quartz.QCPlugInViewController
  ( QCPlugInViewController
  , IsQCPlugInViewController(..)
  , initWithPlugIn_viewNibName
  , plugIn
  , initWithPlugIn_viewNibNameSelector
  , plugInSelector


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

-- | @- initWithPlugIn:viewNibName:@
initWithPlugIn_viewNibName :: (IsQCPlugInViewController qcPlugInViewController, IsQCPlugIn plugIn, IsNSString name) => qcPlugInViewController -> plugIn -> name -> IO RawId
initWithPlugIn_viewNibName qcPlugInViewController plugIn name =
  sendOwnedMessage qcPlugInViewController initWithPlugIn_viewNibNameSelector (toQCPlugIn plugIn) (toNSString name)

-- | @- plugIn@
plugIn :: IsQCPlugInViewController qcPlugInViewController => qcPlugInViewController -> IO (Id QCPlugIn)
plugIn qcPlugInViewController =
  sendMessage qcPlugInViewController plugInSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPlugIn:viewNibName:@
initWithPlugIn_viewNibNameSelector :: Selector '[Id QCPlugIn, Id NSString] RawId
initWithPlugIn_viewNibNameSelector = mkSelector "initWithPlugIn:viewNibName:"

-- | @Selector@ for @plugIn@
plugInSelector :: Selector '[] (Id QCPlugIn)
plugInSelector = mkSelector "plugIn"

