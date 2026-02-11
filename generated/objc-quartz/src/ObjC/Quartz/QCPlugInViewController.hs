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

import ObjC.Quartz.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPlugIn:viewNibName:@
initWithPlugIn_viewNibName :: (IsQCPlugInViewController qcPlugInViewController, IsQCPlugIn plugIn, IsNSString name) => qcPlugInViewController -> plugIn -> name -> IO RawId
initWithPlugIn_viewNibName qcPlugInViewController  plugIn name =
withObjCPtr plugIn $ \raw_plugIn ->
  withObjCPtr name $ \raw_name ->
      fmap (RawId . castPtr) $ sendMsg qcPlugInViewController (mkSelector "initWithPlugIn:viewNibName:") (retPtr retVoid) [argPtr (castPtr raw_plugIn :: Ptr ()), argPtr (castPtr raw_name :: Ptr ())]

-- | @- plugIn@
plugIn :: IsQCPlugInViewController qcPlugInViewController => qcPlugInViewController -> IO (Id QCPlugIn)
plugIn qcPlugInViewController  =
  sendMsg qcPlugInViewController (mkSelector "plugIn") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPlugIn:viewNibName:@
initWithPlugIn_viewNibNameSelector :: Selector
initWithPlugIn_viewNibNameSelector = mkSelector "initWithPlugIn:viewNibName:"

-- | @Selector@ for @plugIn@
plugInSelector :: Selector
plugInSelector = mkSelector "plugIn"

