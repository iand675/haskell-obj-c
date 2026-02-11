{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AMBundleAction@.
module ObjC.Automator.AMBundleAction
  ( AMBundleAction
  , IsAMBundleAction(..)
  , awakeFromBundle
  , hasView
  , view
  , bundle
  , parameters
  , setParameters
  , awakeFromBundleSelector
  , hasViewSelector
  , viewSelector
  , bundleSelector
  , parametersSelector
  , setParametersSelector


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

import ObjC.Automator.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- awakeFromBundle@
awakeFromBundle :: IsAMBundleAction amBundleAction => amBundleAction -> IO ()
awakeFromBundle amBundleAction  =
  sendMsg amBundleAction (mkSelector "awakeFromBundle") retVoid []

-- | @- hasView@
hasView :: IsAMBundleAction amBundleAction => amBundleAction -> IO Bool
hasView amBundleAction  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg amBundleAction (mkSelector "hasView") retCULong []

-- | @- view@
view :: IsAMBundleAction amBundleAction => amBundleAction -> IO (Id NSView)
view amBundleAction  =
  sendMsg amBundleAction (mkSelector "view") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- bundle@
bundle :: IsAMBundleAction amBundleAction => amBundleAction -> IO (Id NSBundle)
bundle amBundleAction  =
  sendMsg amBundleAction (mkSelector "bundle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- parameters@
parameters :: IsAMBundleAction amBundleAction => amBundleAction -> IO (Id NSMutableDictionary)
parameters amBundleAction  =
  sendMsg amBundleAction (mkSelector "parameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setParameters:@
setParameters :: (IsAMBundleAction amBundleAction, IsNSMutableDictionary value) => amBundleAction -> value -> IO ()
setParameters amBundleAction  value =
withObjCPtr value $ \raw_value ->
    sendMsg amBundleAction (mkSelector "setParameters:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @awakeFromBundle@
awakeFromBundleSelector :: Selector
awakeFromBundleSelector = mkSelector "awakeFromBundle"

-- | @Selector@ for @hasView@
hasViewSelector :: Selector
hasViewSelector = mkSelector "hasView"

-- | @Selector@ for @view@
viewSelector :: Selector
viewSelector = mkSelector "view"

-- | @Selector@ for @bundle@
bundleSelector :: Selector
bundleSelector = mkSelector "bundle"

-- | @Selector@ for @parameters@
parametersSelector :: Selector
parametersSelector = mkSelector "parameters"

-- | @Selector@ for @setParameters:@
setParametersSelector :: Selector
setParametersSelector = mkSelector "setParameters:"

